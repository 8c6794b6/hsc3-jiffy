{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

-- base
import Control.Monad (foldM)
import Data.Foldable (foldr')

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

-- criterion
import Criterion.Main (Benchmark, defaultMain, bench, bgroup, nf)

-- hsc3
import Sound.SC3 (Rate(..), BinaryOp(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)

import qualified Sound.SC3 as S
import qualified Sound.SC3.UGen.Graph as SUG
import qualified Sound.SC3.Server.Graphdef as SSG

-- vivid
import qualified Vivid as V
import qualified Vivid.SynthDef.FromUA as FromUA

-- Internal
import Sound.SC3.Jiffy.Encode
import Sound.SC3.Jiffy.UGen.Builder
import Sound.SC3.Jiffy.UGen.DB

h :: S.UGen -> ByteString
h = toStrict . SSG.encode_graphdef .
    graph_to_graphdef "b" . SUG.ugen_to_graph
{-# INLINE h #-}

j :: UGen -> ByteString
j = toStrict . encode_graphdef . ugen_to_graphdef "b"
{-# INLINE j #-}

v :: V.SynthDef a -> ByteString
v = V.encodeSD
{-# INLINE v #-}

g01h :: S.UGen -> S.UGen
g01h freq = S.out 0 (S.sinOsc AR freq 0)

g01j :: UGen -> UGen
g01j freq = out 0 (sinOsc AR freq 0)

g01v :: Double -> V.SynthDef '[]
g01v freq =
  V.sd
    ()
    (do s <- V.sinOsc (V.freq_ freq) V.? V.AR
        V.out (0 :: Int) [s])

bench_g01 :: Benchmark
bench_g01 =
  bgroup "g01"
         [bench "hsc3" (nf (h . g01h) 440)
         ,bench "jiffy" (nf (j . g01j) 440)
         ,bench "vivid" (nf (v . g01v) 440)]

g03h :: Int -> S.UGen
g03h n =
  let freq = S.control KR "freq" 440
      o0 = S.sinOsc AR freq 0
      f b a = S.sinOsc AR (fromIntegral (a * 110)) b
      o = foldl f o0 [1..n]
  in  S.out 0 o

g03j :: Int -> UGen
g03j n =
  let freq = control KR "freq" 440
      o0 = sinOsc AR freq 0
      f a b = sinOsc AR (fromIntegral (a * 110)) b
      o = foldr f o0 [1..n]
  in  out 0 o

g03v :: Int -> V.SynthDef '["freq"]
g03v n =
  V.sd
    (440 :: V.I "freq")
    (do o0 <- V.sinOsc (V.freq_ (V.V :: V.V "freq")) V.? V.AR
        let f b a = V.sinOsc (V.freq_ (a * 110), V.phase_ b)
        o <- foldM f o0 [1..n]
        V.out (0::Int) [o])

bench_g03 :: Benchmark
bench_g03 =
  bgroup "g03"
         [bgroup  "64" [bench "hsc3" (nf (h . g03h) 64)
                       ,bench "jiffy" (nf (j . g03j) 64)
                       ,bench "vivid" (nf (v . g03v) 64)]
         ,bgroup "256" [bench "hsc3" (nf (h . g03h) 256)
                       ,bench "jiffy" (nf (j . g03j) 256)
                       ,bench "vivid" (nf (v . g03v) 256)]]

g07h :: Int -> S.UGen
g07h n =
  let d = S.dust 't' KR 2
      o0 = S.sinOsc AR (S.mce [440,441]) 0 * S.decay d 1
      f a b =
        let cg = S.coinGate a 0.012 d
            dt = S.lag (S.tExpRand a 1e-5 0.4 cg) 1.8
        in  clip2 (b + S.hpf (S.delayN b 0.4 dt * 0.8) 20) 1
      o1 = foldr f o0 [0 .. n]
  in  S.out 0 (o1 * 0.5)

g07j :: Int -> UGen
g07j n = do
  d <- share (dust KR 2)
  let o0 = sinOsc AR (mce [440,441]) 0 * decay d 1
      f _ b = do
        let cg = coinGate 0.012 d
        dt <- share (lag (tExpRand 1e-5 0.4 cg) 1.8)
        b' <- share b
        clip2 (b' + hpf (delayN b' 0.4 dt * 0.8) 20) 1
      o1 = foldr' f o0 [0 .. n]
  out 0 (o1 * 0.5)

-- Auxiliary functions for vivid-synth

-- Note that, as of vivid 0.3.0.2, vivid synth caches synthdef sent to
-- scsynth server. Internally, it stores a
-- "Vivid.SCServer.State.SCServerState" data type instance with with
-- "unsafePerformIO" to "Vivid.SCServer.State.scServerState".
--
-- hsc3 uses lazy bytestring, vivid uses strict bytestring for encoded
-- synthdef.

prob_ :: V.ToSig s as => s -> FromUA.UA "prob" as
prob_ = FromUA.UA . V.toSig

v_coinGate :: V.Args '["prob", "in"] '[] a => a -> V.SDBody a V.Signal
v_coinGate =
  V.makeUGen "CoinGate" V.AR (V.Vs :: V.Vs '["prob", "in"])
             FromUA.NoDefaults

v_tExpRand :: V.Args '["lo", "hi", "in"] '[] a => a -> V.SDBody a V.Signal
v_tExpRand =
  V.makeUGen "TExpRand" V.AR (V.Vs :: V.Vs '["lo", "hi", "in"])
             FromUA.NoDefaults

-- For testing v_coinGate and v_tExpRand.
--
-- v01 :: V.SynthDef '[]
-- v01 = V.sd () $ do
--   t0 <- V.impulse (V.freq_ (dbl 4))
--   t1 <- v_coinGate (V.in_ t0, prob_ (dbl 0.5))
--   e <- V.decay (V.in_ t1, V.decaySecs_ (dbl 1))
--   o <- V.sinOsc (V.freq_ (dbl 440)) V.~* e
--   V.out (dbl 0) [o,o]

dbl :: Double -> Double
dbl = id

-- Seems like, not the same graph made with hsc3. Vivid-synth does not
-- support multi channel expansion ... ?
g07v :: Int -> V.SynthDef '[]
g07v n =
  V.sd
    ()
    (do d <- V.dust (V.density_ (2::Int))
        e <- V.decay (V.in_ d, V.decaySecs_ (1::Int))
        let mko freq = V.sinOsc (V.freq_ (dbl freq)) V.? V.AR V.~* e
            f (b0, c0) _ = do
              cg <- v_coinGate (V.in_ d, prob_ (dbl 0.012))
              dt0 <- v_tExpRand ( V.in_ cg
                                , V.lo_ (dbl 1e-5)
                                , V.hi_ (dbl 0.4))
              dt1 <- V.lag (V.in_ dt0, V.lagSecs_ (dbl 1.8))
              let g x0 = do
                    x1 <- V.delayN ( V.in_ x0
                                   , V.maxDelaySecs_ (dbl 0.4)
                                   , V.delaySecs_ dt1 )
                          V.~* dbl 0.8
                    x2 <- V.hpf (V.in_ x1, V.freq_ (dbl 20)) V.~+ x0
                    V.clip2 x2 (dbl 1)
              b3 <- g b0
              c3 <- g c0
              return (b3, c3)
        o0 <- mko 440
        o1 <- mko 441
        (o2, o3) <- foldM f (o0,o1) [1 .. n]
        V.out (0::Int) [o2 V.~* dbl 0.5, o3 V.~* dbl 0.53])

bench_g07 :: Benchmark
bench_g07 =
  bgroup "g07"
         [bgroup "4" [bench "hsc3" (nf (h . g07h) 4)
                     ,bench "jiffy" (nf (j . g07j) 4)
                     ,bench "vivid" (nf (v . g07v) 4)]
         ,bgroup "5" [bench "hsc3" (nf (h . g07h) 5)
                     ,bench "jiffy" (nf (j . g07j) 5)
                     ,bench "vivid" (nf (v . g07v) 5)]]

main :: IO ()
main = defaultMain
  [ bench_g01
  , bench_g03
  , bench_g07
  ]

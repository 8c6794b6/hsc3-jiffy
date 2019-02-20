module Main where

-- base
import Data.Foldable (foldr')

-- criterion
import Criterion.Main

-- hsc3
import Sound.SC3 (Rate(..), BinaryOp(..))
import Sound.SC3.UGen.Graph (U_Graph(..), ugen_to_graph)
import qualified Sound.SC3 as S

-- Internal
import Sound.SC3.Jiffy.UGen.Builder
  (UGen, gnode_to_graph, control, mce, share)
import Sound.SC3.Jiffy.UGen.DB

h :: S.UGen -> Int
h x = ug_next_id (ugen_to_graph x)
{-# INLINE h #-}

j :: UGen -> Int
j x = ug_next_id (gnode_to_graph x)
{-# INLINE j #-}

g01h :: S.UGen -> S.UGen
g01h freq = S.out 0 (S.sinOsc AR freq 0)

g01j :: UGen -> UGen
g01j freq = out 0 (sinOsc AR freq 0)

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

main :: IO ()
main = defaultMain
  [ bgroup "g01"
           [bench "hsc3" (whnf (h . g01h) 440)
           ,bench "jiffy" (whnf (j . g01j) 440)]
  , bgroup "g03"
           [bgroup  "64" [bench "hsc3" (whnf (h . g03h) 64)
                         ,bench "jiffy" (whnf (j . g03j) 64)]
           ,bgroup "256" [bench "hsc3" (whnf (h . g03h) 256)
                         ,bench "jiffy" (whnf (j . g03j) 256)]]
  , bgroup "g07"
           [bgroup "4" [bench "hsc3" (whnf (h . g07h) 4)
                       ,bench "jiffy" (whnf (j . g07j) 4)]
           ,bgroup "5" [bench "hsc3" (whnf (h . g07h) 5)
                       ,bench "jiffy" (whnf (j . g07j) 5)]]
  ]

module Main where

import Data.Foldable (foldr')

-- deepseq
import Control.DeepSeq

-- hsc3
import Sound.SC3 (Rate(..), BinaryOp(..))

-- Internal
import Sound.SC3.Jiffy.Encode (encode_graphdef)
import Sound.SC3.Jiffy.UGen.Builder (UGen, ugen_to_graphdef, mce, share)
import Sound.SC3.Jiffy.UGen.DB

g07 :: Int -> UGen
g07 n = do
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
main =
  let gd = ugen_to_graphdef "g07" (g07 4096)
  in  print (rnf (encode_graphdef gd))

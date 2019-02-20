module Main where

import Data.Foldable (foldr')

import Sound.SC3 (Rate(..), BinaryOp(..))
import Sound.SC3.UGen.Graph (U_Graph(..))
import Sound.SC3.Jiffy.UGen.Builder (UGen, gnode_to_graph, mce, share)
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
main = print (ug_next_id (gnode_to_graph (g07 4096)))

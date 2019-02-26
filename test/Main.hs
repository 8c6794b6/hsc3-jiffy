module Main where

import Test.Hspec

-- hsc3
import Sound.SC3 (Rate(..), Loop(..), DoneAction(..), Warp(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import qualified Sound.SC3 as S
import qualified Sound.SC3.UGen.Graph as SG
import qualified Sound.SC3.Server.Graphdef as SSG

-- Internal
import Sound.SC3.Jiffy.Encode (encode_graphdef)
import Sound.SC3.UGen.Jiffy

same_graphdef :: String -> UGen -> S.UGen -> Spec
same_graphdef name j h =
  let jg = encode_graphdef (ugen_to_graphdef "tmp" j)
      hg = SSG.encode_graphdef
             (graph_to_graphdef "tmp" (SG.ugen_to_graph h))
  in  describe name (it "should be identical" (jg `shouldBe` hg))

simple_graph :: Spec
simple_graph =
  let j = out 0 (sinOsc AR (hpf (whiteNoise AR) 0) 0)
      h = S.out 0 (S.sinOsc AR (S.hpf (S.whiteNoise 'a' AR) 0) 0)
  in  same_graphdef "simple" j h

mix_mce_graph :: Spec
mix_mce_graph =
  let j = let f1 = control KR "f1" 0
          in out 0 (pan2 (mix (sinOsc AR (mce [f1,0]) 0)) 0 0)
      h = let f1 = S.control KR "f1" 0
          in  S.out 0 (S.pan2 (S.mix (S.sinOsc AR (S.mce [f1,0]) 0)) 0 0)
  in  same_graphdef "mix_mce" j h

nondet_graph :: Spec
nondet_graph =
  let j = do w1 <- share (whiteNoise AR)
             let w2 = whiteNoise AR
             out 0 (mce [w1-w1, w2-w2])
      h = let w1 = S.whiteNoise 'a' AR
              wx x = S.whiteNoise x AR
          in  S.out 0 (S.mce [w1-w1, wx 'b' - wx 'c'])
  in  same_graphdef "nondet" j h

mrg_graph :: Spec
mrg_graph =
  -- Using 'share' to control the order of constant values, storing 0
  -- before 1.
  let j = do s1 <- share (sinOsc AR 0 0)
             _ <- out 1 s1
             out 0 s1
      h = let s1 = S.sinOsc AR 0 0
          in  S.mrg [S.out 0 s1, S.out 1 s1]
  in  same_graphdef "mrg" j h

enum_graph :: Spec
enum_graph = do
  let j0 = playBuf 2 AR 0 0 0 0 NoLoop DoNothing
      h0 = S.playBuf 2 AR 0 0 0 0 NoLoop DoNothing
      j1 = mouseX KR 0 0 Linear 0
      h1 = S.mouseX KR 0 0 Linear 0
  same_graphdef "enum_loop_doneaction" j0 h0
  same_graphdef "enum_warp" j1 h1

controls_graph :: Spec
controls_graph = do
  let j0 = let c1 = tr_control "t_amp" 0
               c2 = control KR "freq" 0
               c3 = control AR "amp" 0
               c4 = control IR "dur" 0
               e = decay c1 c4
               s = sinOsc AR c2 0 * e * c3
           in  out 0 (mce [s,s])
      h0 = let c1 = S.tr_control "t_amp" 0
               c2 = S.control KR "freq" 0
               c3 = S.control AR "amp" 0
               c4 = S.control IR "dur" 0
               e = S.decay c1 c4
               s = S.sinOsc AR c2 0 * e * c3
           in  S.out 0 (S.mce [s,s])
  same_graphdef "controls_with_various_rates" j0 h0

unary_op_graph :: Spec
unary_op_graph =
  let j0 = saw AR (S.midiCPS 67)
      h0 = S.saw AR (S.midiCPS 67)
  in  same_graphdef "constant_folding_with_unary_op" j0 h0

main :: IO ()
main =
  hspec
    (describe
       "comparison"
        (do simple_graph
            mix_mce_graph
            nondet_graph
            mrg_graph
            enum_graph
            controls_graph
            unary_op_graph))

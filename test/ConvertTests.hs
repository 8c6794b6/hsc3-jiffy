-- | Module containing tests for "Sound.SC3.UGen.Jiffy.Builder.Convert".
module ConvertTests
  ( convertTests
  ) where

-- hspec
import Test.Hspec

-- hsc3
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)

-- hsc3-jiffy
import Sound.SC3.Jiffy

whysc :: UGen
whysc = whysc_gen 10 7 4

whysc_gen :: Int -> Int -> Int -> UGen
whysc_gen i j k = do
  let r = resonz (dust AR 0.2 * 50) (200 + (rand 0 3000)) 0.003
  s <- share (mix (dup i r))
  let z = delayN s 0.048 0.048
      t = lfNoise1 KR (rand 0 0.1) * 0.04 + 0.05
      c = combL z 0.1 t 15
  y <- share (mix (dup j c))
  let f b = allpassN b 0.05 (mce2 (rand 0 0.05) (rand 0 0.05)) 1
      x = iterate f y !! k
  out 0 (s + 0.2 * x)

compare_graphdefs :: String -> UGen -> Spec
compare_graphdefs label ug =
  describe label $
    let g1 = ugen_to_graphdef "g" ug
        g2 = graph_to_graphdef "g" (ugen_to_graph ug)
    in  it "should_be_identcal" (g1 `shouldBe` g2)

simple_test :: Spec
simple_test = compare_graphdefs "simple_graph" (out 0 (sinOsc AR 1 0))

control_graph :: UGen
control_graph =
  let p0 = control KR "p0" 440
      p1 = control IR "p1" 3
      e = decay (impulse KR p1 0) 0.2
  in  out 0 (sinOsc AR p0 0 * e)

control_test :: Spec
control_test = compare_graphdefs "control_graph" control_graph

dce_graph :: UGen
dce_graph =
  let s0 = sinOsc AR 440 0
      s1 = sinOsc AR 441 0
      s2 = sinOsc AR 442 0
  in  out 0 (s0+s1+s2)

dce_test :: Spec
dce_test = compare_graphdefs "dead_code_eliminated_graph" dce_graph

whysc_test :: Spec
whysc_test = compare_graphdefs "whysc_graph" whysc

convertTests :: Spec
convertTests =
  describe "convert" $ do
    simple_test
    control_test
    dce_test
    whysc_test

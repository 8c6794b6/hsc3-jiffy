{-# LANGUAGE RecordWildCards #-}
-- | Module to contain tests for "Sound.SC3.UGen.Jiffy.Builder"
module BuilderTests
  ( builderTests
  ) where

-- base
import Data.Foldable (find)

-- hspec
import Test.Hspec

-- hsc3
import Sound.SC3.UGen.Graph (U_Graph(..), U_Node(..))
import Sound.SC3.UGen.Type (Special(..))

-- Internal
import Sound.SC3.Jiffy

--
-- Auxiliary functions
--

hasUNode :: (U_Node -> Bool) -> U_Graph -> Bool
hasUNode test g = maybe False (const True) (find test (ug_ugens g))

hasUGenNamed :: String -> U_Graph -> Bool
hasUGenNamed name =
  let test n = case n of
                 U_Node_U {..} -> u_node_u_name == name
                 _ -> False
  in  hasUNode test

hasBinOp :: Binary -> U_Graph -> Bool
hasBinOp op =
  let test n =
        case n of
          U_Node_U {..} -> u_node_u_name == "BinaryOpUGen" &&
                           u_node_u_special == Special (fromEnum op)
          _ -> False
  in  hasUNode test

hasAdd, hasMul, hasMulAdd, hasSub :: U_Graph -> Bool
hasAdd = hasBinOp Add
hasMul = hasBinOp Mul
hasMulAdd = hasUGenNamed "MulAdd"
hasSub = hasBinOp Sub

--
-- Specs
--

optimize_adds :: Spec
optimize_adds =
  describe "optimize_add" $ do
    let u2g = ugen_to_graph

    describe "constant_folding" $ do
      let g0 = u2g (out 0 (sinOsc AR (440 * 2 + 110) 0))
      it "has_no_MulAdd" (g0 `shouldNotSatisfy` hasMulAdd)
      it "has_no_Add" (g0 `shouldNotSatisfy` hasAdd)

    describe "Sum4" $ do
      let g0 = u2g (out 0 (let s = sinOsc AR 440 0 in s+s+s+s))
      it "has_Sum4" (g0 `shouldSatisfy` hasUGenNamed "Sum4")
      it "has_no_Add" (g0 `shouldNotSatisfy` hasAdd)

    describe "Sum3" $ do
      let g0 = u2g (out 0 (let s = sinOsc AR 440 0 in s+s+s))
      it "has_Sum3" (g0 `shouldSatisfy` hasUGenNamed "Sum3")
      it "has_no_Add" (g0 `shouldNotSatisfy` hasAdd)

    describe "MulAdd" $ do
      let g0 = u2g (out 0 (sinOsc AR 440 0 * 200 + 100))
          g1 = u2g (out 0 (200 * sinOsc KR 10 0 + 100))
          g2 = u2g (out 0 (dc IR 0 * 200 + 100))
      it "has_AR_MulAdd" (g0 `shouldSatisfy` hasMulAdd)
      it "has_KR_MulAdd" (g1 `shouldSatisfy` hasMulAdd)
      it "has_no_IR_MulAdd" (g2 `shouldNotSatisfy` hasMulAdd)

    describe "Sub" $ do
      let g0 = u2g (out 0 (sinOsc AR 5 0 + (negate (saw AR 2))))
      it "has_Sub" (g0 `shouldSatisfy` hasSub)
      it "has_no_Add" (g0 `shouldNotSatisfy` hasAdd)

    describe "dead_code_elimination" $ do
      -- Want to leave `d' as-is, because it's used in out.  The `d'
      -- node is used as an argument of `e' node, which multiplys with
      -- `c', so there is a chance to replace with MulAdd, but prefer
      -- sharing the node than applying MulAdd.
      let g0 = u2g (let a = sinOsc AR 1 0
                        b = sinOsc AR 2 0
                        c = sinOsc AR 3 0
                        d = a * b
                        e = d + c
                    in  out 0 (mce2 d e))
      it "has_Mul" (g0 `shouldSatisfy` hasMul)
      it "has_Add" (g0 `shouldSatisfy` hasAdd)
      it "has_no_MulAdd" (g0 `shouldNotSatisfy` hasMulAdd)

optimize_sub :: Spec
optimize_sub =
  describe "optimize_sub" $ do
    let u2g = ugen_to_graph
    describe "constant_folding" $ do
      let g0 = u2g (out 0 (sinOsc AR (546 - 53 * 2) 0 * 0.1))
      it "has_no_Sub" (g0 `shouldNotSatisfy` hasSub)
    describe "replace_with_constant_on_rhs" $ do
      let g1 = u2g (out 0 (sinOsc AR (440 - negate (saw KR 1 * 20)) 0))
      it "has_Add" (g1 `shouldSatisfy` hasAdd)
      it "has_no_Sub" (g1 `shouldNotSatisfy` hasSub)
    describe "simple_replacement" $ do
      let g2 = u2g (out 0 (sinOsc AR 440 0 - negate (whiteNoise AR)))
      it "has_Add" (g2 `shouldSatisfy` hasAdd)
      it "has_no_Sub" (g2 `shouldNotSatisfy` hasSub)
    describe "no_replacement" $ do
      let g3 = u2g (out 0 (sinOsc AR ((saw KR 1 * 110 + 550) - 40) 0))
      it "has_Sub" (g3 `shouldSatisfy` hasSub)

builderTests :: Spec
builderTests =
  describe "builder"
           (do optimize_adds
               optimize_sub)

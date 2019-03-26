{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- | Module to contain tests for "Sound.SC3.UGen.Jiffy.Builder"
module BuilderTests
  ( builderTests
  ) where

-- base
import Data.Foldable (find)

#if MIN_VERSION_base (4,9,0)
import qualified Control.Monad.Fail as MonadFail
#endif

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

byNameAndSpecial :: (String -> Special -> Bool) -> U_Node -> Bool
byNameAndSpecial test un =
  case un of
    U_Node_U {..} -> test u_node_u_name u_node_u_special
    _             -> False

hasUGenNamed :: String -> U_Graph -> Bool
hasUGenNamed n = hasUNode (byNameAndSpecial (\n' _ -> n' == n))

hasBinaryOp :: Binary -> U_Graph -> Bool
hasBinaryOp = hasOperator "BinaryOpUGen"

hasUnaryOp :: Unary -> U_Graph -> Bool
hasUnaryOp = hasOperator "UnaryOpUGen"

hasOperator :: Enum a => String -> a -> U_Graph -> Bool
hasOperator name op =
  hasUNode (byNameAndSpecial
            (\name' sp -> name' == name &&
                          sp == Special (fromEnum op)))

hasAdd, hasMul, hasMulAdd, hasSub :: U_Graph -> Bool
hasAdd = hasBinaryOp Add
hasMul = hasBinaryOp Mul
hasMulAdd = hasUGenNamed "MulAdd"
hasSub = hasBinaryOp Sub

--
-- Specs
--

ugen_instance_tests :: Spec
ugen_instance_tests =
  describe "instance" $ do
    let u2g = ugen_to_graph
        de = describe
        unary_spec op f = do
          let g0 = out 0 (dc AR (f 42))
              g1 = out 0 (f (saw AR 42))
              op' = show op
          it ("has_" ++ op') (u2g g1 `shouldSatisfy` hasUnaryOp op)
          it ("has_no_" ++ op') (u2g g0 `shouldNotSatisfy` hasUnaryOp op)
        binary_spec op f = do
          let g0 = out 0 (dc AR (f 42 43))
              g1 = out 0 (f (saw AR 42) (saw AR 43))
              op' = show op
          it ("has_" ++ op') (u2g g1 `shouldSatisfy` hasBinaryOp op)
          it ("has_no_" ++ op') (u2g g0 `shouldNotSatisfy` hasBinaryOp op)
        unary_spec_no_cf op f = do
          let g0 = out 0 (dc AR (f 42))
              g1 = out 0 (f (saw AR 42))
              op' = show op
          it ("has_" ++ op') (u2g g1 `shouldSatisfy` hasUnaryOp op)
          it ("has_" ++ op' ++ "_for_constant_node") $
             (u2g g0 `shouldSatisfy` hasUnaryOp op)
        binary_spec_no_cf op f = do
          let g0 = out 0 (dc AR (f 42 43))
              g1 = out 0 (f (saw AR 42) (saw AR 43))
              op' = show op
          it ("has_" ++ op') (u2g g1 `shouldSatisfy` hasBinaryOp op)
          it ("has_" ++ op' ++ "_for_constant_node") $
             (u2g g0 `shouldSatisfy` hasBinaryOp op)
        err x =
          it "should_call_error" $
            print (u2g x) `shouldThrow` anyErrorCall

    de "Eq" $ do
      let g0 = sinOsc AR 440 0
          g1 = sinOsc AR 333 0
      it "is_equal" (g0 `shouldBe` g0)
      it "is_not_equal" (g0 `shouldNotBe` g1)

    describe "Ord" $ do
      it "can_compare_constant_nodes" $
         compare (constant 1) (constant 2) `shouldBe` LT
      de "min" $ binary_spec Min min
      de "max" $ binary_spec Max max

    describe "Enum" $ do
      let g0 f = sinOsc AR (f 440) 0
      de "succ" $ it "increments" $ g0 succ `shouldBe` g0 (+1)
      de "pred" $ it "decrements" $ g0 pred `shouldBe` g0 (\x -> x - 1)
      de "fromEnum" $ it "convert_to_Int" $
        fromEnum (constant 42) `shouldBe` 42
      de "toEnum" $ it "convert_from_Int" $
        toEnum 42 `shouldBe` constant 42
      de "enumFrom" $ it "works_with_constant" $
        [0..] !! 10 `shouldBe` constant 9
      de "enumFromTo" $ it "works_from_to_with_constant" $
        sum [0..10] `shouldBe` constant 55
      de "enumFromThen" $ it "works_from_then_with_constant" $
        [0,10..] !! 10 `shouldBe` constant 90
      de "enumFromThenTo" $ do
        it "works_from_then_to_with_constant" $
          sum [0,10..100] `shouldBe` constant 550
        it "can_enumerate_down" $
          [10,9..0] !! 10 `shouldBe` constant 0

    describe "Num" $ do
      de "abs" $ unary_spec Abs abs
      de "signum" $ unary_spec Sign signum

    describe "Real" $ do
      it "can_converted_to_Rational" $
        toRational (constant 1) `shouldBe` 1

    describe "Fractional" $ do
      de "recip" $ unary_spec Recip recip
      de "(/)" $ binary_spec FDiv (/)
      let g0 = constant 2
      it "can_converted_from_Rational" $
        fromRational (toRational g0) `shouldBe` g0

    describe "Floating" $ do
      de "pi" $ it "is_pi" $ constant pi `shouldBe` pi
      de "exp" $ unary_spec Exp exp
      de "log" $ unary_spec Log log
      de "sqrt" $ unary_spec Sqrt sqrt
      de "(**)" $ binary_spec Pow (**)
      de "logBase" $ it "shows_equal_result" $
        logBase (constant 10) (constant 20) `shouldBe`
        constant (log 20 / log 10)
      de "sin" $ unary_spec Sin sin
      de "cos" $ unary_spec Cos cos
      de "tan" $ unary_spec Tan tan
      de "asin" $ unary_spec ArcSin asin
      de "acos" $ unary_spec ArcCos acos
      de "atan" $ unary_spec ArcTan atan
      de "sinh" $ unary_spec SinH sinh
      de "cosh" $ unary_spec CosH cosh
      de "tanh" $ unary_spec TanH tanh
      de "asinh" $ it "folds_constants" $ asinh (constant 0) `shouldBe` 0
      de "acosh" $ it "folds_constants" $ acosh (constant 1) `shouldBe` 0
      de "atanh" $ it "folds_constants" $ atanh (constant 0) `shouldBe` 0

    describe "RealFrac" $ do
      let c42 = constant 42
      de "properFraction" $
        it "should_call_error" $
          (case properFraction c42 of (a,_) -> return (a::Int))
           `shouldThrow` anyErrorCall
      de "truncate" $ err (truncate c42)
      de "round" $ err (round c42)
      de "ceiling" $ err (ceiling c42)
      de "floor" $ err (floor c42)

    describe "Integral" $ do
      de "quot" $ binary_spec_no_cf IDiv quot
      de "rem" $ binary_spec_no_cf Mod rem
      de "quotRem" $ it "has_IDiv_and_Mod" $ do
        let (q,r) = quotRem 11 4
        u2g (out 0 (dc AR q)) `shouldSatisfy` hasBinaryOp IDiv
        u2g (out 0 (dc AR r)) `shouldSatisfy` hasBinaryOp Mod
      de "div" $ binary_spec_no_cf IDiv div
      de "mod" $ binary_spec_no_cf Mod mod
      de "toInteger" $
        it "should_call_error" $
          print (toInteger (constant 42)) `shouldThrow` anyErrorCall

    describe "EqE" $ do
      de "equal_to" $ binary_spec EQ_ equal_to
      de "not_equal_to" $ binary_spec NE not_equal_to

    describe "OrdE" $ do
      de "less_than" $ binary_spec LT_ less_than
      de "less_than_or_equal_to" $ binary_spec LE less_than_or_equal_to
      de "greater_than" $ binary_spec GT_ greater_than
      de "greater_than_or_equal_to" $
          binary_spec GE greater_than_or_equal_to

    de "RealFracE" $ do
      let c42 = constant 42
      de "properFractionE" $
        it "should_call_error" $
          (case properFractionE c42 of (a,_) -> return (a::UGen))
           `shouldThrow` anyErrorCall
      de "truncateE" $
        it "should_call_error" $
          (case truncateE c42 of a -> a `seq` return (a::UGen))
           `shouldThrow` anyErrorCall
      de "roundE" $ do
        let g0 = out 0 (roundE 1.2)
            g1 = out 0 (roundE (sinOsc AR 440 0 * 100))
        it "has_no_Round" $ u2g g0 `shouldNotSatisfy` hasBinaryOp Round
        it "has_Round" $ u2g g1 `shouldSatisfy` hasBinaryOp Round
      de "ceilingE" $ unary_spec Ceil ceilingE
      de "floorE" $ unary_spec Floor floorE

    describe "UnaryOp" $ do
      de "ampDb" $ unary_spec AmpDb ampDb
      de "asFloat" $ unary_spec_no_cf AsFloat asFloat
      de "asInt" $ unary_spec_no_cf AsInt asInt
      de "cpsMIDI" $ unary_spec CPSMIDI cpsMIDI
      de "cpsOct" $ unary_spec CPSOct cpsOct
      de "cubed" $ unary_spec Cubed cubed
      de "dbAmp" $ unary_spec DbAmp dbAmp
      de "distort" $ unary_spec Distort distort
      de "frac" $ unary_spec_no_cf Frac frac
      de "isNil" $ unary_spec IsNil isNil
      de "log10" $ unary_spec Log10 log10
      de "log2" $ unary_spec Log2 log2
      de "midiCPS" $ unary_spec MIDICPS midiCPS
      de "midiRatio" $ unary_spec MIDIRatio midiRatio
      de "notE" $ unary_spec Not notE
      de "notNil" $ unary_spec NotNil notNil
      de "octCPS" $ unary_spec OctCPS octCPS
      de "ramp_" $ unary_spec_no_cf Ramp_ ramp_
      de "ratioMIDI" $ unary_spec RatioMIDI ratioMIDI
      de "softClip" $ unary_spec SoftClip softClip
      de "squared" $ unary_spec Squared squared

    describe "BinaryOp" $ do
      de "absDif" $ binary_spec AbsDif absDif
      de "amClip" $ binary_spec AMClip amClip
      de "atan2E" $ binary_spec Atan2 atan2E
      de "clip2" $ binary_spec Clip2 clip2
      de "difSqr" $ binary_spec DifSqr difSqr
      de "excess" $ binary_spec Excess excess
      de "exprandRange" $ binary_spec_no_cf ExpRandRange exprandRange
      de "fill" $ binary_spec_no_cf Fill fill
      de "firstArg" $ binary_spec FirstArg firstArg
      de "fold2" $ binary_spec Fold2 fold2
      de "gcdE" $ binary_spec_no_cf GCD gcdE
      de "hypot" $ binary_spec Hypot hypot
      de "hypotx" $ binary_spec Hypotx hypotx
      de "iDiv" $ binary_spec IDiv iDiv
      de "lcmE" $ binary_spec_no_cf LCM lcmE
      de "modE" $ binary_spec Mod modE
      de "randRange" $ binary_spec_no_cf RandRange randRange
      de "ring1" $ binary_spec Ring1 ring1
      de "ring2" $ binary_spec Ring2 ring2
      de "ring3" $ binary_spec Ring3 ring3
      de "ring4" $ binary_spec Ring4 ring4
      de "roundUp" $ binary_spec RoundUp roundUp
      de "scaleNeg" $ binary_spec ScaleNeg scaleNeg
      de "sqrDif" $ binary_spec SqrDif sqrDif
      de "sqrSum" $ binary_spec SqrSum sqrSum
      de "thresh" $ binary_spec Thresh thresh
      de "trunc" $ binary_spec_no_cf Trunc trunc
      de "wrap2" $ binary_spec Wrap2 wrap2

    describe "Show" $ do
      it "shows <UGen>" $ show (undefined :: UGen) `shouldBe` "<UGen>"

optimize_simple_constant_tests :: Spec
optimize_simple_constant_tests =
  describe "optimize_simple_constants" $ do
    let u2g = ugen_to_graphdef "test"
    describe "add" $ do
      let g0 = out 0 (sinOsc AR 440 0)
          g1 = out 0 (0 + sinOsc AR 440 0 + 0)
      it "is_equal" (u2g g0 `shouldBe` u2g g1)
    describe "sub" $ do
      let g0 = out 0 (negate (sinOsc AR 440 0))
          g1 = out 0 (0 - sinOsc AR 440 0 - 0)
      it "is_equal" (u2g g0 `shouldBe` u2g g1)
    describe "mul" $ do
      let g0 = out 0 (negate (sinOsc AR 440 0))
          g1 = out 0 (1 * sinOsc AR 440 0 * (-1))
      it "is_equal" (u2g g0 `shouldBe` u2g g1)
      let g2 = out 0 (negate (sinOsc AR 440 0))
          g3 = out 0 ((-1) * sinOsc AR 440 0 * 1)
      it "is_equal" (u2g g3 `shouldBe` u2g g2)
      let g5 = out 0 (negate (sinOsc AR 440 0) + (whiteNoise AR * 0))
      it "is_equal" (u2g g5 `shouldBe` u2g g2)
    describe "div" $ do
      let g0 = out 0 (negate (sinOsc AR 440 0))
          g1 = out 0 ((sinOsc AR 440 0 / (-1)) / 1)
      it "is_equal" (u2g g0 `shouldBe` u2g g1)

optimize_add_tests :: Spec
optimize_add_tests =
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
      -- Want to leave `d' as-is, because it is used by out.  The `d'
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

optimize_sub_tests :: Spec
optimize_sub_tests =
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

auxiliary_tests :: Spec
auxiliary_tests = do
  describe "G" $ do
    it "should_return_3" $ runUGen (return 3) `shouldBe` (3::Int)
    it "should_return_4" $
      runUGen (fmap succ (return 3)) `shouldBe` (4::Int)
    it "should_return_5" $
      runUGen (pure (+2) <*> pure 3) `shouldBe` (5::Int)
#if MIN_VERSION_base (4,9,0)
    it "should_call_error" $
      runUGen (MonadFail.fail "xxx") `shouldThrow` anyErrorCall
#endif

builderTests :: Spec
builderTests =
  describe "builder"
           (do ugen_instance_tests
               optimize_simple_constant_tests
               optimize_add_tests
               optimize_sub_tests
               auxiliary_tests)

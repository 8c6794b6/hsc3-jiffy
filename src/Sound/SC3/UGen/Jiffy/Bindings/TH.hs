{-# LANGUAGE TemplateHaskell #-}
-- | Module containing template haskell function for generating 'UGen'
-- binding functions from ugen database information in hsc3-db package.
--
-- Doing similar works done in "Sound.SC3.UGen.DB.Bindings" module, but
-- with template haskell.
--
module Sound.SC3.UGen.Jiffy.Bindings.TH where

-- template-haskell
import Language.Haskell.TH

-- hsc3
import Sound.SC3
  ( DoneAction, Envelope, Interpolation, Loop, Rate, Warp
  , from_done_action, from_interpolation, from_loop, from_warp )

-- hsc3-db
import Sound.SC3.UGen.DB (ugenDB)
import Sound.SC3.UGen.DB.Bindings (ugen_hs_name)
import Sound.SC3.UGen.DB.Record (U(..), ugen_fixed_rate)
import Sound.SC3.UGen.DB.Rename (u_renamed_inputs)

-- Internal
import Sound.SC3.UGen.Jiffy.Builder

-- | Macro to define UGen functions.
ugenDecsQ :: Q [Dec]
ugenDecsQ = sequence (concatMap defineUGen ugenDB)

-- | Return declarations of type signature and function body for 'UGen'
-- binding function of given 'U'.
defineUGen :: U -> [DecQ]
defineUGen u =
  let name = case ugen_hs_name u of
               -- Avoid conflicts.
               "concat" -> mkName "concat'"
               "max" -> mkName "max'"
               other -> mkName other
      rate_name = mkName "rate"
      numchan_name = mkName "numChannels"
      scnameE = stringE (ugen_name u)
      input_names = map mkName (u_renamed_inputs u)
      input_pats = map varP input_names
      (pats0, rateE, tyargs0)
        | Just is <- ugen_filter u
        = let r = case is of
                    [i] -> [e|get_rate_at i|]
                    _   -> [e|maximum_rate is|]
          in  (input_pats, r, input_tys)
        | Just r <- ugen_fixed_rate u
        = let r' = fromEnum r
          in  (input_pats, [e|const_rate (toEnum r')|], input_tys)
        | otherwise
        = let p = varP rate_name : input_pats
              r = [e|const_rate $(varE rate_name)|]
              t = [t|Rate|] : input_tys
          in  (p, r, t)
      (pats1, tyargs1)
        | ugen_nc_input u
        = (varP numchan_name : pats0, [t|Int|] : tyargs0)
        | otherwise = (pats0, tyargs0)
      as_enum_typ iv (i,_) =
        maybe ugenT enum_type_of (lookup i iv)
      as_enum_exp iv (i,n) =
        maybe (varE n) (enum_exp_of n) (lookup i iv)
      (input_exps, input_tys)
        | Just iv <- ugen_enumerations u
        = let ixs = zip [0..] input_names
          in  (map (as_enum_exp iv) ixs, map (as_enum_typ iv) ixs)
        | otherwise
        = (map varE input_names, map (const ugenT) input_names)
      noutE
        | Just n <- ugen_outputs u = [e|n|]
        | ugen_nc_input u = varE numchan_name
        | otherwise = [e|error $(stringE nout_error_msg)|]
      nout_error_msg = ugen_name u ++ ": no outputs?"
      ugenidE = if ugen_nondet u
                   then [e|hashUId|]
                   else [e|noId|]
      isPure = ugen_name u `notElem` impureUGens
      mk
        | "LocalBuf" == ugen_name u = [e|mkLocalBufUGen|]
        | "Demand" == ugen_name u   = [e|mkDemandUGen|]
        | ugen_std_mce u > 0        = [e|mkChannelsArrayUGen isPure|]
        | not isPure                = [e|mkImpureUGen|]
        | otherwise                 = [e|mkSimpleUGen|]
      ugenT = [t|UGen|]
      bodyE = [e|$(mk) $(noutE) $(ugenidE) spec0 $(scnameE)
                       $(rateE) $(listE input_exps) |]
      --
      typ = foldr (\a b -> appT (appT arrowT a) b) ugenT tyargs1
      cls = [clause pats1 (normalB bodyE) []]
      --
  in  if ugen_name u `elem` ignoredUGens
         then []
         else [sigD name typ, funD name cls]

enum_exp_of :: Name -> String -> ExpQ
enum_exp_of var_name fn =
  case fn of
    "DoneAction"    -> [e|from_done_action $(var)|]
    "Envelope"      -> [e|envelope_to_ugen $(var)|]
    "Interpolation" -> [e|from_interpolation $(var)|]
    "Loop"          -> [e|from_loop $(var)|]
    "Warp"          -> [e|from_warp $(var)|]
    _               -> [e|error $(stringE (show var_name ++ fn))|]
  where
    var = varE var_name

enum_type_of :: String -> TypeQ
enum_type_of name =
  case name of
    "DoneAction"    -> [t|DoneAction UGen|]
    "Envelope"      -> [t|Envelope UGen|]
    "Loop"          -> [t|Loop UGen|]
    "Interpolation" -> [t|Interpolation UGen|]
    "Warp"          -> [t|Warp UGen|]
    _               -> varT (mkName "unknown_enum_type")

-- | Name of ignored UGens. UGens listed here may exist in database but
-- defined with hand written codes.
ignoredUGens :: [String]
ignoredUGens =
  [ "Dwrand", "PV_HainsworthFoote", "Poll" ]

-- | Name of impure UGens. UGens listed here won't be removed during
-- dead code elimination.
impureUGens :: [String]
impureUGens =
  [ -- From "BufIO.sc"
    "BufWr", "RecordBuf", "ScopeOut", "ScopeOut2", "SetBuf", "ClearBuf"

    -- From "Demand.sc"
  , "Demand", "Dutye", "TDuty"

    -- From "DiskIO.sc"
  , "DiskOut"

    -- From "EnvGen.sc"
  , "Done", "FreeSelf", "PauseSelf", "FreeSelfWhenDone"
  , "PauseSelfWhenDone", "Pause", "Free"

    -- From "Filter.sc"
  , "DetectSilence"

    -- From "InOut.sc"
  , "Out",  "ReplaceOut", "OffsetOut", "LocalOut", "XOut"

    -- From "Noise.sc"
  , "RandID", "RandSeed"

    -- From "Poll.sc"
  , "Poll"

    -- From "Trig.sc"
  , "SendTrig", "SendReply"
  ]

{-
Note [Pure and impure UGens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pure and impure UGens mentioned here means whether unused UGens
instances in synthdef graph could be removed or not.  Pure UGens could
be removed, and impure UGens could not.

Currently, template haskell code is using manually selected impure UGen
names, listed in "impureUGens".  This approach may result in unwanted
removal if there exist a UGen for side effect purpose which is not in
the list.

The sclang class hierarchy has an abstract class named "PureUGen", and
it is known that the sub classes of this UGen could be safely removed
from synthdef graph when unused. Below is a list of sclang UGen class
names which are sub class of "PureUGen" and "PureMultiOutUGen", plus
"BinaryOpUGen" and "UnaryOpUGen":

pureUGens :: [String]
pureUGens =
  [ "A2K", "APF", "AllpassC", "AllpassL", "AllpassN", "AmpComp"
  , "AmpCompA", "BAllPass", "BBandPass", "BBandStop", "BEQSuite"
  , "BHiPass", "BHiShelf", "BLowPass", "BLowShelf", "BPF", "BPZ2"
  , "BPeakEQ", "BRF", "BRZ2", "BinaryOpUGen", "COsc", "Changed"
  , "CircleRamp", "CombC", "CombL", "CombN", "DC", "Decay", "Decay2"
  , "DegreeToKey", "Delay1", "Delay2", "DelayC", "DelayL", "DelayN"
  , "DetectIndex", "DetectSilence", "FOS", "Filter", "Formant", "Formlet"
  , "FreeVerb", "Friction", "GlitchBPF", "GlitchBRF", "GlitchHPF"
  , "GlitchRHPF", "HPF", "HPZ1", "HPZ2", "Impulse", "Index"
  , "IndexInBetween", "IndexL", "InsideOut", "Integrator", "K2A", "LFCub"
  , "LFPar", "LFPulse", "LFSaw", "LFTri", "LPF", "LPZ1", "LPZ2", "Lag"
  , "Lag2", "Lag2UD", "Lag3", "Lag3UD", "LagUD", "LeakDC", "LinExp"
  , "MeanTriggered", "Median", "MedianTriggered", "MidEQ", "MoogFF"
  , "OnePole", "OneZero", "Osc", "OscN", "PureMultiOutUGen", "PureUGen"
  , "RHPF", "RLPF", "Ramp", "Resonz", "Ringz", "SOS", "Select", "Shaper"
  , "SinOsc", "SinOscFB", "Slew", "Slope", "SyncSaw", "T2A", "T2K"
  , "TwoPole", "TwoZero", "UnaryOpUGen", "VOsc", "VOsc3", "VarLag"
  , "VarSaw", "Vibrato", "WaveLoss", "WrapIndex" ]

The above "pureUGen" names were generated with following sclang code:

(
var us = UGen.allSubclasses.reject({ |c|
        not(c.isKindOf(PureUGen.class)) &&
        not(c.isKindOf(PureMultiOutUGen.class)) &&
        not(c.class == BinaryOpUGen.class) &&
        not(c.class == UnaryOpUGen.class);
}).sort({|a b| a.name < b.name
}).collect({|c| "\"" ++ c ++ "\""
});

Post << us << Char.nl;
)

-}

--

_dump :: U -> IO ()
_dump u = do
  docs <- mapM runQ (defineUGen u)
  mapM_ (print . ppr) docs

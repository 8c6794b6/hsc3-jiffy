{-# LANGUAGE TemplateHaskell #-}
-- | Module containing template haskell function for generating 'UGen'
-- binding functions from ugen database information in hsc3-db package.
--
-- Doing similar works done in "Sound.SC3.UGen.DB.Bindings" module, but
-- with template haskell.
--
module Sound.SC3.Jiffy.UGen.TH where

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
import Sound.SC3.Jiffy.UGen.Builder

-- | Macro to define UGen functions.
ugenDecsQ :: Q [Dec]
ugenDecsQ = sequence (concatMap defineUGen ugenDB)

-- | Return declarations of type signature and function body for 'UGen'
-- binding function of given 'U'.
defineUGen :: U -> [DecQ]
defineUGen u =
  let name = mkName (ugen_hs_name u)
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
      inputE = if ugen_std_mce u > 0
                  then [e|stdmce_inputs|]
                  else [e|simple_inputs|]
      ugenT = [t|UGen|]
      bodyE = [e|mkUGen $(noutE) $(ugenidE) spec0 $(scnameE)
                        $(rateE) $(inputE) $(listE input_exps)|]
      --
      typ = foldr (\a b -> appT (appT arrowT a) b) ugenT tyargs1
      cls = [clause pats1 (normalB bodyE) []]
      --
  in  [sigD name typ, funD name cls]

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

--

_dump :: U -> IO ()
_dump u = do
  docs <- mapM runQ (defineUGen u)
  mapM_ (print . ppr) docs

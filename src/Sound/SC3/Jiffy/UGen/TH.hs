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
import Sound.SC3 (Rate)

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
      cls = [clause pats1 (normalB bodyE) []]
      rate_name = mkName "rate"
      numchan_name = mkName "numChannels"
      (pats1, tyargs1)
        | ugen_nc_input u
        = (varP numchan_name : pats0, [t|Int|] : tyargs0)
        | otherwise = (pats0, tyargs0)
      (pats0, rateE, tyargs0)
        | Just is <- ugen_filter u
        = (input_pats, [e|maximum_rate is|], input_tys)
        | Just r <- ugen_fixed_rate u
        = let r' = fromEnum r
          in  (input_pats, [e|const_rate (toEnum r')|], input_tys)
        | otherwise
        = let p = varP rate_name : input_pats
              r = [e|const_rate $(varE rate_name)|]
              t = [t|Rate|] : input_tys
          in  (p, r, t)
      input_pats = map varP input_names
      input_exps = map varE input_names
      input_names = map mkName (u_renamed_inputs u)
      scnameE = stringE (ugen_name u)
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
      --
      bodyE = [e|mkUGen $(noutE) $(ugenidE) spec0 $(scnameE)
                        $(rateE) $(inputE) $(listE input_exps)|]
      --
      typ = foldr (\a b -> appT (appT arrowT a) b) ugenT tyargs1
      input_tys = map (const ugenT) input_names
      ugenT = [t|UGen|]
      --
  in  [sigD name typ, funD name cls]

--

_dump :: U -> IO ()
_dump u = do
  docs <- mapM runQ (defineUGen u)
  mapM_ (print . ppr) docs

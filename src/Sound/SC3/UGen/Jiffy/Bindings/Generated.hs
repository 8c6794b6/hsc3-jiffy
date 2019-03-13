{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Module containing UGen binding functions generated with template
-- haskell, from the data defined in 'Sound.SC3.UGen.DB.ugenDB'.
module Sound.SC3.UGen.Jiffy.Bindings.Generated where

-- Internal
import Sound.SC3.UGen.Jiffy.Bindings.TH (ugenDecsQ)

$(ugenDecsQ)

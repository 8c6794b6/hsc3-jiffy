{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Sound.SC3.UGen.Jiffy.Bindings.DB where

-- Internal
import Sound.SC3.UGen.Jiffy.Bindings.TH (ugenDecsQ)

$(ugenDecsQ)

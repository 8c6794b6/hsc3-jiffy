{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Sound.SC3.Jiffy.UGen.DB where

-- Internal
import Sound.SC3.Jiffy.UGen.TH (ugenDecsQ)

$(ugenDecsQ)

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Sound.SC3.UGen.Jiffy.DB where

-- Internal
import Sound.SC3.UGen.Jiffy.TH (ugenDecsQ)

$(ugenDecsQ)

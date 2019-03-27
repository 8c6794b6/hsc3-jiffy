-- | Replacement module for "Sound.SC3".
--
-- This module re-exports modules exported from "Sound.SC3", except for
-- UGen related modules and functions which are re-exported from
-- hsc3-jiffy package.
--
module Sound.SC3.Jiffy
  ( module Sound.SC3.Common
  , module Sound.SC3.Jiffy.Compat
  , module Sound.SC3.Jiffy.DumpUGens
  , module Sound.SC3.UGen.Jiffy
  ) where

-- hsc3
import Sound.SC3.Common

-- Internal
import Sound.SC3.Jiffy.Compat
import Sound.SC3.Jiffy.DumpUGens
import Sound.SC3.UGen.Jiffy

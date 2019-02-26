-- | Replacement module for "Sound.SC3".
--
-- This module re-exports modules re-exported from "Sound.SC3", except
-- for UGen related modules and functions which are re-exported from
-- hsc3-jiffy package.

module Sound.SC3.Jiffy (module All) where

-- hsc3
import Sound.SC3.Common as All

-- XXX: Hide functions using "Sound.SC3.UGen".
import Sound.SC3.Server.Monad as All

-- Internal
import Sound.SC3.UGen.Jiffy as All

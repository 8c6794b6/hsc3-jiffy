-- | Replacement module for "Sound.SC3".
--
-- This module re-exports modules re-exported from "Sound.SC3", except
-- for UGen related modules and functions which are re-exported from
-- hsc3-jiffy package.

module Sound.SC3.Jiffy (module All) where

-- hsc3
import Sound.SC3.Common as All
import Sound.SC3.Server.Monad as All hiding
  ( Synthdef(..), synthdef
  , defaultSynthdef, defaultSampler
  , synthdefGraph, synthdefParam, synthdef_to_graphdef, synthdefData
  , synthdefWrite, synthdefWrite_dir
  , synthstat, synthstat_ln , synthstat_wr , synthstat_concise
  , d_recv )

-- Internal
import Sound.SC3.Jiffy.Compat as All
import Sound.SC3.UGen.Jiffy as All

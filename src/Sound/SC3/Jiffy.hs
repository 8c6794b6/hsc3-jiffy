-- | Replacement module for "Sound.SC3".
--
-- This module re-exports modules exported from "Sound.SC3", except for
-- UGen related modules and functions which are re-exported from
-- hsc3-jiffy package.
--
-- Some of the functions exported from "Sound.SC3.Server.Monad" are
-- using 'Sound.SC3.UGen.Type.UGen' type defined in hsc3 package, which
-- could not be reused with 'UGen' type defined in hsc3-jiffy
-- package. This module hides functions using the hsc3
-- 'Sound.SC3.UGen.Type.UGen' and re-exports rest of the functions.

module Sound.SC3.Jiffy
  ( -- * Re-exported modules
    module Sound.SC3.Common
  , module Sound.SC3.Jiffy.Compat
  , module Sound.SC3.UGen.Jiffy

    -- * Re-exported functions from Sound.SC3.Server.Monad
  , module Sound.SC3.Server.Monad
  ) where

-- hsc3
import Sound.SC3.Common
import Sound.SC3.Server.Monad hiding
  ( Synthdef(..), synthdef
  , defaultSynthdef, defaultSampler
  , synthdefGraph, synthdefParam, synthdef_to_graphdef, synthdefData
  , synthdefWrite, synthdefWrite_dir
  , synthstat, synthstat_ln , synthstat_wr , synthstat_concise
  , d_recv )

-- Internal
import Sound.SC3.Jiffy.Compat
import Sound.SC3.UGen.Jiffy

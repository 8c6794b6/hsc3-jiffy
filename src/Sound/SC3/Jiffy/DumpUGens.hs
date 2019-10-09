{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- A module containing 'DumpUGens' type class, which is for dumping
-- the ugen contents of synthdef graphs.
--
module Sound.SC3.Jiffy.DumpUGens
  ( DumpUGens(..)
  ) where

-- base
import GHC.IO.Handle.FD (stdout)

--- array
import Data.Array.IArray (Array, (!), listArray)
import Data.Array.Unboxed (UArray)

-- bytestring
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy.Char8 (unpack)

-- hsc3
import Sound.SC3.Common.Math.Operator (unaryName, binaryName)
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import Sound.SC3.UGen.Graph (U_Graph(..))
import qualified Sound.SC3 as SC3
import qualified Sound.SC3.Server.Graphdef as SC3SG
import qualified Sound.SC3.UGen.Graph as SC3UG

-- Internal
import Sound.SC3.Jiffy.Compat
import Sound.SC3.UGen.Jiffy.Builder


-- ------------------------------------------------------------------------
--
-- Dumper
--
-- ------------------------------------------------------------------------

class DumpUGens a where
  dumpUGenString :: a -> String
  dumpUGens :: a -> IO ()
  dumpUGens = putStrLn . dumpUGenString

-- | The dumped format is similar to the format used in
-- "@SynthDef.dumpUGens@" method from /sclang/.
instance DumpUGens Graphdef where
  dumpUGenString = unpack . B.toLazyByteString . dump_graphdef
  dumpUGens = B.hPutBuilder stdout . dump_graphdef

instance DumpUGens U_Graph where
  dumpUGenString = dumpUGenString . graph_to_graphdef "<dump>"
  dumpUGens = dumpUGens . graph_to_graphdef "<dump>"

instance DumpUGens UGen where
  dumpUGenString = dumpUGenString . ugen_to_graphdef "<dump>"
  dumpUGens = dumpUGens . ugen_to_graphdef "<dump>"

instance DumpUGens SC3.UGen where
  dumpUGenString = dumpUGenString . ugen_to_graphdef' "<dump>"
  dumpUGens = dumpUGens . ugen_to_graphdef' "<dump>"

instance DumpUGens Synthdef where
  dumpUGenString (Synthdef name ug) =
    dumpUGenString (ugen_to_graphdef name ug)
  dumpUGens (Synthdef name ug) =
    dumpUGens (ugen_to_graphdef name ug)

instance DumpUGens SC3.Synthdef where
  dumpUGenString (SC3.Synthdef name ug) =
    dumpUGenString (ugen_to_graphdef' name ug)
  dumpUGens (SC3.Synthdef name ug) =
    dumpUGens (ugen_to_graphdef' name ug)

ugen_to_graphdef' :: String -> SC3.UGen -> Graphdef
ugen_to_graphdef' n u = graph_to_graphdef n (SC3UG.ugen_to_graph u)


-- -----------------------------------------------------------------------
--
-- Guts
--
-- -----------------------------------------------------------------------

-- | Array for constants.
type CA = UArray Int SC3.Sample

-- | Array for UGen tuples.
type UA = Array Int SC3SG.UGen

dump_graphdef :: Graphdef -> B.Builder
dump_graphdef gd =
  unlines'
    [ B.byteString (graphdef_name gd)
    , let cs = graphdef_constants gd
          carr = listArray (0,length cs) cs
          us = graphdef_ugens gd
          uarr = listArray (0,length us) us
      in  printsWithIndex carr uarr us
    , "" ]

printsWithIndex :: CA -> UA -> [SC3SG.UGen] -> B.Builder
printsWithIndex ca ua ugens =
  case ugens of
    [] -> "No output"
    _  -> unlines' (zipWith (printOne ca ua) [0..] ugens)

printOne :: CA -> UA -> Int -> SC3SG.UGen -> B.Builder
printOne ca ua i ug@(_name,rate,inputs,_,_special) =
  brackets [pName i Nothing ug, pRate rate, pInputs inputs]
  where
    pName j mb_port (n,_,_,_,s) =
      B.intDec j <> "_" <>
      case n of
        "UnaryOpUGen"  -> B.string7 (unaryName s)
        "BinaryOpUGen" -> B.string7 (binaryName s)
        "Control"      | Just p <- mb_port -> pControl n p
        "TrigControl"  | Just p <- mb_port -> pControl n p
        "AudioControl" | Just p <- mb_port -> pControl n p
        _              | Just p <- mb_port, p /= 0
                       -> B.byteString n <> "[" <> B.intDec p <> "]"
                       | otherwise -> B.byteString n
    pControl n p = B.byteString n <> "[" <> B.intDec p <> "]"
    pRate r =
      case toEnum r of
        SC3.IR -> "scalar"
        SC3.KR -> "control"
        SC3.AR -> "audio"
        SC3.DR -> "demand"
    pInputs is =
      case is of
        [] -> "nil"
        _  -> brackets (map pInput is)
    pInput (SC3SG.Input t k) =
      case t of
        -1 -> B.doubleDec (ca ! k)
        _  -> pName t (Just k) (ua ! t)

unlines' :: [B.Builder] -> B.Builder
unlines' = intercalate "\n"
{-# INLINE unlines' #-}

brackets :: [B.Builder] -> B.Builder
brackets xs = "[ " <> intercalate ", " xs <> " ]"

intercalate :: B.Builder -> [B.Builder] -> B.Builder
intercalate b = go
  where
    go xs =
      case xs of
        []    -> mempty
        x:[]  -> x
        x:xs' -> x <> b <> go xs'
{-# INLINE intercalate #-}

-- | Dump UGens in graph.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Sound.SC3.Jiffy.DumpUGens
  ( DumpUGens(..)
  ) where

-- base
import GHC.IO.Handle.FD (stdout)

-- bytestring
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy.Char8 (pack, unpack)

-- hsc3
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import Sound.SC3.UGen.Graph (U_Graph(..))
import qualified Sound.SC3 as SC3
import qualified Sound.SC3.UGen.Graph as SC3UG

-- Internal
import Sound.SC3.Jiffy.Compat
import Sound.SC3.UGen.Jiffy.Builder

--
-- Dumper
--

class DumpUGens a where
  dumpUGenString :: a -> String
  dumpUGens :: a -> IO ()
  dumpUGens = putStr . dumpUGenString

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

dump_graphdef :: Graphdef -> B.Builder
dump_graphdef gd =
  unlines'
    [ "name: " <> B.lazyByteString (pack (show (graphdef_name gd)))
    , "--- constants ---"
    , printsWithIndex (graphdef_constants gd)
    , "--- controls ---"
    , printsWithIndex (graphdef_controls gd)
    , "--- ugens ---"
    , printsWithIndex (graphdef_ugens gd) ]

unlines' :: [B.Builder] -> B.Builder
unlines' = foldr (\a b -> a <> "\n" <> b) mempty

printsWithIndex :: Show a => [a] -> B.Builder
printsWithIndex xs =
  case xs of
    [] -> "None."
    _  -> let f x y = B.intDec x <> ": " <>
                      B.lazyByteString (pack (show y))
          in  unlines' (zipWith f [(0::Int) ..] xs)

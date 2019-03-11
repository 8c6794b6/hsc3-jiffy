-- | Dump UGens in synthdef graph.
module Sound.SC3.UGen.Jiffy.Dump
  ( Dump(..)
  ) where

-- base
import Data.List (intercalate)

-- hsc3
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import Sound.SC3.UGen.Graph (U_Graph(..), ugen_to_graph)
import qualified Sound.SC3 as SC3

--
-- Dumper
--

class Dump a where
  dumpString :: a -> String
  dump :: a -> IO ()
  dump = putStrLn . dumpString

instance Dump U_Graph where
  dumpString = dump_u_graph

instance Dump Graphdef where
  dumpString = dump_graphdef

instance Dump SC3.UGen where
  dumpString =
    dumpString . graph_to_graphdef "<dump>" . ugen_to_graph

dump_u_graph :: U_Graph -> String
dump_u_graph ugraph =
  unlines'
    [ "--- constants ---"
    , prints (ug_constants ugraph)
    , "--- controls ---"
    , prints (ug_controls ugraph)
    , "--- ugens ---"
    , prints (ug_ugens ugraph) ]

dump_graphdef :: Graphdef -> String
dump_graphdef gd =
  unlines'
    [ "name: " ++ show (graphdef_name gd)
     , "--- constants ---"
     , printsWithIndex (graphdef_constants gd)
     , "--- controls ---"
     , printsWithIndex (graphdef_controls gd)
     , "--- ugens ---"
     , printsWithIndex (graphdef_ugens gd) ]

unlines' :: [String] -> String
unlines' = intercalate "\n"

prints :: Show a => [a] -> String
prints xs = case xs of
  [] -> "None."
  _  -> unlines' (map show xs)

printsWithIndex :: Show a => [a] -> String
printsWithIndex xs =
  case xs of
    [] -> "None."
    _  -> let f x y = concat [show x, ": ", show y]
          in  unlines' (zipWith f [(0::Int) ..] xs)

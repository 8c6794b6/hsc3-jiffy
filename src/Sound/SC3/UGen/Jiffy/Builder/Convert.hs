{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Functions for converting 'DAG' to graph data types defined in hsc3.
module Sound.SC3.UGen.Jiffy.Builder.Convert
  ( dag_to_U_Graph
  , dag_to_Graphdef
  ) where

-- base
import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (groupBy, sortBy)

-- hashtable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HT

-- hosc
import Sound.OSC.Datum (ascii)

-- hsc3
import Sound.SC3 (K_Type(..), Special(..))
import Sound.SC3.Server.Graphdef (Graphdef(..), Control, Input(..), UGen)
import Sound.SC3.UGen.Graph
  ( U_Graph(..), U_Node(..), ug_add_implicit, {- ug_pv_validate, -} )
import Sound.SC3.UGen.Type (Sample)

-- Internal
import Sound.SC3.UGen.Jiffy.Builder.GraphM

--
-- DAG to Graphdef
--

-- | Mapping from control node id to 'Input'.
newtype KTable s = KTable (HT.HashTable s Int Input)

-- XXX: Count the number of local buf ugens while building the synthdef
-- graph, to add 'MaxLocalBuf'.

dag_to_Graphdef :: String -> DAG s -> ST s Graphdef
dag_to_Graphdef name dag = do
  cs <- sortPairs <$> foldBM acc_constants [] (cmap dag)
  kt <- sizeBM (kmap dag) >>= emptyKTable
  gnks <- sortByKType <$> toListBM (kmap dag)
  let ks = zipWith g_node_k_to_control [0..] gnks
  implicit_ugens <- update_ktable kt gnks
  let shift = length implicit_ugens
  us <- sortPairs <$> foldBM (acc_ugens kt shift) [] (umap dag)
  return (Graphdef {graphdef_name=ascii name
                   ,graphdef_constants=cs
                   ,graphdef_controls=ks
                   ,graphdef_ugens=implicit_ugens ++ us})
{-# INLINABLE dag_to_Graphdef #-}

emptyKTable :: Int -> ST s (KTable s)
emptyKTable size = KTable <$> H.newSized size
{-# INLINE emptyKTable #-}

sortPairs :: Ord k => [(k, a)] -> [a]
sortPairs = snd . unzip . sortBy (compare `on` fst)
{-# INLINE sortPairs #-}

acc_constants :: [(a, Sample)] -> (a, G_Node) -> ST s [(a, Sample)]
acc_constants acc (k,n) = return ((k,g_node_c_value n) : acc)
{-# INLINE acc_constants #-}

update_ktable :: KTable s -> [(Int, G_Node)] -> ST s [UGen]
update_ktable (KTable table) controls = do
  let grouped_controls = groupByKType controls
      insert_controls ugen_index nodes =
        zipWithM_ (insert_control ugen_index) [0..] nodes
      insert_control ugen_index local_index (nid,_) =
        H.insert table nid (Input ugen_index local_index)
  zipWithM_ insert_controls [0..] grouped_controls
  return (to_control_ugens grouped_controls)
{-# INLINE update_ktable #-}

to_control_ugens :: [[(Int, G_Node)]] -> [UGen]
to_control_ugens = reverse . snd . foldl' f z
  where
    z = (0,[])
    f (idx, acc) igns =
      let acc' = case igns of
                   (_,g):_ -> g2u g : acc
                   _       -> acc
          g2u g =
            case g_node_k_type g of
              K_IR -> ug "Control" 0
              K_KR -> ug "Control" 1
              K_TR -> ug "TrigControl" 1
              K_AR -> ug "AudioControl" 2
          n_outs = length igns
          ug n r = (ascii n,r,[],replicate n_outs r,idx)
      in  (idx+n_outs, acc')
{-# INLINE to_control_ugens #-}

sortByKType :: [(a, G_Node)] -> [(a, G_Node)]
sortByKType = sortBy (compare `on` (g_node_k_type . snd))
{-# INLINE sortByKType #-}

groupByKType :: [(a, G_Node)] -> [[(a, G_Node)]]
groupByKType = groupBy ((==) `on` (g_node_k_type . snd))
{-# INLINE groupByKType #-}

g_node_k_to_control :: Int -> (a, G_Node) -> (Control, Sample)
g_node_k_to_control i (_, n) =
  case n of
    G_Node_K {..} -> ((ascii g_node_k_name, i), g_node_k_default)
    _ -> error ("g_node_k_to_control: non control node " ++ show n)
{-# INLINE g_node_k_to_control #-}

acc_ugens :: KTable s
          -> Int
          -> [(a, UGen)]
          -> (a, G_Node)
          -> ST s [(a, UGen)]
acc_ugens pm shift acc (k,n) = do
  let name_bs = ascii (g_node_u_name n)
      rate = fromEnum (g_node_u_rate n)
      outs = map fromEnum (g_node_u_outputs n)
      Special sp = g_node_u_special n
  ins <- convert_inputs pm shift (g_node_u_inputs n)
  return ((k, (name_bs,rate,ins,outs,sp)) : acc)
{-# INLINE acc_ugens #-}

convert_inputs :: KTable s -> Int -> [NodeId] -> ST s [Input]
convert_inputs (KTable table) shift = mapM f
  where
    f nid =
      case nid of
        NodeId_U k   -> let !k' = k+shift in return $! Input k' 0
        NodeId_P k p -> let !k' = k+shift in return $! Input k' p
        NodeId_C k   -> return $! Input (-1) k
        NodeId_K n _ -> do
          mb_input <- H.lookup table n
          case mb_input of
            Just input -> return input
            Nothing    -> error ("convert_input: missing " ++ show nid)
        NConstant v  -> error ("convert_input: raw constant " ++ show v)
{-# INLINE convert_inputs #-}

--
-- DAG to U_Graph
--

dag_to_U_Graph :: DAG s -> ST s U_Graph
dag_to_U_Graph (DAG cm km um) = do
  -- XXX: Shift node id of control and UGen input in 'gnode_to_unode'.
  let work o1 bm =
        sortBy (compare `on` u_node_id) <$> foldBM (g o1) [] bm
      g o1 acc (k,v) =
        return (gnode_to_unode (k+o1) v : acc)

  cs <- work 0 cm
  nc <- sizeBM cm

  ks <- work 0 km
  nk <- sizeBM km

  us <- work 0 um
  nu <- sizeBM um

  let graph = U_Graph {ug_next_id=count
                      ,ug_constants=cs
                      ,ug_controls=ks
                      ,ug_ugens=us}
      count = nc + nk + nu + 1
  return (ug_add_implicit graph)
{-# INLINABLE dag_to_U_Graph #-}

gnode_to_unode :: Int -> G_Node -> U_Node
gnode_to_unode nid node =
  case node of
    G_Node_C {..} ->
      U_Node_C {u_node_id=nid
               ,u_node_c_value=g_node_c_value}
    G_Node_K {..} ->
      U_Node_K {u_node_id=nid
               ,u_node_k_rate=g_node_k_rate
               ,u_node_k_index=g_node_k_index
               ,u_node_k_name=g_node_k_name
               ,u_node_k_default=g_node_k_default
               ,u_node_k_type=g_node_k_type
               ,u_node_k_meta=Nothing}
    G_Node_U {..} ->
      U_Node_U {u_node_id=nid
               ,u_node_u_rate=g_node_u_rate
               ,u_node_u_name=g_node_u_name
               ,u_node_u_inputs=map nid_to_port g_node_u_inputs
               ,u_node_u_outputs=g_node_u_outputs
               ,u_node_u_special=g_node_u_special
               ,u_node_u_ugenid=g_node_u_ugenid}
{-# INLINE gnode_to_unode #-}

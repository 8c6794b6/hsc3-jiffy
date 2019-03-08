{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Functions for converting 'DAG' to graph data types defined in hsc3.
module Sound.SC3.UGen.Jiffy.Builder.Convert
  ( dag_to_U_Graph
  , dag_to_Graphdef
  ) where

-- base
import Control.Monad (zipWithM_, when)
import Control.Monad.ST (ST)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.STRef (modifySTRef', readSTRef)

-- array
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)

-- hashtable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HT

-- hosc
import Sound.OSC.Datum (ascii)

-- hsc3
import Sound.SC3 (K_Type(..), Special(..))
import Sound.SC3.Server.Graphdef (Graphdef(..), Control, Input(..), UGen)
import Sound.SC3.UGen.Graph
  ( From_Port(..), U_Graph(..), U_Node(..)
  , ug_add_implicit, {- ug_pv_validate, -} )
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
dag_to_Graphdef name (DAG cm km um) = do
  cs <- sortPairs <$> foldBM acc_constants [] cm
  kt <- sizeBM km >>= emptyKTable
  gnks <- sortByKType <$> toListBM km
  let ks = zipWith g_node_k_to_control [0..] gnks
  implicit_ugens <- update_ktable kt gnks
  let shift = length implicit_ugens
  _ <- eliminate_dead_code um
  us <- sortPairs <$> foldBM (acc_ugens kt shift) [] um
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
  lni <- eliminate_dead_code um
  nc <- sizeBM cm
  nk <- sizeBM km
  nu <- sizeBM um
  let work bm =
        sortBy (compare `on` u_node_id) <$> foldBM g [] bm
      g acc (k,v) = do
        unode <- gnode_to_unode k nc (nc+nk) lni v
        return (unode:acc)
  cs <- work cm
  ks <- work km
  us <- work um
  let graph = U_Graph {ug_next_id=count
                      ,ug_constants=cs
                      ,ug_controls=ks
                      ,ug_ugens=us}
      count = nc + nk + nu
  return (ug_add_implicit graph)
{-# INLINABLE dag_to_U_Graph #-}

gnode_to_unode :: Int -> Int -> Int -> LNI s -> G_Node -> ST s U_Node
gnode_to_unode nid kshift ushift lni node =
  case node of
    G_Node_C {..} ->
      return (U_Node_C {u_node_id=nid
                       ,u_node_c_value=g_node_c_value})
    G_Node_K {..} ->
      return (U_Node_K {u_node_id=nid+kshift
                       ,u_node_k_rate=g_node_k_rate
                       ,u_node_k_index=g_node_k_index
                       ,u_node_k_name=g_node_k_name
                       ,u_node_k_default=g_node_k_default
                       ,u_node_k_type=g_node_k_type
                       ,u_node_k_meta=Nothing})
    G_Node_U {..} -> do
      mb_new_nid <- lookupLNI lni nid
      let n2f = nid_to_port kshift ushift
      case mb_new_nid of
        Just new_nid ->
          return (U_Node_U {u_node_id=new_nid+ushift
                           ,u_node_u_rate=g_node_u_rate
                           ,u_node_u_name=g_node_u_name
                           ,u_node_u_inputs=map n2f g_node_u_inputs
                           ,u_node_u_outputs=g_node_u_outputs
                           ,u_node_u_special=g_node_u_special
                           ,u_node_u_ugenid=g_node_u_ugenid})
        Nothing -> error ("gnode_to_unode: bad id " ++ show nid)
{-# INLINE gnode_to_unode #-}

nid_to_port :: Int -> Int -> NodeId -> From_Port
nid_to_port kshift ushift nid =
  case nid of
    NodeId_C k   -> From_Port_C k
    NodeId_K k t -> From_Port_K (k+kshift) t
    NodeId_U k   -> From_Port_U (k+ushift) Nothing
    NodeId_P k p -> From_Port_U (k+ushift) (Just p)
    NConstant v  -> error ("nid_to_port: constant " ++ show v)
{-# INLINE nid_to_port #-}

--
-- Dead code elimination
--

-- | Live node index, from old ID to new ID.
type LNI s = STUArray s Int Int

newLNI :: Int -> ST s (LNI s)
newLNI size = newArray (0,size) (-1)
{-# INLINE newLNI #-}

insertLNI :: LNI s -> Int -> Int -> ST s ()
insertLNI = writeArray
{-# INLINE insertLNI #-}

lookupLNI :: LNI s -> Int -> ST s (Maybe Int)
lookupLNI lni k = do
  n <- readArray lni k
  if n < 0
     then return Nothing
     else return $! Just n
{-# INLINE lookupLNI #-}

-- | Perform dead code elimination for the key-to-value hashtable of
-- 'BiMap'.
eliminate_dead_code :: BiMap s G_Node -> ST s (LNI s)
eliminate_dead_code (BiMap ref _ kt) = do
  size <- readSTRef ref
  lni <- newLNI size
  H.mapM_ (collect_live_id lni) kt
  n_removed <- remove_unused_nodes lni kt size
  when (0 < n_removed)
       (do H.mapM_ (update_input_index lni kt) kt
           modifySTRef' ref (\x -> x - n_removed))
  return lni
{-# INLINABLE eliminate_dead_code #-}

collect_live_id :: LNI s -> (a, G_Node) -> ST s ()
collect_live_id lni (_,gn) = mapM_ insert_id (g_node_u_inputs gn)
  where
    insert_id nid =
      case nid of
        NodeId_U k   -> insertLNI lni k 0
        NodeId_P k _ -> insertLNI lni k 0
        _            -> return ()
{-# INLINE collect_live_id #-}

remove_unused_nodes :: LNI s -> BiMapKT s G_Node -> Int -> ST s Int
remove_unused_nodes lni kt stop = go 0 0
  where
    -- Looping through BiMapKT with index `k::Int', because the key
    -- values of 'BiMapKT' is not guaranteed to be sorted, so updating
    -- live code index table with counting the number of removed nodes
    -- may not work with HashTables's `foldM'.
    go n_removed !k
      | stop <= k = return n_removed
      | otherwise = do
        mb_found <- lookupLNI lni k
        case mb_found of
          Just _  -> do
            insertLNI lni k (k-n_removed)
            go n_removed (k+1)
          Nothing -> do
            mb_gn <- H.lookup kt k
            case mb_gn of
              Just gn | not (null (g_node_u_outputs gn)) -> do
                H.delete kt k
                let !n_removed' = n_removed + 1
                go n_removed' (k+1)
              _ -> do
                insertLNI lni k (k-n_removed)
                go n_removed (k+1)
{-# INLINE remove_unused_nodes #-}

update_input_index :: LNI s -> BiMapKT s G_Node
                   -> (Int,G_Node) -> ST s ()
update_input_index lni kt (!k,gn) =
  case gn of
    G_Node_U {..} -> do
      inputs' <- mapM lookup_nid g_node_u_inputs
      H.insert kt k $! gn {g_node_u_inputs=inputs'}
    _ -> return ()
  where
    lookup_nid nid =
      case nid of
        NodeId_U old -> do
          mb_new <- lookupLNI lni old
          case mb_new of
            Just new -> return (NodeId_U new)
            Nothing  -> error ("lookup_nid: missing " ++ show old)
        NodeId_P old p -> do
          mb_new <- lookupLNI lni old
          case mb_new of
            Just new -> return (NodeId_P new p)
            Nothing -> error ("lookup_nid: missing" ++ show old)
        _ -> return nid
{-# INLINE update_input_index #-}

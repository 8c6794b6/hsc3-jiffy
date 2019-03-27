{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hashtable
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HT

-- hosc
import Sound.OSC.Datum (ascii)

-- hsc3
import Sound.SC3
  ( Binary(..), K_Type(..), Rate(..), Special(..), Unary(..) )
import Sound.SC3.Server.Graphdef
  ( Graphdef(..), Control, Input(..), UGen )
import Sound.SC3.UGen.Graph
  ( From_Port(..), U_Graph(..), U_Node(..)
  , ug_add_implicit, {- ug_pv_validate, -} )
import Sound.SC3.UGen.Type (Sample)

-- Internal
import Sound.SC3.UGen.Jiffy.Builder.GraphM


-- ------------------------------------------------------------------------
--
-- DAG to Graphdef
--
-- ------------------------------------------------------------------------

-- | Mapping from control node id to 'Input'.
newtype KTable s = KTable (HT.HashTable s Int Input)

dag_to_Graphdef :: String -> DAG s -> ST s Graphdef
dag_to_Graphdef name dag@(DAG nbufs_ref _ cm km um) = do
  -- Optimize the UGen graph
  _ <- optimize_graph dag

  -- Get the number of LocalBuf UGens.
  nbufs <- readSTRef nbufs_ref
  let has_local_buf = 0 < nbufs

  -- Make control UGens, and a lookup table for it.
  kt <- sizeBM km >>= emptyKTable
  gnks <- sortByKType <$> toListBM km
  control_ugens <- update_ktable kt has_local_buf gnks

  -- Add MaxLocalBuf to implicitly added UGens. MaxLocalBuf may add a
  -- new constant value, so let it add before accumulating constants.
  let get_implicit_ugens
        | has_local_buf = (: control_ugens) <$> mkMaxLocalBufs nbufs cm
        | otherwise     = return control_ugens
  implicit_ugens <- get_implicit_ugens

  -- Accumulate the Graphdef fields ...
  cs <- sortPairs <$> foldBM acc_constants [] cm
  let ks = zipWith g_node_k_to_control [0..] gnks
      shift | has_local_buf = length control_ugens + 1
            | otherwise     = length control_ugens
  us <- sortPairs <$> foldBM (acc_ugens kt shift) [] um

  return (Graphdef {graphdef_name=ascii name
                   ,graphdef_constants=cs
                   ,graphdef_controls=ks
                   ,graphdef_ugens=implicit_ugens <> us})
{-# INLINABLE dag_to_Graphdef #-}

mkMaxLocalBufs :: Int -> BiMap s G_Node -> ST s UGen
mkMaxLocalBufs nbufs cm = do
  nid <- hashconsC' cm (fromIntegral nbufs)
  let i = Input (-1) nid
  return $! ("MaxLocalBufs",0,[i],[],0)
{-# INLINE mkMaxLocalBufs #-}

emptyKTable :: Int -> ST s (KTable s)
emptyKTable size = KTable <$> H.newSized size
{-# INLINE emptyKTable #-}

sortPairs :: Ord k => [(k, a)] -> [a]
sortPairs = map snd . sortBy (compare `on` fst)
{-# INLINE sortPairs #-}

acc_constants :: [(a, Sample)] -> (a, G_Node) -> ST s [(a, Sample)]
acc_constants acc (k,n) = return ((k,g_node_c_value n) : acc)
{-# INLINE acc_constants #-}

update_ktable :: KTable s -> Bool -> [(Int, G_Node)] -> ST s [UGen]
update_ktable (KTable table) hasLocalBufs controls = do
  let grouped_controls = groupByKType controls
      insert_controls ugen_index =
        zipWithM_ (insert_control ugen_index) [0..]
      insert_control ugen_index local_index (nid,_) =
        H.insert table nid (Input ugen_index local_index)
      control_ugen_ids
        | hasLocalBufs = [1..]
        | otherwise    = [0..]
  zipWithM_ insert_controls control_ugen_ids grouped_controls
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
          ug n r = (n,r,[],replicate n_outs r,idx)
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
    G_Node_K {..} -> ((g_node_k_name, i), g_node_k_default)
    _ -> error ("g_node_k_to_control: non control node " ++ show n)
{-# INLINE g_node_k_to_control #-}

acc_ugens :: KTable s
          -> Int
          -> [(a, UGen)]
          -> (a, G_Node)
          -> ST s [(a, UGen)]
acc_ugens kt shift acc (k,n) = do
  let name_bs = g_node_u_name n
      rate = fromEnum (g_node_u_rate n)
      outs = map fromEnum (g_node_u_outputs n)
      Special sp = g_node_u_special n
  ins <- convert_inputs kt shift (g_node_u_inputs n)
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

-- ------------------------------------------------------------------------
--
-- DAG to U_Graph
--
-- ------------------------------------------------------------------------

dag_to_U_Graph :: DAG s -> ST s U_Graph
dag_to_U_Graph dag@(DAG _ _ cm km um) = do
  lni <- optimize_graph dag
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
                       ,u_node_k_name=unpack g_node_k_name
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
                           ,u_node_u_name=unpack g_node_u_name
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


-- ------------------------------------------------------------------------
--
-- UGen graph optimization
--
-- ------------------------------------------------------------------------

-- | Descendant node index, from ID to number of descendant nodes.
newtype DNI s = DNI (STUArray s Int Int)

newDNI :: Int -> ST s (DNI s)
newDNI size = DNI <$> newArray (0,size) 0
{-# INLINE newDNI #-}

incrDNI :: DNI s -> Int -> ST s ()
incrDNI (DNI arr) k = readArray arr k >>= writeArray arr k . succ
{-# INLINE incrDNI #-}

resetDNI :: DNI s -> Int -> ST s ()
resetDNI (DNI arr) k = writeArray arr k 0
{-# INLINE resetDNI #-}

readDNI :: DNI s -> Int -> ST s Int
readDNI (DNI arr) = readArray arr
{-# INLINE readDNI #-}

-- | Live node index, from old ID to new ID.
newtype LNI s = LNI (STUArray s Int Int)

dni2lni :: Int -> DNI s -> ST s (LNI s)
dni2lni size (DNI dni) = go dni 0 >> return (LNI dni)
  where
    go arr !n
      | size <= n = return ()
      | otherwise = do
        num_descs <- readArray arr n
        writeArray arr n (if 0 < num_descs then 0 else (- 1))
        go arr (n+1)
{-# INLINE dni2lni #-}

insertLNI :: LNI s -> Int -> Int -> ST s ()
insertLNI (LNI arr) = writeArray arr
{-# INLINE insertLNI #-}

lookupLNI :: LNI s -> Int -> ST s (Maybe Int)
lookupLNI (LNI arr) k = do
  n <- readArray arr k
  if n < 0
     then return Nothing
     else return $! Just n
{-# INLINE lookupLNI #-}

optimize_graph :: DAG s -> ST s (LNI s)
optimize_graph dag@(DAG {umap=bimap}) = do
  size <- sizeBM bimap
  dni <- newDNI size
  count_descendants size dni bimap
  optimize_binops dni dag
  lni <- dni2lni size dni
  eliminate_dead_code size lni bimap
{-# INLINABLE optimize_graph #-}

count_descendants :: Int -> DNI s -> BiMap s G_Node -> ST s ()
count_descendants size dni (BiMap _ _ kt) = go 0
  where
    go !n
      | size <= n = return ()
      | otherwise = do
        mb_node <- H.lookup kt n
        case mb_node of
          Just (G_Node_U {..}) -> mapM_ incr g_node_u_inputs
          _                    -> return ()
        go (n+1)
    incr nid =
      case nid of
        NodeId_U k   -> incrDNI dni k
        NodeId_P k _ -> incrDNI dni k
        _            -> return ()
{-# INLINE count_descendants #-}

optimize_binops :: DNI s -> DAG s -> ST s ()
optimize_binops dni dag@(DAG _ om _ _ (BiMap _ _ vt)) = go
  where
    -- Sort the operator by node id, and perform node lookup for each
    -- operator. The fields in OpArgs data type are 'NodeId', so that
    -- 'Add' nodes could optimized to 'Sum3', and then the 'Sum3' nodes
    -- could optimized to 'Sum4', ... etc.
    go = do ops <- sortBy (compare `on` fst) <$> toListOM om
            mapM_ optimize_binop ops
    optimize_binop (k, oparg) =
      case oparg of
        AddArgs nid0 nid1 -> do
          n0 <- lookup_g_node' nid0 dag
          n1 <- lookup_g_node' nid1 dag
          let rate0 = g_node_rate n0
              rate1 = g_node_rate n1
          mbn <- optimize_add dag dni (post nid1) rate1 nid1 nid0 n0
          case mbn of
            Just node -> replace k nid0 node
            Nothing   -> do
              mbn' <- optimize_add dag dni (pre nid0) rate0 nid0 nid1 n1
              case mbn' of
                Just node' -> replace k nid1 node'
                Nothing    -> return ()
        SubArgs nid0 nid1 -> do
          n0 <- lookup_g_node' nid0 dag
          n1 <- lookup_g_node' nid1 dag
          let r0 = g_node_rate n0
          mbn <- optimize_sub dni r0 nid0 nid1 n1
          case mbn of
            Just node -> replace k nid1 node
            Nothing   -> return ()
    pre x xs = x : xs
    post x xs = xs ++ [x]
    replace k old_nid new_node = do
      resetDNI dni (nid_value old_nid)
      H.insert vt k new_node
{-# INLINE optimize_binops #-}

optimize_add :: DAG s -> DNI s -> ([NodeId] -> [NodeId])
             -> Rate -> NodeId -> NodeId -> G_Node
             -> ST s (Maybe G_Node)
optimize_add dag dni f other_rate other_nid nid gn =
  case gn of
    G_Node_U {..}
     | is_sum3_node gn ->
       dg (do let gn' = gn {g_node_u_name="Sum4"
                           ,g_node_u_inputs=f g_node_u_inputs
                           ,g_node_u_rate=rate}
              return $! Just gn')
     | is_add_node gn ->
       dg (do let gn' = gn {g_node_u_name="Sum3"
                           ,g_node_u_inputs=f g_node_u_inputs
                           ,g_node_u_special=Special 0
                           ,g_node_u_rate=rate}
              return $! Just gn')
     | is_mul_node gn ->
       -- MulAdd UGen has some constraints for its input arguments. See
       -- the "canBeMulAdd" class method defined in sclang source file
       -- "BasicOpUGen.sc".
       dg (do inputs <- mapM get_node_pair g_node_u_inputs
              case inputs of
                [(nid0,n0),(nid1,n1)]
                  | r0 == AR || (r0 == KR && other_rate_ok)
                  -> mul_add [nid0,nid1]
                  | r1 == AR || (r1 == KR && other_rate_ok)
                  -> mul_add [nid1,nid0]
                  where
                    r0 = g_node_rate n0
                    r1 = g_node_rate n1
                    other_rate_ok =
                      other_rate == IR || other_rate == KR
                _ -> return Nothing)
     | is_neg_node gn ->
       dg (do let gn' = gn {g_node_u_name="BinaryOpUGen"
                           ,g_node_u_inputs=other_nid:g_node_u_inputs
                           ,g_node_u_outputs=[rate]
                           ,g_node_u_special=Special (fromEnum Sub)}
              return $! Just gn')
    _ -> return Nothing
  where
    -- dg, do with descendants guard.
    dg m = do n_descs <- readDNI dni (nid_value nid)
              if n_descs == 1
                 then m
                 else return Nothing
    mul_add ins =
      let gn' = gn {g_node_u_name="MulAdd"
                   ,g_node_u_inputs=ins++[other_nid]
                   ,g_node_u_special=Special 0
                   ,g_node_u_rate=rate}
      in  return $! Just gn'
    get_node_pair i = do
      node <- lookup_g_node' i dag
      return (i, node)
    rate = max other_rate (g_node_rate gn)
{-# INLINE optimize_add #-}

optimize_sub :: DNI s -> Rate -> NodeId -> NodeId -> G_Node
             -> ST s (Maybe G_Node)
optimize_sub dni other_rate other_nid nid gn =
  case gn of
    G_Node_U {..}
      | is_neg_node gn -> do
        n_descs <- readDNI dni (nid_value nid)
        if n_descs == 1
           then let gn' = gn {g_node_u_name="BinaryOpUGen"
                             ,g_node_u_inputs=other_nid:g_node_u_inputs
                             ,g_node_u_outputs=[rate g_node_u_rate]
                             ,g_node_u_special=Special (fromEnum Add)}
                in  return $! Just gn'
           else return Nothing
    _ -> return Nothing
  where
    rate = max other_rate
{-# INLINE optimize_sub #-}

is_sum3_node :: G_Node -> Bool
is_sum3_node gn
  | G_Node_U {..} <- gn = "Sum3" == g_node_u_name
  | otherwise = False
{-# INLINE is_sum3_node #-}

is_neg_node :: G_Node -> Bool
is_neg_node gn
  | G_Node_U {..} <- gn
  , Special n <- g_node_u_special
  , n == fromEnum Neg
  = "UnaryOpUGen" == g_node_u_name
  | otherwise = False
{-# INLINE is_neg_node #-}

is_add_node :: G_Node -> Bool
is_add_node = is_binop_node Add
{-# INLINE is_add_node #-}

is_mul_node :: G_Node -> Bool
is_mul_node = is_binop_node Mul
{-# INLINE is_mul_node #-}

is_binop_node :: Binary -> G_Node -> Bool
is_binop_node op gn
  | G_Node_U {..} <- gn
  , Special n <- g_node_u_special
  , n == fromEnum op
  = "BinaryOpUGen" == g_node_u_name
  | otherwise = False
{-# INLINE is_binop_node #-}

-- | Perform dead code elimination for the key-to-value hashtable of
-- 'BiMap'.
eliminate_dead_code :: Int -> LNI s -> BiMap s G_Node -> ST s (LNI s)
eliminate_dead_code size lni (BiMap ref _ kt) = do
  n_removed <- remove_unused_nodes lni kt size
  when (0 < n_removed)
       (do H.mapM_ (update_input_index lni kt) kt
           modifySTRef' ref (\x -> x - n_removed))
  return lni
{-# INLINE eliminate_dead_code #-}

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
              Just (G_Node_U {..}) | g_node_u_pure -> do
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

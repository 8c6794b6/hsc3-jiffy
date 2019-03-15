{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Internal of UGen builder.
--
-- The implementation is heavily inspired from the presentation given by
-- Oleg Kiselyov, found in:
--
--    <http://okmij.org/ftp/tagless-final/sharing/index.html>
--
module Sound.SC3.UGen.Jiffy.Builder.GraphM
  (
    -- * Internal of synthdef builder
    GraphM

    -- * DAG
  , DAG(..)
  , emptyDAG
  , hashconsC
  , hashconsC'
  , hashconsK
  , hashconsU

  , incrementNumLocalBufs
  , registerOp

    -- * BiMap
  , BiMap(..)
  , BiMapVT
  , BiMapKT
  , foldBM
  , toListBM
  , sizeBM

    -- * OpMap
  , OpMap(..)
  , OpArg(..)
  , toListOM

    -- * Node and node ID
  , G_Node(..)
  , NodeId(..)
  , nid_value
  , lookup_g_node
  , lookup_g_node'
  , g_node_rate

    -- * MCE
  , MCE(..)
  , mce_extend
  , mce_degree
  , mce_max_degree
  , mce_list

    -- * Re-export
  , Name
  ) where

-- base
import Control.Monad.ST (ST)
import Data.Foldable (foldl')
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

-- hashable
import Data.Hashable (Hashable(..))

-- hashtables
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as HT

-- hsc3
import Sound.SC3
  ( Output, K_Type(..), Rate(..), Sample, Special(..), UGenId )
import Sound.SC3.Server.Graphdef (Name)

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)

-- Internal
import Sound.SC3.Jiffy.Orphan ()

--
-- DAG and its builder
--

-- | Type synonym for synthdef builder internal, 'ReaderT' transformer
-- with 'DAG' state.
type GraphM s a = ReaderT (DAG s) (ST s) a

-- | Data type of directed acyclic graph to construct synthdef.  The
-- 'BiMap's are separated between constants, control nodes, and ugen
-- nodes, to keep the hash table small. But the 'Int' value for
-- bookkeeping lookup-key of 'BiMap's is shared.
data DAG s =
  DAG { numLocalBufs :: {-# UNPACK #-} !(STRef s Int)
      , opmap :: {-# UNPACK #-} !(OpMap s)
      , cmap :: {-# UNPACK #-} !(BiMap s G_Node)
      , kmap :: {-# UNPACK #-} !(BiMap s G_Node)
      , umap :: {-# UNPACK #-} !(BiMap s G_Node) }

-- | Make a new empty 'DAG' in 'ST' monad, with heuristically selected
-- initial sizes for internal hash tables.
emptyDAG :: ST s (DAG s)
emptyDAG =
  DAG <$> newSTRef 0 <*> newOpMap 8 <*>
  empty 16 <*> empty 8 <*> empty 128

--
-- Simple bidirectional map
--

-- | Bidirectional map data structure with 'Int' key.
data BiMap s a =
  BiMap {-# UNPACK #-} !(STRef s Int)
        {-# UNPACK #-} !(BiMapVT s a)
        {-# UNPACK #-} !(BiMapKT s a)

-- | Value-to-key table in 'BiMap'.
type BiMapVT s a = HT.HashTable s a Int

-- | Key-to-value table in 'BiMap'.
type BiMapKT s a = HT.HashTable s Int a

-- | Create empty 'BiMap' with initial size.
empty :: Int -- ^ Initial size of hash tables.
      -> ST s (BiMap s a)
empty n = BiMap <$> newSTRef 0 <*> HC.newSized n <*> HC.newSized n
{-# INLINE empty #-}

-- | Lookup 'BiMap' with value to find a 'Int' key.
lookup_key :: (Hashable a, Eq a) => a -> BiMap s a -> ST s (Maybe Int)
lookup_key !v (BiMap _ m _) = HC.lookup m v
{-# INLINABLE lookup_key #-}
{-# SPECIALIZE lookup_key
  :: G_Node -> BiMap s G_Node -> ST s (Maybe Int) #-}

-- | Lookup 'Bimap' with 'Int' key to find a value.
lookup_val :: Int -> BiMap s a -> ST s a
lookup_val !k (BiMap _ _ m) = do
  ret <- HC.lookup m k
  case ret of
    Just v -> return v
    Nothing -> error "lookup_val: key does not exist."
{-# INLINABLE lookup_val #-}

-- | Insert given value to 'Bimap' with new key.
insert :: (Hashable a, Eq a) => a -> BiMap s a -> ST s Int
insert !v (BiMap ref vt kt) = do
  !k <- readSTRef ref
  HC.insert vt v k
  HC.insert kt k v
  let k' = k + 1
  k' `seq` writeSTRef ref k'
  return k
{-# INLINE insert #-}

-- | Fold 'BiMap' with 'Int' key and value.
foldBM :: (b -> (Int, a) -> ST s b) -> b -> BiMap s a -> ST s b
foldBM f z (BiMap _ _ kt) = HC.foldM f z kt
{-# INLINE foldBM #-}

-- | Convert 'Bimap' to list.
toListBM :: BiMap s a -> ST s [(Int, a)]
toListBM = foldBM (\as a -> pure (a:as)) []
{-# INLINE toListBM #-}

-- | Get number of elements stored in 'BiMap'.
sizeBM :: BiMap s a -> ST s Int
sizeBM (BiMap ref _ _) = readSTRef ref
{-# INLINE sizeBM #-}

--
-- Operator map, for synthdef graph optimization
--

-- | Operator map, from node id to 'OpArg'.
newtype OpMap s = OpMap (HT.HashTable s Int OpArg)

-- | Operator arguments.
data OpArg
  = AddArgs !NodeId !NodeId
  -- ^ Arguments of binary operator 'Add'.
  | SubArgs !NodeId !NodeId
  -- ^ Arguments of binary operator 'Sub'.
  deriving (Eq, Show)

newOpMap :: Int -> ST s (OpMap s)
newOpMap size = OpMap <$> HC.newSized size
{-# INLINE newOpMap #-}

toListOM :: OpMap s -> ST s [(Int,OpArg)]
toListOM (OpMap om) = HC.toList om
{-# INLINE toListOM #-}

insertOpArg :: OpMap s -> Int -> OpArg -> ST s ()
insertOpArg (OpMap om) !k !v = HC.insert om k v
{-# INLINE insertOpArg #-}


--
-- Node and node ID
--

-- | Data type to represent UGen node in a 'DAG' graph. This data is
-- intended to be converted to 'U_Node' with key values from current
-- graph used for building synthdef. All fields in all constructors are
-- strict, and some of them are unpacked.
data G_Node
  = G_Node_C { g_node_c_value :: {-# UNPACK #-} !Sample }
  -- ^ Constant node, for 'U_Node_C'.
  | G_Node_K { g_node_k_rate :: !Rate
             , g_node_k_index :: !(Maybe Int)
             , g_node_k_name :: !Name
             , g_node_k_default :: {-# UNPACK #-} !Sample
             , g_node_k_type :: !K_Type }
  -- ^ Control node, for 'U_Node_K'.
  | G_Node_U { g_node_u_rate :: !Rate
             , g_node_u_name :: !Name
             , g_node_u_inputs :: [NodeId]
             , g_node_u_outputs :: [Output]
             , g_node_u_special :: {-# UNPACK #-} !Special
             , g_node_u_ugenid :: !UGenId }
  -- ^ UGen node, for 'U_Node_U'.
  --
  -- Note that there is no constructor for proxy node because proxy
  -- nodes are not stored in synthdef graph. Instead, output proxies are
  -- expressed with 'NodeId' data type.
  deriving (Eq, Show)

instance Hashable G_Node where
  -- Not marking with optional constructor index value, because DAG
  -- separates BiMap fields for each constructor.
  hashWithSalt s n =
    case n of
      G_Node_C {..} -> s `hashWithSalt`
                       g_node_c_value
      G_Node_K {..} -> s `hashWithSalt`
                       g_node_k_rate `hashWithSalt`
                       g_node_k_index `hashWithSalt`
                       g_node_k_name `hashWithSalt`
                       g_node_k_default `hashWithSalt`
                       g_node_k_type
      G_Node_U {..} -> s `hashWithSalt`
                       g_node_u_rate `hashWithSalt`
                       g_node_u_name `hashWithSalt`
                       g_node_u_inputs `hashWithSalt`
                       g_node_u_outputs `hashWithSalt`
                       g_node_u_special `hashWithSalt`
                       g_node_u_ugenid
  {-# INLINE hashWithSalt #-}

-- | Data type to represent UGen graph node id for inputs and outputs.
data NodeId
  = NodeId_C {-# UNPACK #-} !Int
  -- ^ Constant node.
  | NodeId_K {-# UNPACK #-} !Int !K_Type
  -- ^ Control node.
  | NodeId_U {-# UNPACK #-} !Int
  -- ^ UGen node.
  | NodeId_P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  -- ^ Proxy node, which is a UGen node with optional 'Int' value to
  -- keep track of output index for multi-channeled node.
  | NConstant {-# UNPACK #-} !Sample
  -- ^ Constant value not yet stored in DAG. This constructor is used
  -- for constant folding in unary and binary operator functions.
  deriving (Eq, Show)

instance Hashable NodeId where
  -- Like in 'G_Node', not marking with optional constructor index
  -- value.
  hashWithSalt s n =
    case n of
      NodeId_C k   -> s `hashWithSalt` k
      NodeId_K k t -> s `hashWithSalt` k `hashWithSalt` t
      NodeId_U k   -> s `hashWithSalt` k
      NodeId_P k p -> s `hashWithSalt` k `hashWithSalt` p
      NConstant v  -> s `hashWithSalt` v
  {-# INLINE hashWithSalt #-}

--
-- MCE, recursivly nested
--

-- | Recursive data type for multi channel expansion.
data MCE a
  = MCEU !a
  | MCEV {-# UNPACK #-} !Int -- ^ Number of channels.
         [MCE a]             -- ^ Channel contents.
  deriving (Eq, Show)

instance Functor MCE where
  fmap f = go
    where
      go (MCEU a) = MCEU (f a)
      go (MCEV n as) = MCEV n (fmap go as)
  {-# INLINE fmap #-}

instance Foldable MCE where
  foldMap f = go
    where
      go (MCEU a) = f a
      go (MCEV _ as) = foldMap go as
  {-# INLINE foldMap #-}

instance Traversable MCE where
  traverse f = go
    where
      go (MCEU a) =  MCEU <$> f a
      go (MCEV n as) = MCEV n <$> traverse go as
  {-# INLINE traverse #-}

mce_extend :: Int -> MCE a -> MCE a
mce_extend !n m =
  case m of
    MCEU _ -> MCEV n (replicate n m)
    MCEV n' xs | n' == n   -> m
               | otherwise -> MCEV n (take n (cycle xs))
{-# INLINE mce_extend #-}

mce_degree :: MCE a -> Int
mce_degree m =
  case m of
    MCEU _   -> 1
    MCEV n _ -> n
{-# INLINE mce_degree #-}

mce_max_degree :: [MCE a] -> Int
mce_max_degree = foldl' (\c m -> max (mce_degree m) c) 1
{-# INLINE mce_max_degree #-}

mce_list :: MCE a -> [MCE a]
mce_list m =
  case m of
    MCEU _    -> [m]
    MCEV _ xs -> xs
{-# INLINE mce_list #-}

--
-- Auxiliary functions for NodeId, DAG, and G_Node
--

nid_value :: NodeId -> Int
nid_value nid =
  case nid of
    NodeId_C i   -> i
    NodeId_K i _ -> i
    NodeId_U i   -> i
    NodeId_P i _ -> i
    NConstant v  -> error ("nid_value: constant " ++ show v)
{-# INLINE nid_value #-}

lookup_g_node :: NodeId -> DAG s -> GraphM s G_Node
lookup_g_node nid dag = lift (lookup_g_node' nid dag)
{-# INLINE lookup_g_node #-}

lookup_g_node' :: NodeId -> DAG s -> ST s G_Node
lookup_g_node' nid dag =
  case nid of
    NodeId_C k   -> lookup_val k (cmap dag)
    NodeId_K k _ -> lookup_val k (kmap dag)
    NodeId_U k   -> lookup_val k (umap dag)
    NodeId_P k _ -> lookup_val k (umap dag)
    NConstant v  -> error ("lookup_g_node: constant " ++ show v)
{-# INLINE lookup_g_node' #-}

g_node_rate :: G_Node -> Rate
g_node_rate n =
  case n of
    G_Node_C {} -> IR
    G_Node_K {} -> g_node_k_rate n
    G_Node_U {} -> g_node_u_rate n
{-# INLINE g_node_rate #-}

--
-- Synthdef construction helpers
--

incrementNumLocalBufs :: DAG s -> GraphM s ()
incrementNumLocalBufs dag =
  lift (modifySTRef' (numLocalBufs dag) (+1))
{-# INLINE incrementNumLocalBufs #-}

registerOp :: DAG s -> NodeId -> OpArg -> GraphM s NodeId
registerOp (DAG _ opmap _ _ _) me oparg = do
  lift (insertOpArg opmap (nid_value me) oparg)
  return me
{-# INLINE registerOp #-}

--
-- Hash-consing
--

hashcons :: (Int -> NodeId)
         -> (DAG s -> BiMap s G_Node)
         -> G_Node
         -> GraphM s NodeId
hashcons con prj !x = do
  bimap <- prj <$> ask
  v <- lift (lookup_key x bimap)
  case v of
    Nothing -> lift (con <$> insert x bimap)
    Just k  -> return (con k)
{-# INLINE hashcons #-}

hashconsC :: G_Node -> GraphM s NodeId
hashconsC g = hashcons NodeId_C cmap g
{-# INLINABLE hashconsC #-}

hashconsC' :: BiMap s G_Node -> Sample -> ST s Int
hashconsC' bimap !smpl = do
  let x = G_Node_C smpl
  v <- lookup_key x bimap
  case v of
    Nothing -> insert x bimap
    Just k  -> return k
{-# INLINABLE hashconsC' #-}

hashconsK :: G_Node -> GraphM s NodeId
hashconsK g = hashcons (flip NodeId_K (g_node_k_type g)) kmap g
{-# INLINABLE hashconsK #-}

hashconsU :: G_Node -> GraphM s NodeId
hashconsU g = hashcons NodeId_U umap g
{-# INLINABLE hashconsU #-}

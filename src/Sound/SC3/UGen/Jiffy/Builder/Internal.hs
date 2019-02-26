{-# LANGUAGE RecordWildCards #-}
-- | Internal of UGen builder.
module Sound.SC3.UGen.Jiffy.Builder.Internal
  ( -- * DAG
    DAG(..)
  , BiMap(..)
  , emptyDAG
  , hashconsC
  , hashconsK
  , hashconsU

    -- * Node and node ID
  , G_Node(..)
  , NodeId(..)
  , nid_value
  , nid_to_port
  , lookup_g_node
  , g_node_rate

    -- * MCE
  , MCE(..)
  , mce_extend
  , mce_degree
  , mce_list
  , is_mce_vector
  ) where

-- base
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- hashable
import Data.Hashable (Hashable(..))

-- hashtables
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as HT

-- hsc3
import Sound.SC3
  ( Output, K_Type(..), Rate(..), Sample, Special(..), UGenId )
import Sound.SC3.UGen.Graph (From_Port(..))

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)

-- Internal
import Sound.SC3.Jiffy.Orphan ()

-- | Data type of directed acyclic graph to construct synthdef.  The
-- 'BiMap's are separated between constants, control nodes, and ugen
-- nodes, to keep the hash table small. But the 'Int' value for
-- bookkeeping lookup-key of 'BiMap's is shared.
data DAG s =
  DAG { hashcount :: {-# UNPACK #-} !(STRef s Int)
      , cmap :: {-# UNPACK #-} !(BiMap s G_Node)
      , kmap :: {-# UNPACK #-} !(BiMap s G_Node)
      , umap :: {-# UNPACK #-} !(BiMap s G_Node) }

instance Show (DAG s) where
  show _ = "<DAG>"

-- | Make a new empty 'DAG' in 'ST' monad, with heuristically selected
-- initial sizes for internal hash tables.
emptyDAG :: ST s (DAG s)
emptyDAG = DAG <$> newSTRef 0 <*> empty 16 <*> empty 8 <*> empty 128

--
-- Simple bidirectional map
--

-- | Bidirectional map data structure with 'Int' key.
data BiMap s a =
  BiMap {-# UNPACK #-} !(HT.HashTable s a Int)
        {-# UNPACK #-} !(HT.HashTable s Int a)

-- | Create empty 'BiMap' with initial size.
empty :: Int -- ^ Initial size of hash tables.
      -> ST s (BiMap s a)
empty n = BiMap <$> HC.newSized n <*> HC.newSized n
{-# INLINABLE empty #-}

-- | Lookup 'BiMap' with value to find a 'Int' key.
lookup_key :: (Hashable a, Eq a) => a -> BiMap s a -> ST s (Maybe Int)
lookup_key v (BiMap m _) = HC.lookup m v
{-# INLINABLE lookup_key #-}
{-# SPECIALIZE lookup_key
  :: G_Node -> BiMap s G_Node -> ST s (Maybe Int) #-}

-- | Lookup 'Bimap' with 'Int' key to find a value.
lookup_val :: Int -> BiMap s a -> ST s a
lookup_val k (BiMap _ m) = do
  ret <- HC.lookup m k
  case ret of
    Just v -> return v
    Nothing -> error "lookup_val: key does not exist."
{-# INLINABLE lookup_val #-}

-- | Insert new element to 'Bimap' with given value and key.
insert_at :: (Hashable a, Eq a) => a -> Int -> BiMap s a -> ST s ()
insert_at v k (BiMap vt kt) = HC.insert vt v k >> HC.insert kt k v
{-# INLINABLE insert_at #-}
{-# SPECIALIZE insert_at
  :: G_Node -> Int -> BiMap s G_Node -> ST s () #-}

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
             , g_node_k_name :: !String
             , g_node_k_default :: {-# UNPACK #-} !Sample
             , g_node_k_type :: !K_Type }
  -- ^ Control node, for 'U_Node_K'.
  | G_Node_U { g_node_u_rate :: !Rate
             , g_node_u_name :: !String
             , g_node_u_inputs :: ![NodeId]
             , g_node_u_outputs :: ![Output]
             , g_node_u_special :: {-# UNPACK #-} !Special
             , g_node_u_ugenid :: !UGenId }
  -- ^ UGen node, for 'U_Node_U'.
  --
  -- Note that there is no constructor for proxy node because proxy
  -- nodes are not stored in synthdef graph. Instead, output proxies are
  -- expressed with 'NodeId' data type.
  deriving (Eq, Show)

instance Hashable G_Node where
  hashWithSalt s n =
    case n of
      G_Node_C {..} -> s `hashWithSalt`
                       ci `hashWithSalt`
                       g_node_c_value
      G_Node_K {..} -> s `hashWithSalt`
                       ck `hashWithSalt`
                       g_node_k_rate `hashWithSalt`
                       g_node_k_index `hashWithSalt`
                       g_node_k_name `hashWithSalt`
                       g_node_k_default `hashWithSalt`
                       g_node_k_type
      G_Node_U {..} -> s `hashWithSalt`
                       cu `hashWithSalt`
                       g_node_u_rate `hashWithSalt`
                       g_node_u_name `hashWithSalt`
                       g_node_u_inputs `hashWithSalt`
                       g_node_u_outputs `hashWithSalt`
                       g_node_u_special `hashWithSalt`
                       g_node_u_ugenid
    where
      ci, ck, cu :: Int
      (ci,ck,cu) = (0,1,2)
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
  deriving (Eq, Ord, Show)

instance Hashable NodeId where
  -- Not marking each constructor with optional constructor index value,
  -- because DAG separates BiMap fields for each constructor.
  hashWithSalt s n =
    case n of
      NodeId_C k -> s `hashWithSalt` k
      NodeId_K k t -> s `hashWithSalt` k `hashWithSalt` t
      NodeId_U k -> s `hashWithSalt` k
      NodeId_P k p -> s `hashWithSalt` k `hashWithSalt` p
      NConstant v -> s `hashWithSalt` v
  {-# INLINE hashWithSalt #-}

--
-- MCE, recursivly nested
--

-- Note [Recursively nested MCE vectors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- 'Sound.SC3.UGen.MCE.MCE' data type cannot express nested list
-- (a.k.a. Rose tree), so 'Sound.SC3.UGen.Type.UGen' data type has
-- recursive constructor 'MCE_U', to express nested MCE values such as:
--
--    MCE_U (MCE_Vector [MCE_U (MCE_Vector [...]), ...])
--
-- The 'MCE' data type defined in this module has recursive structure to
-- express such kind of input nodes.

-- | Recursive data type for multi channel expansion.
data MCE a
  = MCEU !a
  | MCEV ![MCE a]
  deriving (Eq, Ord, Show)

instance Functor MCE where
  fmap f m =
    case m of
      MCEU x  -> MCEU (f x)
      MCEV xs -> MCEV (fmap (fmap f) xs)
  {-# INLINE fmap #-}

instance Foldable MCE where
  foldMap f m =
    case m of
      MCEU a  -> f a
      MCEV as -> foldMap (foldMap f) as
  {-# INLINE foldMap #-}

instance Traversable MCE where
  traverse f m =
    case m of
      MCEU a  -> fmap MCEU (f a)
      MCEV as -> fmap MCEV (traverse (traverse f) as)
  {-# INLINE traverse #-}

instance Applicative MCE where
  pure = MCEU
  {-# INLINE pure #-}
  f <*> x =
    case f of
      MCEU g  | MCEU y  <- x -> MCEU (g y)
              | MCEV ys <- x -> MCEV (fmap (fmap g) ys)
      MCEV gs | MCEU _  <- x -> MCEV (fmap (<*> x) gs)
              | MCEV ys <- x -> MCEV (zipWith (<*>) gs ys)
  {-# INLINE (<*>) #-}

mce_extend :: Int -> MCE a -> MCE a
mce_extend n m =
  case m of
    MCEU _  -> MCEV (replicate n m)
    MCEV xs | length xs == n -> m
            | otherwise      -> MCEV (take n (cycle xs))
{-# INLINABLE mce_extend #-}

mce_degree :: MCE a -> Int
mce_degree m =
  case m of
    MCEU _  -> 1
    MCEV xs -> length xs
{-# INLINABLE mce_degree #-}

mce_list :: MCE a -> [MCE a]
mce_list m =
  case m of
    MCEU _ -> [m]
    MCEV xs -> xs
{-# INLINABLE mce_list #-}

is_mce_vector :: MCE a -> Bool
is_mce_vector m =
  case m of
    MCEV {} -> True
    _       -> False
{-# INLINABLE is_mce_vector #-}

--
-- Auxiliary functions for NodeId, DAG, and G_Node
--

nid_value :: NodeId -> Int
nid_value nid =
  case nid of
    NodeId_C i -> i
    NodeId_K i _ -> i
    NodeId_U i -> i
    NodeId_P i _ -> i
    NConstant v -> error ("nid_value: constant " ++ show v)
{-# INLINABLE nid_value #-}

lookup_g_node :: NodeId -> DAG s -> ST s G_Node
lookup_g_node nid dag =
  case nid of
    NodeId_C k -> lookup_val k (cmap dag)
    NodeId_K k _ -> lookup_val k (kmap dag)
    NodeId_U k -> lookup_val k (umap dag)
    NodeId_P k _ -> lookup_val k (umap dag)
    NConstant v -> error ("lookup_g_node: constant " ++ show v)
{-# INLINABLE lookup_g_node #-}

nid_to_port :: NodeId -> From_Port
nid_to_port nid =
  case nid of
    NodeId_C k -> From_Port_C k
    NodeId_K k t -> From_Port_K k t
    NodeId_U k -> From_Port_U k Nothing
    NodeId_P k p -> From_Port_U k (Just p)
    NConstant v -> error ("nid_to_port: constant " ++ show v)
{-# INLINABLE nid_to_port #-}

g_node_rate :: G_Node -> Rate
g_node_rate n =
  case n of
    G_Node_C {} -> IR
    G_Node_K {} -> g_node_k_rate n
    G_Node_U {} -> g_node_u_rate n
{-# INLINABLE g_node_rate #-}

--
-- Hash-consing
--

hashcons :: (Int -> NodeId)
         -> (DAG s -> BiMap s G_Node)
         -> G_Node
         -> ReaderT (DAG s) (ST s) NodeId
hashcons con prj x = do
  dag <- ask
  v <- lift (lookup_key x (prj dag))
  case v of
    Nothing -> lift (do count <- readSTRef (hashcount dag)
                        let count' = count + 1
                        insert_at x count (prj dag)
                        count' `seq` writeSTRef (hashcount dag) count'
                        return $! con count)
    Just k  -> return $! con k
{-# INLINABLE hashcons #-}

hashconsC :: G_Node -> ReaderT (DAG s) (ST s) NodeId
hashconsC = hashcons NodeId_C cmap
{-# INLINABLE hashconsC #-}

hashconsK :: G_Node -> ReaderT (DAG s) (ST s) NodeId
hashconsK n = hashcons (flip NodeId_K (g_node_k_type n)) kmap n
{-# INLINABLE hashconsK #-}

hashconsU :: G_Node -> ReaderT (DAG s) (ST s) NodeId
hashconsU = hashcons NodeId_U umap
{-# INLINABLE hashconsU #-}
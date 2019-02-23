{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- -----------------------------------------------------------------------
-- |
-- Implicit and explicit sharing with hsc3 synthdef.
--
-- This module contains functions to construct SuperCollider synthdef
-- with hsc3 package.  The construction computation supports implicit
-- and explicit sharing in the UGen graph construction DSL.  The
-- implementation is heavily inspired from the presentation given by
-- Oleg Kiselyov, found in below URL:
--
--    <http://okmij.org/ftp/tagless-final/sharing/index.html>
--

module Sound.SC3.Jiffy.UGen.Builder
  ( UGen
  , ugen_to_graph
  , ugen_to_graphdef

  , share
  , constant
  , control
  , tr_control
  , mce
  , mce2
  , mceChannel
  , dup
  , mix

  , mkUGen
  , mkSimpleUGen
  , mkChannelsArrayUGen
  , mkDemandUGen

  , const_rate
  , maximum_rate
  , get_rate_at
  , noId
  , hashUId
  , spec0
  , envelope_to_ugen

  , Dump(..)
  ) where

-- base
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Function (on)
import Data.List (intercalate, sortBy, transpose)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- hashable
import Data.Hashable (Hashable(..))

-- hashtables
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as HT

-- hsc3
import Sound.SC3
  ( Audible(..), Binary(..), BinaryOp(..), Rate(..), K_Type(..)
  , Output, Sample, Special(..), UGenId(..), Unary(..), UnaryOp(..)
  , Envelope(..), envelope_sc3_array )
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.Server.Graphdef.Graph (graph_to_graphdef)
import Sound.SC3.UGen.Graph
  ( U_Graph(..), U_Node(..), From_Port(..)
  , ug_add_implicit, {- ug_pv_validate, -} )
import qualified Sound.SC3 as SC3
import qualified Sound.SC3.UGen.Graph as SC3UG

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)

-- Internal
import Sound.SC3.Jiffy.UGen.Orphan ()


--
-- Wrapper newtype
--

-- | Newtype for UGen graph construction with hash-consing.
newtype G a = G {unG :: forall s. ReaderT (DAG s) (ST s) a}

instance Functor G where
  fmap f (G g) = G (fmap f g)
  {-# INLINE fmap #-}

instance Applicative G where
  pure x = (G (pure x))
  {-# INLINE pure #-}
  G f <*> G x = G (f <*> x)
  {-# INLINE (<*>) #-}

instance Monad G where
  return = pure
  {-# INLINE return #-}
  G m >>= k = G (m >>= unG . k)
  {-# INLINE (>>=) #-}

runG :: G a -> (a, (Int, [U_Node], [U_Node], [U_Node]))
runG (G m) =
  runST
    (do dag <- emptyDAG
        r <- runReaderT m dag
        let as_u_nodes f | BiMap _ kt <- f dag =
              fmap (sortBy (compare `on` u_node_id)) (HC.foldM g [] kt)
            g acc (k,v) = return (gnode_to_unode k v : acc)
        cm <- as_u_nodes cmap
        km <- as_u_nodes kmap
        um <- as_u_nodes umap
        count <- readSTRef (hashcount dag)
        return (r, (count, cm, km, um)))

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
  deriving (Eq, Ord, Show)

instance Hashable NodeId where
  hashWithSalt s n =
    s `hashWithSalt` case n of
                       NodeId_C k -> k
                       NodeId_K k t -> k `hashWithSalt` t
                       NodeId_U k -> k
                       NodeId_P k p -> k `hashWithSalt` p
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

mce_elem :: MCE a -> [a]
mce_elem m =
  case m of
    MCEU x  -> [x]
    MCEV xs -> concatMap mce_elem xs
{-# INLINABLE mce_elem #-}

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
-- The UGen type and instance declarations
--

-- In this module, UGen is a type synonym.
type UGen = G (MCE NodeId)

instance Num UGen where
  fromInteger = constant . fromInteger
  {-# INLINE fromInteger #-}
  (+) = mk_binary_op_ugen Add
  {-# INLINE (+) #-}
  (*) = mk_binary_op_ugen Mul
  {-# INLINE (*) #-}
  (-) = mk_binary_op_ugen Sub
  {-# INLINE (-) #-}
  abs = mk_unary_op_ugen Abs
  {-# INLINE abs #-}
  signum = mk_unary_op_ugen Sign
  {-# INLINE signum #-}
  negate = mk_unary_op_ugen Neg
  {-# INLINE negate #-}

instance Fractional UGen where
  recip = error "G: recip"
  (/) = mk_binary_op_ugen FDiv
  {-# INLINE (/) #-}
  fromRational = constant . fromRational
  {-# INLINE fromRational #-}

instance Eq UGen where
  _ == _ = error "G: =="

instance Ord UGen where
  compare _ _ = error "G: compare"

instance Enum UGen where
  toEnum = constant . fromIntegral
  {-# INLINE toEnum #-}
  fromEnum _ = error "G: fromEnum"
  enumFrom = iterate (+ 1)
  {-# INLINE enumFrom #-}
  enumFromThen n m = iterate (+ (m - n)) n
  {-# INLINE enumFromThen #-}
  enumFromTo n m = takeWhile (<= m+1/2) (enumFrom n)
  {-# INLINE enumFromTo #-}
  enumFromThenTo n n' m =
    let p | n' >= n   = (>=)
          | otherwise = (<=)
    in  takeWhile (p (m + (n'-n)/2)) (enumFromThen n n')
  {-# INLINE enumFromThenTo #-}

instance Real UGen where
  toRational = error "G: toRational"

instance Floating UGen where
  pi = constant pi
  exp = error "G: exp"
  log = error "G: log"
  sin = error "G: sin"
  cos = error "G: cos"
  asin = error "G: asin"
  acos = error "G: acos"
  atan = error "G: atan"
  sinh = error "G: sinh"
  cosh = error "G: cosh"
  asinh = error "G: asinh"
  acosh = error "G: acosh"
  atanh = error "G: atanh"

instance RealFrac UGen where
  properFraction = error "G: properfraction"
  truncate = error "G: truncate"
  round = error "G: round"
  ceiling = error "G: ceiling"
  floor = error "G: floor"

instance Audible UGen where
  play_at opt g = play_at opt (ugen_to_graphdef "anon" g)

instance UnaryOp UGen where
  midiCPS = mk_unary_op_ugen MIDICPS
  cpsMIDI = mk_unary_op_ugen CPSMIDI

instance BinaryOp UGen where
  clip2 = mk_binary_op_ugen Clip2


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
{-# INLINABLE nid_value #-}

lookup_g_node :: NodeId -> DAG s -> ST s G_Node
lookup_g_node nid dag =
  case nid of
    NodeId_C k -> lookup_val k (cmap dag)
    NodeId_K k _ -> lookup_val k (kmap dag)
    NodeId_U k -> lookup_val k (umap dag)
    NodeId_P k _ -> lookup_val k (umap dag)
{-# INLINABLE lookup_g_node #-}

nid_to_port :: NodeId -> From_Port
nid_to_port nid =
  case nid of
    NodeId_C k -> From_Port_C k
    NodeId_K k t -> From_Port_K k t
    NodeId_U k -> From_Port_U k Nothing
    NodeId_P k p -> From_Port_U k (Just p)
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
                        return (con count))
    Just k  -> return (con k)
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


--
-- Converting to U_Graph and Graphdef
--

ugen_to_graphdef :: String -> UGen -> Graphdef
ugen_to_graphdef name g =
  let gr = ugen_to_graph g
  in  gr `seq` graph_to_graphdef name gr

ugen_to_graph :: UGen -> U_Graph
ugen_to_graph g =
  case runG g of
    (_, (count, cm, km, um)) ->
      let ug = U_Graph {ug_next_id=count
                       ,ug_constants=cm
                       ,ug_controls=km
                       ,ug_ugens=um}
      in  ug `seq` ug_add_implicit ug

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
{-# INLINABLE gnode_to_unode #-}


--
-- Constant, control, and UGen constructors
--

-- | Create constant value node.
constant :: Sample -> UGen
constant v = G (fmap MCEU (hashconsC (G_Node_C v)))
{-# INLINE constant #-}

-- | Create control value node.
control :: Rate -> String -> Sample -> UGen
control rate name val = G (fmap MCEU (hashconsK node))
  where
    node = G_Node_K {g_node_k_rate=rate
                    ,g_node_k_index=Nothing
                    ,g_node_k_name=name
                    ,g_node_k_default=val
                    ,g_node_k_type=rate_to_k_type rate}
{-# INLINABLE control #-}

-- | Create trigger control.
tr_control :: String -> Sample -> UGen
tr_control name val = G (fmap MCEU (hashconsK node))
  where
    node = G_Node_K {g_node_k_rate=KR
                    ,g_node_k_index=Nothing
                    ,g_node_k_name=name
                    ,g_node_k_default=val
                    ,g_node_k_type=K_TR}
{-# INLINABLE tr_control #-}

-- | Recursively expand multi channel inputs, and if the number of
-- outputs were greater than 1, make proxy node ids.
normalize :: Int
          -- ^ Number of outputs.
          -> ([NodeId] -> ReaderT (DAG s) (ST s) NodeId)
          -- ^ Function applied to expanded inputs.
          -> [MCE NodeId]
          -- ^ Multi-channel input node ids.
          -> ReaderT (DAG s) (ST s) (MCE NodeId)
normalize n_outputs f inputs = go inputs
  where
    go inputs0 =
      if any is_mce_vector inputs0
         then do
           let n = maximum (map mce_degree inputs0)
               inputs1 = map (mce_list . mce_extend n) inputs0
           fmap MCEV (mapM go (transpose inputs1))
         else do
           nid <- f (concatMap mce_elem inputs0)
           if n_outputs > 1
              then let mk_nodeid_p = MCEU . NodeId_P (nid_value nid)
                       nids = map mk_nodeid_p [0..(n_outputs-1)]
                   in  return (MCEV nids)
              else return (MCEU nid)
    {-# INLINE go #-}
{-# INLINABLE normalize #-}

-- | Generalized 'UGen' constructor, for defining binding functions for
-- UGens.
mkUGen :: Int
       -- ^ Number of outputs.
       -> (forall s. DAG s -> ST s UGenId)
       -- ^ Function to get 'UGenId' for 'g_node_u_ugenid' field.
       -> Special
       -- ^ UGen special.
       -> String
       -- ^ UGen name.
       -> (forall s. [NodeId] -> DAG s -> ST s Rate)
       -- ^ Function to determine the 'Rate' of UGen
       -> (forall s. [UGen] -> ReaderT (DAG s) (ST s) [MCE NodeId])
       -- ^ Function for converting inputs to mce node ids.
       -> [UGen]
       -- ^ Input arguments.
       -> UGen
mkUGen n_output uid_fn special name rate_fn input_fn input_ugens =
  G (do let f inputs = do
              dag <- ask
              rate <- lift (rate_fn inputs dag)
              uid <- lift (uid_fn dag)
              let outputs = replicate n_output rate
              hashconsU (G_Node_U {g_node_u_rate=rate
                                  ,g_node_u_name=name
                                  ,g_node_u_inputs=inputs
                                  ,g_node_u_outputs=outputs
                                  ,g_node_u_special=special
                                  ,g_node_u_ugenid=uid})
        input_mce_nids <- input_fn input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkUGen #-}

-- | Make simple UGen function.
mkSimpleUGen :: Int
             -- ^ Number of outputs.
             -> (forall s. DAG s -> ST s UGenId)
             -- ^ Function to get 'UGenId' for 'g_node_u_ugenid' field.
             -> Special
             -- ^ UGen special.
             -> String
             -- ^ UGen name.
             -> (forall s. [NodeId] -> DAG s -> ST s Rate)
             -- ^ Function to determine the 'Rate' of UGen
             -> [UGen]
             -- ^ Input arguments.
             -> UGen
mkSimpleUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f inputs = do
              dag <- ask
              rate <- lift (rate_fn inputs dag)
              uid <- lift (uid_fn dag)
              let outputs = replicate n_output rate
              hashconsU (G_Node_U {g_node_u_rate=rate
                                  ,g_node_u_name=name
                                  ,g_node_u_inputs=inputs
                                  ,g_node_u_outputs=outputs
                                  ,g_node_u_special=special
                                  ,g_node_u_ugenid=uid})
        input_mce_nids <- mapM unG input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkSimpleUGen #-}

-- | Like 'mkSimpleUGen', but treats last input argument as channels
-- array.
mkChannelsArrayUGen :: Int
                    -- ^ Number of outputs.
                    -> (forall s. DAG s -> ST s UGenId)
                    -- ^ Function to get 'UGenId' for 'g_node_u_ugenid'
                    -- field.
                    -> Special
                    -- ^ UGen special.
                    -> String
                    -- ^ UGen name.
                    -> (forall s. [NodeId] -> DAG s -> ST s Rate)
                    -- ^ Function to determine the 'Rate' of UGen
                    -> [UGen]
                    -- ^ Input arguments.
                    -> UGen
mkChannelsArrayUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f inputs = do
              dag <- ask
              rate <- lift (rate_fn inputs dag)
              uid <- lift (uid_fn dag)
              let outputs = replicate n_output rate
              hashconsU (G_Node_U {g_node_u_rate=rate
                                  ,g_node_u_name=name
                                  ,g_node_u_inputs=inputs
                                  ,g_node_u_outputs=outputs
                                  ,g_node_u_special=special
                                  ,g_node_u_ugenid=uid})
        input_mce_nids <- unwrap input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkChannelsArrayUGen #-}

-- | Dedicated UGen constructor function for demand UGen.
mkDemandUGen :: a
             -- ^ Number of outputs, actually unused.
             -> (forall s. DAG s -> ST s UGenId)
             -- ^ Function to get 'UGenId' for 'g_node_u_ugenid' field.
             -> Special
             -- ^ UGen special.
             -> String
             -- ^ UGen name.
             -> (forall s. [NodeId] -> DAG s -> ST s Rate)
             -- ^ Function to determine the 'Rate' of UGen
             -> [UGen]
             -- ^ Input arguments.
             -> UGen
mkDemandUGen _n_output uid_fn special name rate_fn input_ugens =
  G (do n_output <- mce_degree <$> unG (last input_ugens)
        let f inputs = do
              dag <- ask
              rate <- lift (rate_fn inputs dag)
              uid <- lift (uid_fn dag)
              let outputs = replicate n_output rate
              hashconsU (G_Node_U {g_node_u_rate=rate
                                  ,g_node_u_name=name
                                  ,g_node_u_inputs=inputs
                                  ,g_node_u_outputs=outputs
                                  ,g_node_u_special=special
                                  ,g_node_u_ugenid=uid})
        input_mce_nids <- unwrap input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkDemandUGen #-}

-- mk_simple_ugen :: String -> Rate -> [UGen] -> UGen
-- mk_simple_ugen name rate = mkUGen 1 noId spec0 name r_fn i_fn
--   where
--     r_fn = const_rate rate
--     i_fn = mapM unG
-- {-# INLINE mk_simple_ugen #-}

-- mk_simple_id_ugen :: String -> Rate -> [UGen] -> UGen
-- mk_simple_id_ugen name rate = mkUGen 1 hashUId spec0 name r_fn i_fn
--   where
--     r_fn = const_rate rate
--     i_fn = simple_inputs
-- {-# INLINABLE mk_simple_id_ugen #-}

-- mk_filter_ugen :: Int -> (forall s . DAG s -> ST s UGenId)
--                -> Special -> String -> [UGen] -> UGen
-- mk_filter_ugen n_output u_fn special name input_ugens =
--   mkUGen n_output u_fn special name r_fn i_fn input_ugens
--   where
--     r_fn = maximum_rate [0..(length input_ugens - 1)]
--     i_fn = simple_inputs
-- {-# INLINABLE mk_filter_ugen #-}

-- mk_simple_filter_ugen :: String -> [UGen] -> UGen
-- mk_simple_filter_ugen name = mk_filter_ugen 1 noId spec0 name
-- {-# INLINABLE mk_simple_filter_ugen #-}

-- mk_filter_id_ugen :: String -> [UGen] -> UGen
-- mk_filter_id_ugen = mk_filter_ugen 1 hashUId spec0
-- {-# INLINABLE mk_filter_id_ugen #-}

mk_unary_op_ugen :: Unary -> UGen -> UGen
mk_unary_op_ugen op a = mkSimpleUGen 1 noId special name r_fn [a]
  where
    special = Special (fromEnum op)
    name = "UnaryOpUGen"
    r_fn = get_rate_at 0
{-# INLINABLE mk_unary_op_ugen #-}

mk_binary_op_ugen :: Binary -> UGen -> UGen -> UGen
mk_binary_op_ugen op a b = mkSimpleUGen 1 noId special name r_fn [a,b]
  where
    special = Special (fromEnum op)
    name = "BinaryOpUGen"
    r_fn = maximum_rate [0,1]
{-# INLINABLE mk_binary_op_ugen #-}

noId :: DAG s -> ST s UGenId
noId _ = return NoId
{-# INLINE noId #-}

hashUId :: DAG s -> ST s UGenId
hashUId = fmap UId . readSTRef . hashcount
{-# INLINE hashUId #-}

spec0 :: Special
spec0 = Special 0
{-# INLINE spec0 #-}

const_rate :: Rate -> a -> b -> ST s Rate
const_rate r _ _ = return r
{-# INLINE const_rate #-}

-- | Get rate from index of 'NodeId' argument.
get_rate_at :: Int -> [NodeId] -> DAG s -> ST s Rate
get_rate_at i nids dag = do
  n <- lookup_g_node (nids !! i) dag
  return (g_node_rate n)
{-# INLINE get_rate_at #-}

-- | Get maximum rate from selected node ids by input argument indices.
maximum_rate :: [Int] -> [NodeId] -> DAG s -> ST s Rate
maximum_rate is nids dag = do
  let f current i = do
        node <- lookup_g_node (nids !! i) dag
        return $! max current (g_node_rate node)
  foldM f IR is
{-# INLINE maximum_rate #-}


--
-- Composite and auxiliary UGen related functions
--

share :: Applicative m => G a -> G (m a)
share g = G (fmap pure (unG g))
{-# INLINE share #-}
{-# SPECIALIZE share :: UGen -> G UGen #-}

mceChannel :: Int -> UGen -> UGen
mceChannel n g =
  G (do nid <- unG g
        case nid of
          MCEV xs -> return (xs !! n)
          MCEU _ | n == 0 -> return nid
          _ -> error "mceChannel: index out of range")
{-# INLINABLE mceChannel #-}

mce :: [UGen] -> UGen
mce gs =
  case gs of
    [g] -> g
    _   -> G (fmap MCEV (mapM unG gs))
{-# INLINABLE mce #-}

mce2 :: UGen -> UGen -> UGen
mce2 a b = G ((\x y -> MCEV [x,y]) <$> unG a <*> unG b)
{-# INLINABLE mce2 #-}

dup :: Int -> UGen -> UGen
dup n = mce . (replicate n)
{-# INLINABLE dup #-}

mix :: UGen -> UGen
mix g = do
  mce_nid <- g
  case mce_nid of
    MCEV nids ->
      -- XXX: Use 'Sum3' and 'Sum4', so move to other module.
      let f xs = case xs of
                   []    -> constant 0
                   x:[]  -> return x
                   x:xs' -> return x + f xs'
      in  f nids
    MCEU _    -> return mce_nid
{-# INLINABLE mix #-}

envelope_to_ugen :: Envelope UGen -> UGen
envelope_to_ugen e =
  case envelope_sc3_array e of
    Just as -> mce as
    Nothing -> error "envelope_to_ugen: bad Envelope"
{-# INLINABLE envelope_to_ugen #-}


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

instance Dump UGen where
  dumpString = dumpString . ugen_to_graphdef "<dump>"

instance Dump SC3.UGen where
  dumpString =
    dumpString . graph_to_graphdef "<dump>" . SC3UG.ugen_to_graph

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


--
-- Auxilary
--

rate_to_k_type :: Rate -> K_Type
rate_to_k_type rate =
  case rate of
    IR -> K_IR
    KR -> K_KR
    AR -> K_AR
    DR -> error "control: DR control"
{-# INLINE rate_to_k_type #-}

unwrap :: [G (MCE a)] -> ReaderT (DAG s) (ST s) [(MCE a)]
unwrap is =
  case is of
    []   -> return []
    j:[] -> mce_list <$> unG j
    j:js -> (:) <$> unG j <*> unwrap js
{-# INLINE unwrap #-}

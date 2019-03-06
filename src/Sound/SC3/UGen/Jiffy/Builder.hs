{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- -----------------------------------------------------------------------
-- |
-- Implicit and explicit sharing with hsc3 synthdef.  This module
-- contains functions to construct SuperCollider synthdef with hsc3
-- package.  The construction computation supports implicit and explicit
-- sharing in the UGen graph construction DSL.
--
-- The implementation is heavily inspired from the presentation given by
-- Oleg Kiselyov, found in:
--
--    <http://okmij.org/ftp/tagless-final/sharing/index.html>
--

module Sound.SC3.UGen.Jiffy.Builder
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
  , isSink

  , Dump(..)
  ) where

-- base
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldlM, toList)
import Data.List (transpose)

-- hsc3
import Sound.SC3
  ( Binary(..), BinaryOp(..), Rate(..), K_Type(..)
  , Sample, Special(..), UGenId(..), Unary(..), UnaryOp(..)
  , Envelope(..), envelope_sc3_array )
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.UGen.Graph (U_Graph)

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)

-- Internal
import Sound.SC3.Jiffy.Orphan ()
import Sound.SC3.UGen.Jiffy.Builder.Convert
import Sound.SC3.UGen.Jiffy.Builder.GraphM


--
-- Wrapper newtype
--

-- | Newtype for Synthdef UGen graph construction with hash-consing.
newtype G a = G {unG :: forall s. GraphM s a}

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


--
-- The UGen type and instance declarations
--

-- In this module, UGen is a type synonym.
type UGen = G (MCE NodeId)

instance Num UGen where
  fromInteger = constant . fromInteger
  {-# INLINE fromInteger #-}
  (+) = binary_op_add
  {-# INLINE (+) #-}
  (*) = binary_op_ugen_with (*) Mul
  {-# INLINE (*) #-}
  (-) = binary_op_ugen_with (-) Sub
  {-# INLINE (-) #-}
  abs = unary_op_ugen_with abs Abs
  {-# INLINE abs #-}
  signum = unary_op_ugen_with signum Sign
  {-# INLINE signum #-}
  negate = unary_op_ugen_with negate Neg
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
  min = mk_binary_op_ugen Min
  {-# INLINE min #-}
  max = mk_binary_op_ugen Max
  {-# INLINE max #-}

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

instance UnaryOp UGen where
  cubed = unary_op_ugen_with cubed Cubed
  {-# INLINE cubed #-}
  midiCPS = unary_op_ugen_with midiCPS MIDICPS
  {-# INLINE midiCPS #-}
  cpsMIDI = unary_op_ugen_with cpsMIDI CPSMIDI
  {-# INLINE cpsMIDI #-}
  midiRatio = unary_op_ugen_with midiRatio MIDIRatio
  {-# INLINE midiRatio #-}
  ratioMIDI = unary_op_ugen_with ratioMIDI RatioMIDI
  {-# INLINE ratioMIDI #-}

instance BinaryOp UGen where
  clip2 = binary_op_ugen_with clip2 Clip2

instance Dump UGen where
  dumpString = dumpString . ugen_to_graphdef "<dump>"


--
-- Converting to U_Graph and Graphdef
--

ugen_to_graphdef :: String -> UGen -> Graphdef
ugen_to_graphdef name (G g) =
  runST (do dag <- emptyDAG
            _ <- runReaderT g dag
            dag_to_Graphdef name dag)
{-# INLINABLE ugen_to_graphdef #-}

ugen_to_graph :: UGen -> U_Graph
ugen_to_graph (G m) =
  runST (do dag <- emptyDAG
            _ <- runReaderT m dag
            dag_to_U_Graph dag)
{-# INLINABLE ugen_to_graph #-}

--
-- Constant, control, and UGen constructors
--

-- Note [Constant foldings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We want to perform computation on constant value with Haskell
-- function, to reduce the number of UGen in synthdef graph. And at the
-- same time, want to prevent inserting unused constant values to DAG.
--
-- To meet these needs, constant nodes are represented with 'NConstant'
-- and 'NodeId_C'. 'NConstant' represents the constant value itself, and
-- 'NodeId_C' represents the node id in DAG.
--
-- In unary and binary operator functions, when all inputs were
-- 'NConstant' values, corresponding haskell function performs inplace
-- computation with the given arguments.  Non-operator UGen function
-- calls 'runG' function in its body, to hashcons the constant
-- values. At this point 'NConstnat' get converted to 'NodeId_C' in DAG.
--
-- When DAG can perform dead code elimination efficiently, 'NConstant'
-- could be removed. Apply Haskell function to constant values referred
-- by 'NodeId_C', then remove the unused constants from current graph.

-- | Unwrap the action inside 'G'. This function will store
-- 'NConstant' values to 'DAG' when found, and return 'NodeId_C'
-- value instead of the original 'NConstant'.
runG :: G (MCE NodeId) -> GraphM s (MCE NodeId)
runG g =
  let f n = case n of
              NConstant v -> hashconsC (G_Node_C v)
              _           -> return n
  in  unG g >>= mapM f
{-# INLINE runG #-}

-- | Create constant value node.
constant :: Sample -> UGen
constant v = G (return (MCEU (NConstant v)))
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

-- Note [Recursively nested MCE vectors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- 'Sound.SC3.UGen.MCE.MCE' data type does not express nested list
-- (a.k.a. Rose tree), so 'Sound.SC3.UGen.Type.UGen' data type has
-- recursive 'MCE_U' constructor, to express nested MCE values such as:
--
--   MCE_U (MCE_Vector [MCE_U (MCE_Vector [...]), ...])
--
-- The 'Sound.SC3.UGen.Jiffy.Builder.Internal.MCE' data type used in
-- 'normalize' has recursive structure to express such recusive input
-- nodes.

-- | Recursively expand multi channel inputs, and if the number of
-- outputs were greater than 1, make proxy node ids.
normalize :: Int
          -- ^ Number of outputs.
          -> ([NodeId] -> GraphM s NodeId)
          -- ^ Function applied to expanded inputs.
          -> [MCE NodeId]
          -- ^ Multi-channel input node ids.
          -> GraphM s (MCE NodeId)
normalize n_outputs f = go
  where
    go inputs =
      case mce_max_degree inputs of
        1 -> do
          nid <- f (concatMap toList inputs)
          if n_outputs > 1
             then let mk_nodeid_p = MCEU . NodeId_P (nid_value nid)
                      nids = map mk_nodeid_p [0..(n_outputs-1)]
                  in  return (MCEV n_outputs nids)
             else return (MCEU nid)
        n -> do
          let inputs' = map (mce_list . mce_extend n) inputs
          MCEV n <$> mapM go (transpose inputs')
    {-# INLINE go #-}
{-# INLINABLE normalize #-}

-- | Inner function used in UGen constructor functions.
mkUGenFn :: forall s. Int
         -> (DAG s -> ST s UGenId)
         -> Special
         -> String
         -> ([NodeId] -> DAG s -> GraphM s Rate)
         -> [NodeId]
         -> GraphM s NodeId
mkUGenFn !n_output uid_fn special name rate_fn inputs = do
  dag <- ask
  rate <- rate_fn inputs dag
  uid <- lift (uid_fn dag)
  let outputs = replicate n_output rate
  hashconsU (G_Node_U {g_node_u_rate=rate
                      ,g_node_u_name=name
                      ,g_node_u_inputs=inputs
                      ,g_node_u_outputs=outputs
                      ,g_node_u_special=special
                      ,g_node_u_ugenid=uid})
{-# INLINE mkUGenFn #-}

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
       -> (forall s. [NodeId] -> DAG s -> GraphM s Rate)
       -- ^ Function to determine the 'Rate' of UGen
       -> (forall s. [UGen] -> GraphM s [MCE NodeId])
       -- ^ Function for converting inputs to mce node ids.
       -> [UGen]
       -- ^ Input arguments.
       -> UGen
mkUGen n_output uid_fn special name rate_fn input_fn input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn
        input_mce_nids <- input_fn input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkUGen #-}

-- | Synonym to make 'UGen' binding function.
type MkUGen
  = Int
  -- ^ Number of outputs.
  -> (forall s. DAG s -> ST s UGenId)
  -- ^ Function to get 'UGenId'.
  -> Special
  -- ^ UGen special.
  -> String
  -- ^ UGen name
  -> (forall s. [NodeId] -> DAG s -> GraphM s Rate)
  -- ^ Function to determine the 'Rate' of resulting 'UGen'.
  -> [UGen]
  -- ^ Input arguments.
  -> UGen
  -- ^ The resulting 'UGen'.

-- | Make simple UGen function.
mkSimpleUGen :: MkUGen
mkSimpleUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn
        input_mce_nids <- mapM runG input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkSimpleUGen #-}

-- | Like 'mkSimpleUGen', but treats last input argument as channels
-- array.
mkChannelsArrayUGen :: MkUGen
mkChannelsArrayUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn
        input_mce_nids <- unChannelsArray input_ugens
        normalize n_output f input_mce_nids)
{-# INLINABLE mkChannelsArrayUGen #-}

-- | Dedicated UGen constructor function for demand UGen.
mkDemandUGen :: MkUGen
mkDemandUGen _n_output uid_fn special name rate_fn input_ugens =
  -- Traversing input ugens first, to get 'n_output' from the last
  -- element via 'mce_degree'.
  G (do (n_output, input_mce_ids) <- undemand input_ugens
        let f = mkUGenFn n_output uid_fn special name rate_fn
        normalize n_output f input_mce_ids)
{-# INLINABLE mkDemandUGen #-}

-- | Make a unary operator UGen, with constant folding function applied
-- to 'NConstant' input values.
unary_op_ugen_with :: (Sample -> Sample) -> Unary -> UGen -> UGen
unary_op_ugen_with fn op a =
  G (do let f inputs =
              case inputs of
                [NConstant v] -> return $ NConstant (fn v)
                [nid0] -> do
                  dag <- ask
                  n0 <- lookup_g_node nid0 dag
                  let rate = g_node_rate n0
                      opnum = fromEnum op
                      special = Special opnum
                  hashconsU (G_Node_U {g_node_u_rate=rate
                                      ,g_node_u_name="UnaryOpUGen"
                                      ,g_node_u_inputs=inputs
                                      ,g_node_u_outputs=[rate]
                                      ,g_node_u_special=special
                                      ,g_node_u_ugenid=NoId})
                _ -> error "unary_op_ugen_with: bad input"
        input_mce_nid <- unG a
        normalize 1 f [input_mce_nid])
{-# INLINEABLE unary_op_ugen_with #-}

-- | Make a binary operator UGen, with constant folding function applied
-- to 'NConstant' input values.
binary_op_ugen_with :: (Sample -> Sample -> Sample)
                    -> Binary
                    -> UGen -> UGen -> UGen
binary_op_ugen_with fn op a b =
  G (do let f inputs =
              case inputs of
                [NConstant v0, NConstant v1] ->
                  return $ NConstant (fn v0 v1)
                [NConstant v0, nid1] -> do
                  dag <- ask
                  nid0' <- hashconsC (G_Node_C v0)
                  n1 <- lookup_g_node nid1 dag
                  mkU (max IR (g_node_rate n1)) [nid0',nid1]
                [nid0, NConstant v1] -> do
                  dag <- ask
                  n0 <- lookup_g_node nid0 dag
                  nid1' <- hashconsC (G_Node_C v1)
                  mkU (max (g_node_rate n0) IR) [nid0,nid1']
                [nid0, nid1] -> do
                  dag <- ask
                  n0 <- lookup_g_node nid0 dag
                  n1 <- lookup_g_node nid1 dag
                  mkU (max (g_node_rate n0) (g_node_rate n1)) inputs
                _ -> error "binary_op_ugen_with: bad inputs"
            mkU rate inputs =
              hashconsU (G_Node_U {g_node_u_rate=rate
                                  ,g_node_u_name="BinaryOpUGen"
                                  ,g_node_u_inputs=inputs
                                  ,g_node_u_outputs=[rate]
                                  ,g_node_u_special=special
                                  ,g_node_u_ugenid=NoId})
            special = Special opnum
            opnum = fromEnum op
        a' <- unG a
        b' <- unG b
        normalize 1 f [a',b'])
{-# INLINE binary_op_ugen_with #-}

-- Note [Synthdef optimization and graph rewrite]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Some of the binary operator and unary operators could be optimized in
-- synthdef graph. When optimized replacement UGens are inserted to DAG,
-- it is possible to leave some unused UGens. The function converting
-- 'UGen' to Graphdef or U_Graph performs dead code elimination to
-- remove such unused UGen nodes from DAG.

binary_op_add :: UGen -> UGen -> UGen
binary_op_add a b =
  G (do a' <- unG a
        b' <- unG b
        normalize 1 add_ugens_inner [a',b'])
{-# INLINE binary_op_add #-}

add_ugens_inner :: [NodeId] -> GraphM s NodeId
add_ugens_inner inputs =
  case inputs of
    [NConstant a, NConstant b] -> return (NConstant (a+b))
    [NConstant a, nid1] -> do
      dag <- ask
      nid0 <- hashconsC (G_Node_C a)
      n1 <- lookup_g_node nid1 dag
      mb_nid <- maybe_optimize_add (pre nid0) nid0 IR n1
      case mb_nid of
        Just nid -> return nid
        Nothing  -> mkU (max IR (g_node_rate n1)) [nid0,nid1]
    [nid0, NConstant b] -> do
      dag <- ask
      nid1 <- hashconsC (G_Node_C b)
      n0 <- lookup_g_node nid0 dag
      mb_nid <- maybe_optimize_add (post nid1) nid1 IR n0
      case mb_nid of
        Just nid -> return nid
        Nothing  -> mkU (max (g_node_rate n0) IR) [nid0,nid1]
    [nid0, nid1] -> do
      dag <- ask
      n0 <- lookup_g_node nid0 dag
      n1 <- lookup_g_node nid1 dag
      let rate0 = g_node_rate n0
          rate1 = g_node_rate n1
      mb_nid0 <- maybe_optimize_add (post nid1) nid1 rate1 n0
      case mb_nid0 of
        Just nid -> return nid
        Nothing  -> do
          mb_nid1 <- maybe_optimize_add (pre nid0) nid0 rate0 n1
          case mb_nid1 of
            Just nid -> return nid
            Nothing  -> mkU (max rate0 rate1) [nid0, nid1]
    _ -> error "add_ugens_inner: bad inputs"
  where
    pre x xs = x:xs
    post x xs = xs <> [x]
    mkU rate ins =
      hashconsU (G_Node_U {g_node_u_rate=rate
                          ,g_node_u_name="BinaryOpUGen"
                          ,g_node_u_inputs=ins
                          ,g_node_u_outputs=[rate]
                          ,g_node_u_special=Special (fromEnum Add)
                          ,g_node_u_ugenid=NoId})
{-# INLINE add_ugens_inner #-}

maybe_optimize_add :: ([NodeId] -> [NodeId])
                   -> NodeId
                   -> Rate
                   -> G_Node
                   -> GraphM s (Maybe NodeId)
maybe_optimize_add f nid other_rate gn =
  case gn of
    G_Node_U {..}
      | is_sum3_node gn ->
        let gn' = gn {g_node_u_name="Sum4"
                     ,g_node_u_inputs=f g_node_u_inputs
                     ,g_node_u_rate=max g_node_u_rate other_rate}
        in  Just <$> hashconsU gn'
      | is_add_node gn ->
        let gn' = gn {g_node_u_name="Sum3"
                     ,g_node_u_inputs=f g_node_u_inputs
                     ,g_node_u_special=spec0
                     ,g_node_u_rate=max g_node_u_rate other_rate}
        in  Just <$> hashconsU gn'
      | is_mul_node gn -> do
        -- MulAdd UGen has some constraints for its input arguments. See
        -- the "canBeMulAdd" class method defined in "BasicOpUGen.sc"
        -- sclang source file.
        dag <- ask
        let get_node_pair i = do
               node <- lookup_g_node i dag
               return (i, node)
            mul_add ins =
              let gn' = gn {g_node_u_name="MulAdd"
                           ,g_node_u_inputs=ins++[nid]
                           ,g_node_u_special=spec0
                           ,g_node_u_rate=max g_node_u_rate other_rate}
              in  Just <$> hashconsU gn'
        inputs <- mapM get_node_pair g_node_u_inputs
        case inputs of
          [(nid0, n0), (nid1, n1)]
            | r0 == AR || (r0 == KR && other_rate_ok)
            -> mul_add [nid0,nid1]
            | r1 == AR || (r1 == KR && other_rate_ok)
            -> mul_add [nid1,nid0]
           where
             r0 = g_node_rate n0
             r1 = g_node_rate n1
             other_rate_ok = other_rate == IR || other_rate == KR
          _ -> return Nothing
      | is_neg_node gn ->
        let gn' = gn {g_node_u_name="BinaryOpUGen"
                     ,g_node_u_inputs=nid:g_node_u_inputs
                     ,g_node_u_special=Special (fromEnum Sub)
                     ,g_node_u_rate=max g_node_u_rate other_rate}
        in  Just <$> hashconsU gn'
    _ -> return Nothing
{-# INLINE maybe_optimize_add #-}

is_sum3_node :: G_Node -> Bool
is_sum3_node gn
  | G_Node_U {..} <- gn = "Sum3" == g_node_u_name
  | otherwise = False
{-# INLINE is_sum3_node #-}

is_add_node :: G_Node -> Bool
is_add_node = is_binop_node Add
{-# INLINE is_add_node #-}

is_mul_node :: G_Node -> Bool
is_mul_node = is_binop_node Mul
{-# INCLUDE is_mul_node #-}

is_neg_node :: G_Node -> Bool
is_neg_node gn
  | G_Node_U {..} <- gn
  , Special n <- g_node_u_special
  , n == fromEnum Neg
  = "UnaryOpUGen" == g_node_u_name
  | otherwise = False
{-# INLINE is_neg_node #-}

is_binop_node :: Binary -> G_Node -> Bool
is_binop_node op gn
  | G_Node_U {..} <- gn
  , Special n <- g_node_u_special
  , n == fromEnum op
  = "BinaryOpUGen" == g_node_u_name
  | otherwise = False
{-# INLINE is_binop_node #-}

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
hashUId = fmap UId . sizeBM . umap
{-# INLINE hashUId #-}

spec0 :: Special
spec0 = Special 0
{-# INLINE spec0 #-}

const_rate :: Rate -> a -> b -> GraphM s Rate
const_rate !r _ _ = return r
{-# INLINE const_rate #-}

-- | Get rate from index of 'NodeId' argument.
get_rate_at :: Int -> [NodeId] -> DAG s -> GraphM s Rate
get_rate_at !i nids dag = do
  n <- lookup_g_node (nids !! i) dag
  return $ g_node_rate n
{-# INLINE get_rate_at #-}

-- | Get maximum rate from selected node ids by input argument indices.
maximum_rate :: [Int] -> [NodeId] -> DAG s -> GraphM s Rate
maximum_rate is nids dag = do
  let f !current !i = do
        node <- lookup_g_node (nids !! i) dag
        return $ max current (g_node_rate node)
  foldlM f IR is
{-# INLINE maximum_rate #-}

-- | Input unwrapper for channels array UGens.
unChannelsArray :: [G (MCE NodeId)] -> GraphM s [MCE NodeId]
unChannelsArray is =
  case is of
    []   -> return []
    [j]  -> mce_list <$> runG j
    j:js -> (:) <$> runG j <*> unChannelsArray js
{-# INLINE unChannelsArray #-}

-- | Input unwrapper for demand UGen.
undemand :: [G (MCE NodeId)] -> GraphM s (Int, [MCE NodeId])
undemand xs =
  case xs of
    []   -> return (0,[])
    [y]  -> runG y >>= \n -> return (mce_degree n,mce_list n)
    y:ys -> runG y >>= \n -> fmap (fmap (n:)) (undemand ys)
{-# INLINE undemand #-}

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
          MCEV _ xs -> return (xs !! n)
          MCEU _ | n == 0 -> return nid
          _ -> error "mceChannel: index out of range")
{-# INLINABLE mceChannel #-}

mce :: [UGen] -> UGen
mce gs =
  G (do let f (!len,acc) g = do
              mce_nid <- unG g
              return (len+1,mce_nid:acc)
        (len,xs) <- foldlM f (0,[]) gs
        return (MCEV len (reverse xs)))
{-# INLINABLE mce #-}

mce2 :: UGen -> UGen -> UGen
mce2 a b = G ((\x y -> MCEV 2 [x,y]) <$> unG a <*> unG b)
{-# INLINE mce2 #-}

dup :: Int -> UGen -> UGen
dup n = mce . (replicate n)
{-# INLINABLE dup #-}

envelope_to_ugen :: Envelope UGen -> UGen
envelope_to_ugen e =
  case envelope_sc3_array e of
    Just as -> mce as
    Nothing -> error "envelope_to_ugen: bad Envelope"
{-# INLINABLE envelope_to_ugen #-}

isSink :: UGen -> G (Bool, MCE NodeId)
isSink ug =
  G (do mce_nid0 <- unG ug
        dag <- ask
        let f acc nid = do
              g <- lookup_g_node nid dag
              return (acc || null (g_node_u_outputs g))
        is_sink <- foldlM f False mce_nid0
        return (is_sink, mce_nid0))
{-# INLINABLE isSink #-}

--
-- Auxilary
--

rate_to_k_type :: Rate -> K_Type
rate_to_k_type rate =
  case rate of
    IR -> K_IR
    KR -> K_KR
    AR -> K_AR
    DR -> error "rate_to_ktype: DR control"
{-# INLINE rate_to_k_type #-}

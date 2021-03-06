{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- -----------------------------------------------------------------------
-- | Module containing functions to construct SuperCollider synthdef
-- with hsc3.  The construction computation supports implicit and
-- explicit sharing in the UGen graph construction DSL.
--
module Sound.SC3.UGen.Jiffy.Builder
  ( UGen
  , runUGen
  , share
  , ugen_to_graph
  , ugen_to_graphdef

  , constant
  , control
  , tr_control

  , MkUGen
  , mkSimpleUGen
  , mkImpureUGen
  , mkChannelsArrayUGen
  , mkDemandUGen
  , mkLocalBufUGen

  , mkUGenFn
  , multiNew
  , const_rate
  , maximum_rate
  , get_rate_at
  , noId
  , hashUId
  , spec0

  , mce
  , mce2
  , mceChannel
  , mceChannels
  , mceEdit

  , envelope_to_ugen

  , G(..)
  , runG
  , MCE(..)
  , NodeId(..)
  ) where

-- base
import Control.Monad.ST (ST, runST)
#if MIN_VERSION_base (4,9,0)
import Control.Monad.Fail (MonadFail(..))
#endif
import Data.Fixed (mod')
import Data.Foldable (foldlM, toList)
import Data.Function (on)
import Data.List (transpose)

-- bytestring
import Data.ByteString.Char8 (pack)

-- hsc3
import Sound.SC3
  ( SC3_Binary_Op(..), BinaryOp(..), Envelope(..), EqE(..), K_Type(..)
  , OrdE(..),  Rate(..), RealFracE(..), Sample, Special(..)
  , UGenId(..), SC3_Unary_Op(..), UnaryOp(..), envelope_sc3_array )
import Sound.SC3.Common.Math (sc3_round_to)
import Sound.SC3.Server.Graphdef (Graphdef(..))
import Sound.SC3.UGen.Graph (U_Graph)

-- Internal
import Sound.SC3.Jiffy.Orphan ()
import Sound.SC3.UGen.Jiffy.Builder.Convert
import Sound.SC3.UGen.Jiffy.Builder.GraphM


-- ------------------------------------------------------------------------
--
-- Wrapper newtype
--
-- ------------------------------------------------------------------------

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

#if MIN_VERSION_base (4,9,0)
instance MonadFail G where
  fail = error . ("G: " ++)
#endif


-- ------------------------------------------------------------------------
--
-- The UGen type and instance declarations
--
-- ------------------------------------------------------------------------

-- | UGen is a type synonym of 'G' with 'MCE' of 'NodeId'.
type UGen = G (MCE NodeId)

instance Eq UGen where
  (==) = (==) `on` ugen_to_graphdef "(Eq.==)"
  {-# INLINE (==) #-}

instance Ord UGen where
  compare a b =
    case (runUGen a, runUGen b) of
      (MCEU (NConstant x), MCEU (NConstant y)) -> compare x y
      _ -> error "compare: non-constant values"
  {-# INLINE compare #-}
  min = binary_op_with min Min
  {-# INLINE min #-}
  max = binary_op_with max Max
  {-# INLINE max #-}

instance Enum UGen where
  succ = (+ 1)
  {-# INLINE succ #-}
  pred x = x - 1
  {-# INLINE pred #-}
  toEnum = constant . fromIntegral
  {-# INLINE toEnum #-}
  fromEnum g =
    case runUGen g of
      MCEU (NConstant v) -> fromEnum v
      _ -> error "fromEnum: non-constant value"
  {-# INLINE fromEnum #-}
  enumFrom a =
    case runUGen a of
      MCEU (NConstant a') -> map constant (enumFrom a')
      _ -> error "enumFrom: non-constant value"
  {-# INLINE enumFrom #-}
  enumFromThen a b =
    case (runUGen a, runUGen b) of
      (MCEU (NConstant a'), MCEU (NConstant b')) ->
        map constant (enumFromThen a' b')
      _ -> error "enumFromThen: non-constant value"
  {-# INLINE enumFromThen #-}
  enumFromTo a b =
    case (runUGen a, runUGen b) of
      (MCEU (NConstant a'), MCEU (NConstant b')) ->
        map constant (enumFromTo a' b')
      _ -> error "enumFromTo: non-constant value"
  {-# INLINE enumFromTo #-}
  enumFromThenTo a1 a2 b =
    case (runUGen a1, runUGen a2, runUGen b) of
      (MCEU (NConstant a1'), MCEU (NConstant a2'), MCEU (NConstant b')) ->
        map constant (enumFromThenTo a1' a2' b')
      _ -> error "enumFromThenTo: non-constant value"
  {-# INLINE enumFromThenTo #-}

instance Num UGen where
  fromInteger = constant . fromInteger
  {-# INLINE fromInteger #-}
  a + b = G (mkbinop binary_add a b)
  {-# INLINE (+) #-}
  a * b = G (mkbinop binary_mul a b)
  {-# INLINE (*) #-}
  a - b = G (mkbinop binary_sub a b)
  {-# INLINE (-) #-}
  abs = unary_op_with abs Abs
  {-# INLINE abs #-}
  signum = unary_op_with signum Sign
  {-# INLINE signum #-}
  negate = unary_op_with negate Neg
  {-# INLINE negate #-}

instance Real UGen where
  toRational g = case runUGen g of
                   MCEU (NConstant v) -> toRational v
                   _ -> error "toRational: non-constant value"
  {-# INLINE toRational #-}

instance Fractional UGen where
  recip = unary_op_with recip Recip
  {-# INLINE recip #-}
  a / b = G (mkbinop binary_fdiv a b)
  {-# INLINE (/) #-}
  fromRational = constant . fromRational
  {-# INLINE fromRational #-}

instance Floating UGen where
  pi = constant pi
  {-# INLINE pi #-}
  exp = unary_op_with exp Exp
  {-# INLINE exp #-}
  log = unary_op_with log Log
  {-# INLINE log #-}
  sqrt = unary_op_with sqrt Sqrt
  {-# INLINE sqrt #-}
  (**) = binary_op_with (**) Pow
  {-# INLINE (**) #-}
  logBase a b = log b / log a
  {-# INCLUDE logBase #-}
  sin = unary_op_with sin Sin
  {-# INLINE sin #-}
  cos = unary_op_with cos Cos
  {-# INLINE cos #-}
  tan = unary_op_with tan Tan
  {-# INLINE tan #-}
  asin = unary_op_with asin ArcSin
  {-# INLINE asin #-}
  acos = unary_op_with acos ArcCos
  {-# INLINE acos #-}
  atan = unary_op_with atan ArcTan
  {-# INLINE atan #-}
  sinh = unary_op_with sinh SinH
  {-# INLINE sinh #-}
  cosh = unary_op_with cosh CosH
  {-# INLINE cosh #-}
  tanh = unary_op_with tanh TanH
  {-# INLINE tanh #-}
  asinh x = log (x + sqrt (x*x+1))
  {-# INLINE asinh #-}
  acosh x = log (x + sqrt (x*x-1))
  {-# INLINE acosh #-}
  atanh x = (log (1+x) - log (1-x)) / 2
  {-# INLINE atanh #-}

instance RealFrac UGen where
  properFraction = error "G: properfraction"
  truncate = error "G: truncate"
  round = error "G: round"
  ceiling = error "G: ceiling"
  floor = error "G: floor"

instance Integral UGen where
  quot = binary_op IDiv
  {-# INLINE quot #-}
  rem = binary_op Mod
  {-# INLINE rem #-}
  quotRem a b = (quot a b, rem a b)
  {-# INLINE quotRem #-}
  div = binary_op IDiv
  {-# INLINE div #-}
  mod = binary_op Mod
  {-# INLINE mod #-}
  toInteger = error "G: toInteger"

instance Show UGen where
  showsPrec _ _ = showString "<UGen>"

instance EqE UGen where
  equal_to = binary_op_with equal_to EQ_
  {-# INLINE equal_to #-}
  not_equal_to = binary_op_with not_equal_to NE
  {-# INLINE not_equal_to #-}

instance OrdE UGen where
  less_than = binary_op_with less_than LT_
  {-# INLINE less_than #-}
  less_than_or_equal_to = binary_op_with less_than_or_equal_to LE
  {-# INLINE less_than_or_equal_to #-}
  greater_than = binary_op_with greater_than GT_
  {-# INLINE greater_than #-}
  greater_than_or_equal_to = binary_op_with greater_than_or_equal_to GE
  {-# INLINE greater_than_or_equal_to #-}

instance RealFracE UGen where
  properFractionE = error "G: properFractionE"
  truncateE = error "G: truncateE"
  roundE i = binary_op_with sc3_round_to Round i 1
  {-# INLINE roundE #-}
  ceilingE = unary_op_with ceilingE Ceil
  {-# INLINE ceilingE #-}
  floorE = unary_op_with floorE Floor
  {-# INLINE floorE #-}

instance UnaryOp UGen where
  ampDb = unary_op_with ampDb AmpDb
  {-# INLINE ampDb #-}
  asFloat = unary_op AsFloat
  {-# INLINE asFloat #-}
  asInt = unary_op AsInt
  {-# INLINE asInt #-}
  cpsMIDI = unary_op_with cpsMIDI CPSMIDI
  {-# INLINE cpsMIDI #-}
  cpsOct = unary_op_with cpsOct CPSOct
  {-# INLINE cpsOct #-}
  cubed = unary_op_with cubed Cubed
  {-# INLINE cubed #-}
  dbAmp = unary_op_with dbAmp DbAmp
  {-# INLINE dbAmp #-}
  distort = unary_op_with distort Distort
  {-# INLINE distort #-}
  frac = unary_op Frac
  {-# INLINE frac #-}
  isNil = unary_op_with isNil IsNil
  {-# INLINE isNil #-}
  log10 = unary_op_with log10 Log10
  {-# INLINE log10 #-}
  log2 = unary_op_with log2 Log2
  {-# INLINE log2 #-}
  midiCPS = unary_op_with midiCPS MIDICPS
  {-# INLINE midiCPS #-}
  midiRatio = unary_op_with midiRatio MIDIRatio
  {-# INLINE midiRatio #-}
  notE = unary_op_with notE Not
  {-# INLINE notE #-}
  notNil = unary_op_with notNil NotNil
  {-# INLINE notNil #-}
  octCPS = unary_op_with octCPS OctCPS
  {-# INLINE octCPS #-}
  ramp_ = unary_op Ramp_
  {-# INLINE ramp_ #-}
  ratioMIDI = unary_op_with ratioMIDI RatioMIDI
  {-# INLINE ratioMIDI #-}
  softClip = unary_op_with softClip SoftClip
  {-# INLINE softClip #-}
  squared = unary_op_with squared Squared
  {-# INLINE squared #-}

instance BinaryOp UGen where
  absDif = binary_op_with absDif AbsDif
  {-# INLINE absDif #-}
  amClip = binary_op_with amClip AMClip
  {-# INLINE amClip #-}
  atan2E = binary_op_with atan2E Atan2
  {-# INLINE atan2E #-}
  clip2 = binary_op_with clip2 Clip2
  {-# INLINE clip2 #-}
  difSqr = binary_op_with difSqr DifSqr
  {-# INLINE difSqr #-}
  excess = binary_op_with excess Excess
  {-# INLINE excess #-}
  exprandRange = binary_op ExpRandRange
  {-# INLINE exprandRange #-}
  fill = binary_op Fill
  {-# INLINE fill #-}
  firstArg = binary_op_with firstArg FirstArg
  {-# INLINE firstArg #-}
  fold2 = binary_op_with fold2 Fold2
  {-# INLINE fold2 #-}
  gcdE = binary_op GCD
  {-# INLINE gcdE #-}
  hypot = binary_op_with hypot Hypot
  {-# INLINE hypot #-}
  hypotx = binary_op_with hypotx Hypotx
  {-# INLINE hypotx #-}
  iDiv = binary_op_with iDiv IDiv
  {-# INLINE iDiv #-}
  lcmE = binary_op LCM
  {-# INLINE lcmE #-}
  modE = binary_op_with mod' Mod
  {-# INLINE modE #-}
  randRange = binary_op RandRange
  {-# INLINE randRange #-}
  ring1 = binary_op_with ring1 Ring1
  {-# INLINE ring1 #-}
  ring2 = binary_op_with ring2 Ring2
  {-# INLINE ring2 #-}
  ring3 = binary_op_with ring3 Ring3
  {-# INLINE ring3 #-}
  ring4 = binary_op_with ring4 Ring4
  {-# INLINE ring4 #-}
  roundUp = binary_op_with roundUp RoundUp
  {-# INLINE roundUp #-}
  scaleNeg = binary_op_with scaleNeg ScaleNeg
  {-# INLINE scaleNeg #-}
  sqrDif = binary_op_with sqrDif SqrDif
  {-# INLINE sqrDif #-}
  sqrSum = binary_op_with sqrSum SqrSum
  {-# INLINE sqrSum #-}
  thresh = binary_op_with thresh Thresh
  {-# INLINE thresh #-}
  trunc = binary_op Trunc
  {-# INLINE trunc #-}
  wrap2 = binary_op_with wrap2 Wrap2
  {-# INLINE wrap2 #-}


-- ------------------------------------------------------------------------
--
-- Converting to U_Graph and Graphdef
--
-- ------------------------------------------------------------------------

runUGen :: G a -> a
runUGen (G g) = runST (emptyDAG >>= runGraphM g)
{-# INLINE runUGen #-}

runUGenWith :: (forall s. DAG s -> ST s a) -> UGen -> a
runUGenWith f (G g) =
  runST (do dag <- emptyDAG
            _ <- runGraphM g dag
            f dag)
{-# INLINE runUGenWith #-}

share :: UGen -> G UGen
share g = G (fmap pure (unG g))
{-# INLINE share #-}

ugen_to_graphdef :: String -> UGen -> Graphdef
ugen_to_graphdef name = runUGenWith (dag_to_Graphdef name)
{-# INLINABLE ugen_to_graphdef #-}

ugen_to_graph :: UGen -> U_Graph
ugen_to_graph = runUGenWith dag_to_U_Graph
{-# INLINABLE ugen_to_graph #-}


-- ------------------------------------------------------------------------
--
-- Constant, control, and UGen constructors
--
-- ------------------------------------------------------------------------

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
runG :: UGen -> GraphM s (MCE NodeId)
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

-- | Create a control node with given rate, name, and default value.
control :: Rate -> String -> Sample -> UGen
control rate name val = G (fmap MCEU (hashconsK node))
  where
    node = G_Node_K {g_node_k_rate=rate
                    ,g_node_k_index=Nothing
                    ,g_node_k_name=pack name
                    ,g_node_k_default=val
                    ,g_node_k_type=k_type}
    k_type =
      case rate of
        IR -> K_IR
        KR -> K_KR
        AR -> K_AR
        DR -> error "control: DR"
{-# INLINABLE control #-}

-- | Create a trigger control node.
tr_control :: String -> Sample -> UGen
tr_control name val = G (fmap MCEU (hashconsK node))
  where
    node = G_Node_K {g_node_k_rate=KR
                    ,g_node_k_index=Nothing
                    ,g_node_k_name=pack name
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
-- 'multiNew' has recursive structure to express such recusive input
-- nodes.

-- | Recursively expand multi channel inputs, and if the number of
-- outputs were greater than 1, make output proxy node ids.
multiNew :: Int
          -- ^ Number of outputs.
          -> ([NodeId] -> GraphM s NodeId)
          -- ^ Function applied to expanded inputs.
          -> [MCE NodeId]
          -- ^ Multi-channel input node ids.
          -> GraphM s (MCE NodeId)
multiNew n_outputs f = go
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
{-# INLINABLE multiNew #-}

-- | Inner function used in UGen constructor functions.
mkUGenFn :: forall s. Int
         -> (DAG s -> GraphM s UGenId)
         -> Special
         -> Name
         -> ([NodeId] -> DAG s -> GraphM s Rate)
         -> Bool
         -> [NodeId]
         -> GraphM s NodeId
mkUGenFn !n_output uid_fn special name rate_fn is_pure inputs = do
  dag <- ask
  rate <- rate_fn inputs dag
  uid <- uid_fn dag
  let outputs = replicate n_output rate
  hashconsU (G_Node_U {g_node_u_rate=rate
                      ,g_node_u_name=name
                      ,g_node_u_inputs=inputs
                      ,g_node_u_outputs=outputs
                      ,g_node_u_special=special
                      ,g_node_u_ugenid=uid
                      ,g_node_u_pure=is_pure})
{-# INLINE mkUGenFn #-}

-- | Synonym to make 'UGen' binding function.
type MkUGen
  = Int
  -- ^ Number of outputs.
  -> (forall s. DAG s -> GraphM s UGenId)
  -- ^ Function to get 'UGenId'.
  -> Special
  -- ^ UGen special.
  -> Name
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
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn True
        input_mce_nids <- mapM runG input_ugens
        multiNew n_output f input_mce_nids)
{-# INLINABLE mkSimpleUGen #-}

mkImpureUGen :: MkUGen
mkImpureUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn False
        input_mce_nids <- mapM runG input_ugens
        multiNew n_output f input_mce_nids)
{-# INLINABLE mkImpureUGen #-}

-- | Like 'mkSimpleUGen', but treats last input argument as channels
-- array.
mkChannelsArrayUGen :: Bool -> MkUGen
mkChannelsArrayUGen is_pure n_output uid_fn special name rate_fn
                    input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn is_pure
        input_mce_nids <- unChannelsArray input_ugens
        multiNew n_output f input_mce_nids)
{-# INLINABLE mkChannelsArrayUGen #-}

-- | Dedicated UGen constructor function for demand UGen.
mkDemandUGen :: MkUGen
mkDemandUGen _n_output uid_fn special name rate_fn input_ugens =
  -- Traversing input ugens first, to get 'n_output' from the last
  -- element via 'mce_degree'.
  G (do (n_output, input_mce_ids) <- undemand input_ugens
        let f = mkUGenFn n_output uid_fn special name rate_fn True
        multiNew n_output f input_mce_ids)
{-# INLINABLE mkDemandUGen #-}

-- | Dedicated UGen constructor function for localBuf UGen.
mkLocalBufUGen :: MkUGen
mkLocalBufUGen n_output uid_fn special name rate_fn input_ugens =
  G (do let f = mkUGenFn n_output uid_fn special name rate_fn True
        dag <- ask
        incrementNumLocalBufs dag
        input_mce_nids <- mapM runG input_ugens
        multiNew n_output f input_mce_nids)
{-# INLINABLE mkLocalBufUGen #-}

-- | Make a unary operator UGen, with constant folding function applied
-- to 'NConstant' input values.
unary_op_with :: (Sample -> Sample) -> SC3_Unary_Op -> UGen -> UGen
unary_op_with fn op a =
  G (do let f inputs =
              case inputs of
                [NConstant v] -> return $ NConstant (fn v)
                [nid0] -> do
                  dag <- ask
                  n0 <- lookup_g_node nid0 dag
                  let rate = g_node_rate n0
                      special = Special (fromEnum op)
                  hashconsU (G_Node_U {g_node_u_rate=rate
                                      ,g_node_u_name="UnaryOpUGen"
                                      ,g_node_u_inputs=inputs
                                      ,g_node_u_outputs=[rate]
                                      ,g_node_u_special=special
                                      ,g_node_u_ugenid=NoId
                                      ,g_node_u_pure=True})
                _ -> error "unary_op_with: bad input"
        input_mce_nid <- unG a
        multiNew 1 f [input_mce_nid])
{-# INLINEABLE unary_op_with #-}

unary_op :: SC3_Unary_Op -> UGen -> UGen
unary_op op a = mkSimpleUGen 1 noId special name r_fn [a]
  where
    special = Special (fromEnum op)
    name = "UnaryOpUGen"
    r_fn = get_rate_at 0
{-# INLINE unary_op #-}

-- | Make a binary operator UGen, with applying given function when both
-- arguments were 'NConstant'.
binary_op_with :: (Sample -> Sample -> Sample)
               -> SC3_Binary_Op
               -> UGen -> UGen -> UGen
binary_op_with fn op a b = G (mkbinop f a b)
  where
    f inputs =
      case inputs of
        [NConstant v0, NConstant v1] ->
          return $ NConstant (fn v0 v1)
        [NConstant v0, nid1] -> do
          dag <- ask
          nid0 <- hashconsC (G_Node_C v0)
          n1 <- lookup_g_node nid1 dag
          mkU (max IR (g_node_rate n1)) [nid0,nid1]
        [nid0, NConstant v1] -> do
          dag <- ask
          n0 <- lookup_g_node nid0 dag
          nid1 <- hashconsC (G_Node_C v1)
          mkU (max (g_node_rate n0) IR) [nid0,nid1]
        [nid0, nid1] -> do
          dag <- ask
          n0 <- lookup_g_node nid0 dag
          n1 <- lookup_g_node nid1 dag
          let rate = max (g_node_rate n0) (g_node_rate n1)
          mkU rate inputs
        _ -> error "binary_op_with: bad inputs"
    mkU rate inputs =
      hashconsU (G_Node_U {g_node_u_rate=rate
                          ,g_node_u_name="BinaryOpUGen"
                          ,g_node_u_inputs=inputs
                          ,g_node_u_outputs=[rate]
                          ,g_node_u_special=Special (fromEnum op)
                          ,g_node_u_ugenid=NoId
                          ,g_node_u_pure=True})
{-# INLINE binary_op_with #-}

mkbinop :: ([NodeId] -> GraphM s NodeId)
        -> UGen -> UGen -> GraphM s (MCE NodeId)
mkbinop f (G a) (G b) =
  do a' <- a
     b' <- b
     multiNew 1 f [a',b']
{-# INLINE mkbinop #-}

binary_op :: SC3_Binary_Op -> UGen -> UGen -> UGen
binary_op op a b = mkSimpleUGen 1 noId special name r_fn [a,b]
  where
    special = Special (fromEnum op)
    name = "BinaryOpUGen"
    r_fn = maximum_rate [0,1]
{-# INLINABLE binary_op #-}

-- Note [Synthdef optimization and graph rewrite]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Some of the binary operator and unary operators could be optimized in
-- synthdef graph. When optimized, replacement UGens are inserted to DAG
-- after counting the descendants of replaced UGen. Such replacements
-- may leave some unused UGens. The function converting 'UGen' to
-- Graphdef and U_Graph performs dead code elimination to remove unused
-- nodes from DAG.

binary_add :: [NodeId] -> GraphM s NodeId
binary_add inputs =
  case inputs of
    [NConstant 0,        nid1] -> return nid1
    [nid0,        NConstant 0] -> return nid0
    [NConstant a, NConstant b] -> return (NConstant (a+b))
    [NConstant a,        nid1] -> registerCN Add AddArgs a nid1
    [nid0,        NConstant b] -> registerNC Add AddArgs nid0 b
    [nid0,               nid1] -> registerNN Add AddArgs nid0 nid1
    _ -> error "binary_add_inner: bad inputs"
{-# INLINE binary_add #-}

binary_sub :: [NodeId] -> GraphM s NodeId
binary_sub inputs =
  case inputs of
    [nid0,        NConstant 0] -> return nid0
    [NConstant a, NConstant b] -> return (NConstant (a - b))
    [NConstant 0,        nid1] -> negN nid1
    [NConstant a,        nid1] -> registerCN Sub SubArgs a nid1
    [nid0,        NConstant b] -> registerNC Sub SubArgs nid0 b
    [nid0,               nid1] -> registerNN Sub SubArgs nid0 nid1
    _ -> error "binary_sub_inner"
{-# INLINE binary_sub #-}

binary_mul :: [NodeId] -> GraphM s NodeId
binary_mul inputs =
  case inputs of
    [NConstant 0,              _] -> return (NConstant 0)
    [NConstant 1,           nid1] -> return nid1
    [_,              NConstant 0] -> return (NConstant 0)
    [nid0,           NConstant 1] -> return nid0
    [NConstant a,    NConstant b] -> return (NConstant (a*b))
    [NConstant (-1),        nid1] -> negN nid1
    [NConstant a,           nid1] -> binaryCN Mul a nid1
    [nid0,        NConstant (-1)] -> negN nid0
    [nid0,           NConstant b] -> binaryNC Mul nid0 b
    [nid0,                  nid1] -> binaryNN Mul nid0 nid1
    _ -> error "binary_mul: bad inputs"
{-# INLINE binary_mul #-}

binary_fdiv :: [NodeId] -> GraphM s NodeId
binary_fdiv inputs =
  case inputs of
    [nid0,        NConstant 1] -> return nid0
    [NConstant a, NConstant b] -> return (NConstant (a/b))
    [nid0,     NConstant (-1)] -> negN nid0
    [NConstant a,        nid1] -> binaryCN FDiv a nid1
    [nid0,        NConstant b] -> binaryNC FDiv nid0 b
    [nid0,               nid1] -> binaryNN FDiv nid0 nid1
    _ -> error "binary_fdiv: bad inputs"
{-# INLINE binary_fdiv #-}

registerCN :: SC3_Binary_Op -> (NodeId -> NodeId -> OpArg)
           -> Sample -> NodeId -> GraphM s NodeId
registerCN op oparg v0 nid1 = do
  nid0 <- hashconsC (G_Node_C v0)
  dag <- ask
  n1 <- lookup_g_node nid1 dag
  me <- mkBinaryOp op (g_node_rate n1) [nid0,nid1]
  registerOp dag me (oparg nid0 nid1)
{-# INLINE registerCN #-}

registerNC :: SC3_Binary_Op -> (NodeId -> NodeId -> OpArg)
           -> NodeId -> Sample -> GraphM s NodeId
registerNC op oparg nid0 v1 = do
  nid1 <- hashconsC (G_Node_C v1)
  dag <- ask
  n0 <- lookup_g_node nid0 dag
  me <- mkBinaryOp op (g_node_rate n0) [nid0,nid1]
  registerOp dag me (oparg nid0 nid1)
{-# INLINE registerNC #-}

registerNN :: SC3_Binary_Op -> (NodeId -> NodeId -> OpArg)
           -> NodeId -> NodeId -> GraphM s NodeId
registerNN op oparg nid0 nid1 = do
  dag <- ask
  n0 <- lookup_g_node nid0 dag
  n1 <- lookup_g_node nid1 dag
  let r0 = g_node_rate n0
      r1 = g_node_rate n1
  me <- mkBinaryOp op (max r0 r1) [nid0,nid1]
  registerOp dag me (oparg nid0 nid1)
{-# INLINE registerNN #-}

-- | Binary op, constant first argument, node ID second argument.
binaryCN :: SC3_Binary_Op -> Sample -> NodeId -> GraphM s NodeId
binaryCN op v0 nid1 = do
  nid0 <- hashconsC (G_Node_C v0)
  n1 <- ask >>= lookup_g_node nid1
  mkBinaryOp op (g_node_rate n1) [nid0,nid1]
{-# INLINE binaryCN #-}

-- | Binary op, node ID first argument, constant second argument.
binaryNC :: SC3_Binary_Op -> NodeId -> Sample -> GraphM s NodeId
binaryNC op nid0 v1 = do
  n0 <- ask >>= lookup_g_node nid0
  nid1 <- hashconsC (G_Node_C v1)
  mkBinaryOp op (g_node_rate n0) [nid0,nid1]
{-# INLINE binaryNC #-}

-- | Binary op, node ID first and second argument.
binaryNN :: SC3_Binary_Op -> NodeId -> NodeId -> GraphM s NodeId
binaryNN op nid0 nid1 = do
  dag <- ask
  n0 <- lookup_g_node nid0 dag
  n1 <- lookup_g_node nid1 dag
  let rate = max (g_node_rate n0) (g_node_rate n1)
  mkBinaryOp op rate [nid0, nid1]
{-# INLINE binaryNN #-}

negN :: NodeId -> GraphM s NodeId
negN nid = do
  n <- ask >>= lookup_g_node nid
  mkNeg (g_node_rate n) nid
{-# INLINE negN #-}

mkNeg :: Rate -> NodeId -> GraphM s NodeId
mkNeg rate nid =
  hashconsU (G_Node_U {g_node_u_rate=rate
                      ,g_node_u_name="UnaryOpUGen"
                      ,g_node_u_inputs=[nid]
                      ,g_node_u_outputs=[rate]
                      ,g_node_u_special=Special (fromEnum Neg)
                      ,g_node_u_ugenid=NoId
                      ,g_node_u_pure=True})
{-# INLINE mkNeg #-}

mkBinaryOp :: SC3_Binary_Op -> Rate -> [NodeId] -> GraphM s NodeId
mkBinaryOp op rate ins =
  hashconsU (G_Node_U {g_node_u_rate=rate
                      ,g_node_u_name="BinaryOpUGen"
                      ,g_node_u_inputs=ins
                      ,g_node_u_outputs=[rate]
                      ,g_node_u_special=Special (fromEnum op)
                      ,g_node_u_ugenid=NoId
                      ,g_node_u_pure=True})
{-# INLINE mkBinaryOp #-}

noId :: DAG s -> GraphM s UGenId
noId _ = return NoId
{-# INLINE noId #-}

hashUId :: DAG s -> GraphM s UGenId
hashUId = liftST . fmap UId . sizeBM . umap
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
  case g_node_rate n of
    -- Filter UGens with demand rate inputs are forced to run in control
    -- rate, otherwise unwanted demand rate appear while chaining
    -- signals in 'pvcollect'.
    DR -> pure KR
    rt -> pure rt
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
unChannelsArray :: [UGen] -> GraphM s [MCE NodeId]
unChannelsArray is =
  case is of
    []   -> return []
    [j]  -> mce_list <$> runG j
    j:js -> (:) <$> runG j <*> unChannelsArray js
{-# INLINE unChannelsArray #-}

-- | Input unwrapper for demand UGen.
undemand :: [UGen] -> GraphM s (Int, [MCE NodeId])
undemand xs =
  case xs of
    []   -> return (0,[])
    [y]  -> runG y >>= \n -> return (mce_degree n,mce_list n)
    y:ys -> runG y >>= \n -> fmap (fmap (n:)) (undemand ys)
{-# INLINE undemand #-}


-- ------------------------------------------------------------------------
--
-- Multichannel expansion
--
-- ------------------------------------------------------------------------

-- | Multichannel expansion node constructor
mce :: Foldable t => t UGen -> UGen
mce gs =
  G (do let f (!len,acc) g = do
              mce_nid <- unG g
              return (len+1,mce_nid:acc)
        (len,xs) <- foldlM f (0,[]) gs
        case xs of
          [x] -> return x
          _   -> return (MCEV len (reverse xs)))
{-# INLINABLE mce #-}
{-# SPECIALIZE mce :: [UGen] -> UGen #-}

-- | Multiple channel expansion for two inputs.
mce2 :: UGen -> UGen -> UGen
mce2 a b = G ((\x y -> MCEV 2 [x,y]) <$> unG a <*> unG b)
{-# INLINE mce2 #-}

-- | Obtain indexed channel at MCE.
mceChannel :: Int -> UGen -> UGen
mceChannel n g =
  G (do nid <- unG g
        case nid of
          MCEV m xs | n < m  -> return (xs !! n)
          MCEU _    | n == 0 -> return nid
          _ -> error "mceChannel: index out of range")
{-# INLINABLE mceChannel #-}

-- | Output channels of UGen as a list. Unlike the function defined in
-- hsc3 package, this function lives inside 'G' monad.
mceChannels :: UGen -> G [UGen]
mceChannels g =
  G (do mce_nid <- unG g
        case mce_nid of
          MCEV _ xs -> pure (map pure xs)
          MCEU _    -> pure [pure mce_nid])
{-# INLINABLE mceChannels #-}

-- | Apply UGen list operation on MCE contents.
mceEdit :: ([UGen] -> [UGen]) -> UGen -> UGen
mceEdit f g =
  G (do mce_nid <- unG g
        let ms = f (map pure (mce_list mce_nid))
        case ms of
          [x] -> unG x
          _   -> unG (mce ms))
{-# INLINE mceEdit #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

-- | Unwrap 'Envelope' to 'UGen' with 'envelope_sc3_array'.
envelope_to_ugen :: Envelope UGen -> UGen
envelope_to_ugen e =
  case envelope_sc3_array e of
    Just as -> mce as
    Nothing -> error "envelope_to_ugen: bad Envelope"
{-# INLINABLE envelope_to_ugen #-}

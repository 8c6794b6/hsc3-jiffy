{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Handwritten UGen binding functions.
module Sound.SC3.UGen.Jiffy.Bindings.Handwritten
  ( -- * Not defined with hsc3-db data
    clearBuf
  , dinf
  , dwrand
  , fftTrigger
  , packFFT
  , poll
  , pv_HainsworthFoote
  , sendReply
  , unpack1FFT

    -- * Composite UGen functions
  , asLocalBuf
  , changed
  , dup
  , dynKlang
  , dynKlank
  , exprange
  , fft'
  , ifft'
  , klangSpec
  , klankSpec
  , linLin
  , mix
  , onsets'
  , packFFTSpec
  , pmOsc
  , pvcollect
  , unpackFFT
  , silent
  , soundIn
  , splay
  , tap
  , tChoose
  , tWChoose
  , wrapOut

    -- * Re-export
  , onsetType
  ) where

-- base
import Data.Foldable (Foldable(..), foldlM)
import Data.List (transpose)

-- hosc
import Sound.OSC (sendMessage)

-- hsc3
import Sound.SC3
  ( Audible(..), DoneAction(..), Loop(..), Rate(..), Sample
  , (>**), onsetType )
import Sound.SC3.Server.Command.Generic (withCM)
import Sound.SC3.Server.Command.Plain (d_recv_bytes, s_new)

-- Internal
import Sound.SC3.Jiffy.Encode (encode_graphdef)
import Sound.SC3.UGen.Jiffy.Bindings.Generated
import Sound.SC3.UGen.Jiffy.Builder
import Sound.SC3.UGen.Jiffy.Builder.GraphM


-- ------------------------------------------------------------------------
--
-- Orphan instance
--
-- ------------------------------------------------------------------------

-- 'Audible' instance for 'UGen' is defined here, since the 'out'
-- UGen, whiich is generated from template haskell code, is referred
-- from definition body via 'wrapOut'.

instance Audible UGen where
  play_at (nid,aa,gid,params) ug =
    let gd = ugen_to_graphdef "anon" (wrapOut ug)
        dr = d_recv_bytes (encode_graphdef gd)
        sn = s_new "anon" nid aa gid params
    in  sendMessage (withCM dr sn)


-- ------------------------------------------------------------------------
--
-- UGens not defined with hsc3-db data
--
-- ------------------------------------------------------------------------

-- | Zero local buffer.
--
-- ClearBuf is impure, used for side effect purpose.
clearBuf :: UGen -> UGen
clearBuf a = mkImpureUGen 1 noId spec0 name r_fn [a]
  where
    name = "ClearBuf"
    r_fn = const_rate IR

-- | Infinity constant value.
dinf :: UGen
dinf = constant (1/0)

-- | Demand rate weighted random sequence generator.
dwrand :: UGen -> UGen -> UGen -> UGen
dwrand repeats weights lst =
  G (do repeats' <- runG repeats
        weights' <- runG weights
        lst' <- runG lst
        let n = mce_degree weights'
            weights'' = mce_list (mce_extend n weights')
            lst'' = mce_list lst'
        n_nid <- hashconsC (G_Node_C (fromIntegral n))
        let inputs = repeats': MCEU n_nid : weights'' ++ lst''
            f = mkUGenFn 1 hashUId spec0 "Dwrand" (const_rate DR) True
        multiNew 1 f inputs)

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger buf hop pol = mkSimpleUGen 1 noId spec0 name rt [buf,hop,pol]
  where
    name = "FFTTrigger"
    rt = const_rate KR

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> Int -> Int -> Int -> UGen -> UGen -> UGen
packFFT chain bufsize from to zeroothers magsphases =
  G (do chain' <- runG chain
        bufsize' <- intG bufsize
        from' <- intG from
        to' <- intG to
        zeroothers' <- runG zeroothers
        magsphases' <- runG magsphases
        let n = mce_degree magsphases'
            mps = mce_list magsphases'
        n' <- intG n
        let inputs = chain':bufsize':from':to':zeroothers':n':mps
            f = mkUGenFn 1 noId spec0 "PackFFT" (const_rate KR) True
        multiNew 1 f inputs)

-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> String -> UGen
poll t sig tid label =
  G (do t' <- runG t
        sig' <- runG sig
        tid' <- runG tid
        let n = fromIntegral (length label)
        n' <- MCEU <$> hashconsC (G_Node_C n)
        n_plus_1' <- MCEU <$> hashconsC (G_Node_C (n+1))
        label' <- mapM charG label
        let inputs = t':sig':tid':n_plus_1':n':label'
            f = mkUGenFn 0 noId spec0 "Poll" (get_rate_at 1) False
        multiNew 0 f inputs)

-- | FFT onset detector.
pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf proph propf thres waittime =
  mkSimpleUGen 1 noId spec0 name rt [buf,proph,propf,thres,waittime]
    where
      name = "PV_HainsworthFoote"
      rt = const_rate AR

-- | Send a reply message from the server back to all registered clients.
sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply t rid cmd vals =
  G (do t' <- runG t
        rid' <- runG rid
        cmd' <- stringG cmd
        vals' <- mapM runG vals
        let f = mkUGenFn 0 noId spec0 "SendReply" (get_rate_at 0) False
            inputs = t':rid':cmd' ++ vals'
        multiNew 0 f inputs)

-- | Unpack a single value (magnitude or phase) from an FFT chain.
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf sz idx which =
  mkSimpleUGen 1 noId spec0 name (const_rate DR) [buf,sz,idx,which]
    where
      name = "Unpack1FFT"


-- ------------------------------------------------------------------------
--
-- Composite UGen functions
--
-- ------------------------------------------------------------------------

-- | Generate a 'localBuf' and use 'setBuf' to initialise it.
asLocalBuf :: Foldable t => t UGen -> UGen
asLocalBuf xs = do
  b <- localBuf 1 (fromIntegral (length xs))
  _ <- setBuf (return b) 0 (fromIntegral (length xs)) (mce xs)
  return b
{-# SPECIALIZE asLocalBuf :: [UGen] -> UGen #-}

-- | Triggers when a value changes
changed :: UGen -> UGen -> UGen
changed inp thres = abs (hpz1 inp) >** thres

-- | Duplicate given 'UGen' for given number.
dup :: Int -> UGen -> UGen
dup n = mce . (replicate n)
{-# INLINABLE dup #-}

-- | Dynamic klang, dynamic sine oscillator bank
dynKlang :: Rate -> UGen -> UGen -> UGen -> UGen
dynKlang rt fs fo spec =
  let gen (f:a:p:rest) = sinOsc rt (f*fs+fo) p * a + gen rest
      gen _ = 0
  in  mceChannels spec >>= gen

-- | Dynamic klank, set of non-fixed resonating filters.
dynKlank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dynKlank i fs fo ds spec =
  let gen (f:a:d:rest) = ringz i (f*fs+fo) (d*ds) * a + gen rest
      gen _ = 0
  in  mceChannels spec >>= gen

-- | 'linExp' with input range of @(-1,1)@.
exprange :: UGen -> UGen -> UGen -> UGen
exprange lo hi s = linExp s (-1) 1 lo hi

-- | Variant 'fft' with default values for hop size (0.5), window type
-- (0), active status (1) and wndow size (0).
fft' :: UGen -> UGen -> UGen
fft' buf i = fft buf i 0.5 0 1 0

-- | Variant 'ifft' with default value for window type.
ifft' :: UGen -> UGen
ifft' buf = ifft buf 0 0

-- | Format frequency, amplitude, and phase data as required for
-- 'klang'.
klangSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klangSpec fs as ps = mce (concat (transpose [fs, as, ps]))

-- | Format frequency, amplitude, and decay time as required for
-- 'klank'.
klankSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klankSpec = klangSpec

-- | Map from one linear range to another linear range.
linLin :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linLin i slo shi dlo dhi =
  let scale = (dhi - dlo) / (shi - slo)
      offset = dlo - (scale * slo)
  in  i * scale + offset

-- | Collapse possible mce by summing.
mix :: UGen -> UGen
mix g = do
  mce_nid <- g
  case mce_nid of
    MCEV _ nids ->
      let f xs =
            case xs of
              []         -> constant 0
              [a]        -> p a
              [a,b]      -> p a + p b
              [a,b,c]    -> sum3 (p a) (p b) (p c)
              [a,b,c,d]  -> sum4 (p a) (p b) (p c) (p d)
              a:b:c:d:ys -> sum4 (p a) (p b) (p c) (p d) + f ys
          p = pure
      in  f nids
    MCEU _    -> return mce_nid
{-# INLINABLE mix #-}

-- | Onset detector with default values for minor parameters.
onsets' :: UGen -> UGen -> UGen -> UGen
onsets' chain thres otype = onsets chain thres otype 1 0.1 10 11 1 0

-- | Format magnitude and phase data data as required for 'packFFT'.
packFFTSpec :: [UGen] -> [UGen] -> UGen
packFFTSpec mags phases = mce (interleave mags phases)

-- | Phase modulation oscillator pair.
pmOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

-- | Apply function /f/ to each bin of an @FFT@ chain, /f/ receives
-- magnitude, phase, and index and returns a @(magnitude, phase)@.
pvcollect :: UGen -- ^ FFT chain.
          -> Int  -- ^ Number of frames.
          -> (UGen -> UGen -> Int -> (UGen,UGen)) -- ^ Function /f/.
          -> Int  -- ^ From bin.
          -> Int  -- ^ To bin.
          -> UGen -- ^ Zero others?
          -> UGen
pvcollect c nframes f from to z =
  let (ms,ps) = unzip (unpackFFT c nframes from to)
      is = [from .. to]
      e = zipWith3 f ms ps is
      mps = uncurry packFFTSpec (unzip e)
  in  packFFT c nframes from to z mps

-- | Output silence.
silent :: Int -> UGen
silent n =
  let sig = dc AR 0
  in  case n of
        1 -> sig
        _ -> dup n sig

-- | Zero indexed audio input buses.
soundIn :: UGen -> UGen
soundIn u =
  G (do u' <- unG u
        case u' of
          MCEV n (MCEU (NConstant i):cs)
            | cs == map (MCEU . NConstant) [i+1..i+fromIntegral (n-1)]
            -> unG (in' n AR (numOutputBuses + constant i))
          _ -> unG (in' 1 AR (numOutputBuses + return u')))

-- | Spreads an array of channels across the streo field.
splay :: UGen -> UGen -> UGen -> UGen -> Bool -> UGen
splay i s l c lc =
  G (do i' <- unG i
        let n = max 2 (mce_degree i')
            m = n - 1
            f = constant . (+ (-1.0)) . (* (2/fromIntegral m)) .
                fromIntegral
            p = map f [0..m]
            a = if lc
                   then sqrt (1/fromIntegral n)
                   else 1
        unG (mix (pan2 i (mce p * s + c) a) * l))

-- | Single tap into a delayline.
tap :: Int -> UGen -> UGen -> UGen
tap numChannels bufnum delaytime =
  let n = delaytime * negate sampleRate
  in  playBuf numChannels AR bufnum 1 0 n Loop DoNothing

-- | Randomly select one of several inputs on trigger.
tChoose :: Foldable t => UGen -> t UGen -> UGen
tChoose trg arr = select (tiRand 0 (constant n) trg) (mce arr)
  where n = fromIntegral ((length arr) - 1)
{-# SPECIALIZE tChoose :: UGen -> [UGen] -> UGen #-}

-- | Randomly select one of several inputs on trigger (weighted).
tWChoose :: (Foldable t1, Foldable t2)
          => UGen -> t1 UGen -> t2 UGen -> UGen -> UGen
tWChoose trg arr weights nrm =
  select (tWindex trg nrm (mce weights)) (mce arr)
{-# SPECIALIZE tWChoose :: UGen -> [UGen] -> [UGen] -> UGen -> UGen#-}

-- | Unpack an FFT chain into separate demand-rate FFT bin streams.
--
-- Unline hsc3, this function returns a list of tuples of magnitude and
-- phase from DC up to Nyquiest, i.e.:
--
-- > [(m[0],p[0]),(m[1],p[1]) ... (m[nyquist],p[nyquist])]
--
unpackFFT :: UGen -> Int -> Int -> Int -> [(UGen, UGen)]
unpackFFT chain nf from to =
  let go acc n =
        if to < n
           then reverse acc
           else let f = unpack1FFT chain (int nf) (int n)
                    m = f 0
                    p = f 1
                    acc' =  m `seq` p `seq` (m,p) : acc
                    n' = n+1
                in  acc' `seq` n' `seq` go acc' n'
      int = constant . fromIntegral
  in  go [] from

-- | Jiffy version of 'Sound.SC3.UGen.Bindings.Composite.wrapOut'.
wrapOut :: UGen -> UGen
wrapOut ug = do
  (sink, mce_nid) <- isSink ug
  if sink
     then return mce_nid
     else out (control IR "out" 0) (return mce_nid)
{-# INLINE wrapOut #-}


-- ------------------------------------------------------------------------
--
-- Auxiliary
--
-- ------------------------------------------------------------------------

-- | Get list of 'MCE' 'NodeId' from 'String', with length prefix.
stringG :: String -> GraphM s [MCE NodeId]
stringG str = do
  n <- MCEU <$> hashconsC (G_Node_C (fromIntegral (length str)))
  chars <- mapM charG str
  pure (n:chars)
{-# INLINE stringG #-}

-- | Store 'Char' as constant value in 'GraphM'.
charG :: Char -> GraphM s (MCE NodeId)
charG = intG . fromEnum
{-# INLINE charG #-}

-- | Store 'Int' as constant value in 'GraphM'.
intG :: Int -> GraphM s (MCE NodeId)
intG = constantG . fromIntegral
{-# INLINE intG #-}

-- | Store constant 'Sample' value in 'GraphM'.
constantG :: Sample -> GraphM s (MCE NodeId)
constantG = fmap MCEU . hashconsC . G_Node_C
{-# INLINE constantG #-}

-- | Merge with taking element from each list.
--
-- >>> interleave [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]
--
interleave :: [a] -> [a] -> [a]
interleave = go
  where
    go []     ys = ys
    go (x:xs) ys = x : go ys xs
{-# INLINE interleave #-}

-- | Returns 'True' and node id if the node is sink node.
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

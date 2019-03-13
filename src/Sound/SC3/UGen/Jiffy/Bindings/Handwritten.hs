{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Handwritten UGen bindings.
module Sound.SC3.UGen.Jiffy.Bindings.Handwritten
  ( -- * Not defined with hsc3-db data
    clearBuf
  -- , dwrand
  , packFFT
  -- , pv_HainsworthFoote
  , sendReply
  , unpack1FFT

    -- * Composite UGen functions
  , asLocalBuf
  , dup
  , fft'
  , mix
  , ifft'
  , unpackFFT
  , wrapOut
  ) where

-- hosc
import Sound.OSC (sendMessage)

-- hsc3
import Sound.SC3 (Audible(..))
import Sound.SC3.Server.Command.Generic (withCM)
import Sound.SC3.Server.Command.Plain (d_recv_bytes, s_new)

-- Internal
import Sound.SC3.Jiffy.Encode (encode_graphdef)
import Sound.SC3.UGen.Jiffy.Bindings.Generated
import Sound.SC3.UGen.Jiffy.Builder
import Sound.SC3.UGen.Jiffy.Builder.GraphM

--
-- Orphan instance
--

-- | 'Audible' instance for 'UGen' is defined here, since the 'out'
-- UGen, whiich is generated from template haskell code, is referred
-- from definition body via 'wrapOut'.
instance Audible UGen where
  play_at (nid,aa,gid,params) ug =
    let gd = ugen_to_graphdef "anon" (wrapOut ug)
        dr = d_recv_bytes (encode_graphdef gd)
        sn = s_new "anon" nid aa gid params
    in  sendMessage (withCM dr sn)

--
-- Not defined with hsc3-db data
--

clearBuf :: UGen -> UGen
clearBuf = error "clearBuf"

-- dwrand :: UGen -> UGen -> UGen -> UGen
-- dwrand = error "dwrand"

packFFT :: UGen -> Int -> Int -> Int -> UGen -> UGen -> UGen
packFFT = error "packFFT"

-- poll?

-- pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
-- pv_HainsworthFoote = error "pv_HainsworthFoote"

sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply = error "sendReply"

unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT = error "unpack1FFT"


--
-- Composite UGen functions
--

asLocalBuf :: [UGen] -> UGen
asLocalBuf = error "asLocalBuf"

-- | Duplicate given 'UGen' for given number.
dup :: Int -> UGen -> UGen
dup n = mce . (replicate n)
{-# INLINABLE dup #-}

-- | Sum MCE UGen to single channel.
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

-- | Jiffy version of 'Sound.SC3.UGen.Composite.fft''.
fft' :: UGen -> UGen -> UGen
fft' buf i = fft buf i 0.5 0 1 0

-- | Jiffy version of 'Sound.SC3.UGen.Composite.ifft''.
ifft' :: UGen -> UGen
ifft' buf = ifft buf 0 0

unpackFFT :: UGen -> Int -> Int -> Int -> UGen -> [UGen]
unpackFFT = error "unpackFFT"

-- | Jiffy version of 'Sound.SC3.UGen.Bindings.Composite.wrapOut'.
wrapOut :: UGen -> UGen
wrapOut ug = do
  (sink, mce_nid) <- isSink ug
  if sink
     then return mce_nid
     else out 0 (return mce_nid)
{-# INLINE wrapOut #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Hand-written composite UGen functions.
module Sound.SC3.UGen.Jiffy.Composite
  ( mix
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
import Sound.SC3.UGen.Jiffy.Builder
import Sound.SC3.UGen.Jiffy.Builder.Internal
import Sound.SC3.UGen.Jiffy.DB

--
-- Orphan instance
--

-- 'Audible' instance for 'UGen' is defined here, since the 'out' UGen,
-- whiich is generated from template haskell code, is referred from
-- definition body via 'wrapOut'.

instance Audible UGen where
  play_at (nid,aa,gid,params) ug =
    let gd = ugen_to_graphdef "anon" (wrapOut ug)
        dr = d_recv_bytes (encode_graphdef gd)
        sn = s_new "anon" nid aa gid params
    in  sendMessage (withCM dr sn)

--
-- Hand-written Functions
--

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

-- | Jiffy Variant of 'Sound.SC3.UGen.Bindings.Composite.wrapOut'.
wrapOut :: UGen -> UGen
wrapOut ug = do
  (sink, mce_nid) <- isSink ug
  if sink
     then return mce_nid
     else out 0 (return mce_nid)
{-# INLINE wrapOut #-}

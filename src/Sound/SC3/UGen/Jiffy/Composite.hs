-- | Hand-written composite UGen functions.
module Sound.SC3.UGen.Jiffy.Composite
  ( mix
  ) where

-- Internal
import Sound.SC3.UGen.Jiffy.Builder
import Sound.SC3.UGen.Jiffy.Builder.Internal
import Sound.SC3.UGen.Jiffy.DB

-- | Sum MCE UGen to single channel.
mix :: UGen -> UGen
mix g = do
  mce_nid <- g
  case mce_nid of
    MCEV nids ->
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

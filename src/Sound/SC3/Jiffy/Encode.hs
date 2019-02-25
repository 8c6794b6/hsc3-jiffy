{-# LANGUAGE OverloadedStrings #-}
-- ------------------------------------------------------------------------
-- |
-- Functions to encode 'UGen' to synthdef bytecode.
module Sound.SC3.Jiffy.Encode
  ( encode_graphdef
  ) where

-- base
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))

-- bytestring
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- hsc3
import Sound.SC3.Server.Graphdef
  ( Graphdef(..), Name, Control, Sample, Input(..), UGen )

--
-- Encoding functions for 'Graphdef'
--

encode_pstr :: Name -> B.Builder
encode_pstr n = B.word8 (fromIntegral (S.length n)) <> B.byteString n
{-# INLINE encode_pstr #-}

encode_input :: Input -> B.Builder
encode_input (Input u p) = i16 u <> i16 p
{-# INLINE encode_input #-}

encode_control :: Control -> B.Builder
encode_control (nm,k) = encode_pstr nm <> i16 k
{-# INLINE encode_control #-}

encode_ugen :: UGen -> B.Builder
encode_ugen (nm,r,is,os,s) =
  encode_pstr nm <>
  B.int8 (fromIntegral r) <>
  i16 (length is) <>
  i16 (length os) <>
  i16 s <>
  encodes_with encode_input is <>
  encodes_with (B.int8 . fromIntegral) os
{-# INLINE encode_ugen #-}

encode_sample :: Sample -> B.Builder
encode_sample = B.floatBE . realToFrac
{-# INLINE encode_sample #-}

encode_graphdef' :: Graphdef -> B.Builder
encode_graphdef' (Graphdef nm cs ks us) =
  let (ks_ctl, ks_def) = unzip ks
  in  B.byteString "SCgf" <>
      B.int32BE 0 <>
      B.int16BE 1 <>
      encode_pstr nm <>
      i16 (length cs) <>
      encodes_with encode_sample cs <>
      i16 (length ks_def) <>
      encodes_with encode_sample ks_def <>
      i16 (length ks_ctl) <>
      encodes_with encode_control ks_ctl <>
      i16 (length us) <>
      encodes_with encode_ugen us
{-# INLINE encode_graphdef #-}

encode_graphdef :: Graphdef -> L.ByteString
encode_graphdef = B.toLazyByteString . encode_graphdef'

--
-- Auxiliary
--

i16 :: Int -> B.Builder
i16 = B.int16BE . fromIntegral
{-# INLINE i16 #-}

encodes_with :: (a -> B.Builder) -> [a] -> B.Builder
encodes_with f = foldr (\a b -> f a <> b) mempty
{-# INLINE encodes_with #-}

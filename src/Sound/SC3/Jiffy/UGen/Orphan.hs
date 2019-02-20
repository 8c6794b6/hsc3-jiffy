{-# OPTIONS_GHC -Wno-orphans #-}
-- -----------------------------------------------------------------------
-- | Module containing orphan instances.
module Sound.SC3.Jiffy.UGen.Orphan () where

-- hashable
import Data.Hashable (Hashable(..), hashUsing)

-- hsc3
import Sound.SC3 (K_Type(..), Rate(..), Special(..), UGenId(..))

instance Ord Special where
  compare (Special a) (Special b) = compare a b
  {-# INLINE compare #-}

instance Hashable Special where
  hashWithSalt s (Special n) = s `hashWithSalt` n
  {-# INLINE hashWithSalt #-}

instance Hashable K_Type where
  hashWithSalt s kt =
    s `hashWithSalt` (case kt of
                        K_IR -> 0
                        K_KR -> 1
                        K_TR -> 2
                        K_AR -> 3 :: Int)
  {-# INLINE hashWithSalt #-}

instance Ord UGenId where
  compare NoId    NoId    = EQ
  compare NoId    (UId _) = LT
  compare (UId _) NoId    = GT
  compare (UId a) (UId b) = compare a b
  {-# INLINE compare #-}

instance Hashable UGenId where
  hashWithSalt s uid =
    s `hashWithSalt` (case uid of
                        NoId  -> (0::Int)
                        UId i -> (1::Int) `hashWithSalt` i)
  {-# INLINE hashWithSalt #-}

instance Hashable Rate where
  hashWithSalt = hashUsing fromEnum
  {-# INLINE hashWithSalt #-}

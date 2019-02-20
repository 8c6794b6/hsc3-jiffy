{-# OPTIONS_GHC -Wno-orphans #-}
-- -----------------------------------------------------------------------
-- | Module containing orphan instances.
module Sound.SC3.Jiffy.UGen.Orphan () where

-- hashable
import Data.Hashable (Hashable(..), hashUsing)

-- hsc3
import Sound.SC3 (K_Type(..), Rate(..), Special(..), UGenId(..))
import Sound.SC3.UGen.Graph (From_Port(..))

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

instance Ord From_Port where
  compare (From_Port_C n1) (From_Port_C n2) =
    compare n1 n2
  compare (From_Port_C {}) (From_Port_K {}) = LT
  compare (From_Port_C {}) (From_Port_U {}) = LT
  compare (From_Port_K {}) (From_Port_C {}) = GT
  compare (From_Port_K n1 k1) (From_Port_K n2 k2) =
    compare n1 n2 <> compare k1 k2
  compare (From_Port_K {}) (From_Port_U {}) = LT
  compare (From_Port_U {}) (From_Port_C {}) = GT
  compare (From_Port_U {}) (From_Port_K {}) = GT
  compare (From_Port_U n1 i1) (From_Port_U n2 i2) =
    compare n1 n2 <> compare i1 i2
  {-# INLINE compare #-}

instance Hashable From_Port where
  hashWithSalt s fp =
    s `hashWithSalt` case fp of
      From_Port_C n -> n
      From_Port_K n t -> n `hashWithSalt` t
      From_Port_U n i -> n `hashWithSalt` i
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

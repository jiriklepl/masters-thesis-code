{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Data.Bounds (module CMM.Data.Bounds, module CMM.Data.Bounds.Impl) where

import safe Data.PartialOrd (PartialOrd((<=), (>), (>=)))
import safe Prelude hiding (Ord(..))

import safe CMM.Data.Lattice (Lattice(..), join, meet)

import safe CMM.Data.Bounds.Impl

instance Lattice a => Semigroup (Bounds a) where
  Bounds low high <> Bounds low' high' = join low low' `Bounds` meet high high'

instance (Lattice a, Bounded a) => Monoid (Bounds a) where
  mempty = minBound `Bounds` maxBound

absurdBounds :: Bounded a => Bounds a
absurdBounds = maxBound `Bounds` minBound

isTrivial :: Eq a => Bounds a -> Bool
isTrivial (Bounds low high) = low == high

isAbsurd :: PartialOrd a => Bounds a -> Bool
isAbsurd (Bounds low high) = low > high

isTrivialOrAbsurd :: PartialOrd a => Bounds a -> Bool
isTrivialOrAbsurd (Bounds low high) = low >= high

normalizeAbsurd :: (PartialOrd a, Bounded a) => Bounds a -> Bounds a
normalizeAbsurd bounds@(Bounds low high) =
  if low <= high
    then bounds
    else absurdBounds

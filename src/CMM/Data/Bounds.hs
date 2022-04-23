{-# LANGUAGE Safe #-}

module CMM.Data.Bounds
  ( module CMM.Data.Bounds
  , module CMM.Data.Bounds.Impl
  ) where

import safe Data.Bool (Bool)
import safe Data.Eq (Eq((==)))
import safe Data.PartialOrd (PartialOrd((<=), (>), (>=)))

import safe CMM.Data.Bounded (Bounded(maxBound, minBound))

import safe CMM.Data.Bounds.Impl (Bounds(Bounds), lowerBound, upperBound)

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

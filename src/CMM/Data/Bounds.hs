{-# LANGUAGE Safe #-}

module CMM.Data.Bounds
  ( module CMM.Data.Bounds
  , module CMM.Data.Bounds.Impl
  ) where

import safe Data.PartialOrd as PO (PartialOrd((<=), (>), (>=)))

import safe CMM.Data.Bounds.Impl (Bounds(Bounds), lowerBound, upperBound)

absurdBounds :: Bounded a => Bounds a
absurdBounds = maxBound `Bounds` minBound

isTrivial :: Eq a => Bounds a -> Bool
isTrivial (Bounds low high) = low == high

isAbsurd :: PartialOrd a => Bounds a -> Bool
isAbsurd (Bounds low high) = low PO.> high

isTrivialOrAbsurd :: PartialOrd a => Bounds a -> Bool
isTrivialOrAbsurd (Bounds low high) = low PO.>= high

normalizeAbsurd :: (PartialOrd a, Bounded a) => Bounds a -> Bounds a
normalizeAbsurd bounds@(Bounds low high) =
  if low PO.<= high
    then bounds
    else absurdBounds

{-# LANGUAGE Safe #-}

module CMM.Data.Bounds
  ( module CMM.Data.Bounds
  , module CMM.Data.Bounds.Impl
  ) where

import safe Data.PartialOrd as PO (PartialOrd((<=), (>), (>=)))

import safe CMM.Data.Bounds.Impl (Bounds(Bounds), lowerBound, upperBound)

-- | Returns a normalized form of absurd bounds (where lower bound is the top and upper bound is the bottom)
absurdBounds :: Bounded a => Bounds a
absurdBounds = maxBound `Bounds` minBound

-- | Returns `True` iff the `Bounds` object is trivial
isTrivial :: Eq a => Bounds a -> Bool
isTrivial (Bounds low high) = low == high

-- | Returns `True` iff the `Bounds` object is absurd
isAbsurd :: PartialOrd a => Bounds a -> Bool
isAbsurd (Bounds low high) = low PO.> high

-- | Returns `True` iff the `Bounds` object is trivial or absurd
isTrivialOrAbsurd :: PartialOrd a => Bounds a -> Bool
isTrivialOrAbsurd (Bounds low high) = low PO.>= high

-- | Returns the object if it is not absurd, otherwise, returns `absurdBounds`
normalizeAbsurd :: (PartialOrd a, Bounded a) => Bounds a -> Bounds a
normalizeAbsurd bounds@(Bounds low high) =
  if low PO.<= high
    then bounds
    else absurdBounds

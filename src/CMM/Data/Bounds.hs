{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds where

import safe Control.Lens.TH (makeLenses)
import safe Data.Data (Data)
import safe Data.PartialOrd (PartialOrd((<=), (>), (>=)))
import safe Prelude hiding (Ord(..))
import safe Prelude (Ord)

import safe CMM.Data.Lattice (Lattice(..), join, meet)

data Bounds a =
  Bounds
    { _lowerBound :: a
    , _upperBound :: a
    }
  deriving (Show, Eq, Ord, Functor, Data)

makeLenses ''Bounds

instance Lattice a => Semigroup (Bounds a) where
  Bounds low high <> Bounds low' high' = join low low' `Bounds` meet high high'

instance (Lattice a, Bounded a) => Monoid (Bounds a) where
  mempty = minBound `Bounds` maxBound

absurdBounds :: Bounded a => Bounds a
absurdBounds = maxBound `Bounds` minBound

isTrivial :: PartialOrd a => Bounds a -> Bool
isTrivial (Bounds low high) = low >= high

isAbsurd :: PartialOrd a => Bounds a -> Bool
isAbsurd (Bounds low high) = low > high

normalizeAbsurd :: (PartialOrd a, Bounded a) => Bounds a -> Bounds a
normalizeAbsurd bounds@(Bounds low high) =
  if low <= high
    then bounds
    else absurdBounds

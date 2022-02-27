{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds where

import Control.Lens.TH (makeLenses)
import safe Data.Data (Data)

import safe CMM.Data.Lattice (Lattice(..), join, meet)

data Bounds a =
  Bounds
    { _lowerBound :: a
    , _upperBound :: a
    }
  deriving (Show, Eq, Ord, Data)

makeLenses ''Bounds

instance Lattice a => Semigroup (Bounds a) where
  Bounds low high <> Bounds low' high' = join low low' `Bounds` meet high high'

instance (Lattice a, Bounded a) => Monoid (Bounds a) where
  mempty = minBound `Bounds` maxBound

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds.Impl
  ( Bounds(Bounds)
  , lowerBound
  , upperBound
  ) where

import safe Control.Lens.TH (makeLenses)

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Functor (Functor)
import safe Data.Ord (Ord)
import safe Data.Semigroup ( Semigroup((<>)) )
import safe Data.Monoid ( Monoid(mempty) )
import safe Text.Show (Show)

import safe CMM.Data.Lattice ( join, meet, Lattice )
import safe CMM.Data.Bounded ( Bounded (minBound, maxBound) )

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

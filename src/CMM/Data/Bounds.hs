{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds where

import safe Data.Data (Data)
import safe Data.Kind as Kind ( Type, Constraint )
import Control.Lens.TH (makeLenses)

import safe CMM.Data.Lattice ( Lattice(..), join, meet )

data Bounds a (b :: Kind.Type -> Kind.Constraint) =
  Bounds
    { _lowerBound :: a
    , _upperBound :: a
    }
  deriving (Show, Eq, Ord, Data)

makeLenses ''Bounds

instance Lattice a => Semigroup (Bounds a Lattice) where
  Bounds low high <> Bounds low' high' =
    meet low low' `Bounds` join high high'

instance (Lattice a, Bounded a) => Monoid (Bounds a Lattice) where
  mempty = minBound `Bounds` maxBound

instance Ord a => Semigroup (Bounds a Ord) where
  Bounds low high <> Bounds low' high' =
    max low low' `Bounds` min high high'

instance (Ord a, Bounded a) => Monoid (Bounds a Ord) where
  mempty = minBound `Bounds` maxBound

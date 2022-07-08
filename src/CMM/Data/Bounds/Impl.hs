{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds.Impl
  ( Bounds(Bounds)
  , lowerBound
  , upperBound
  ) where

import safe Control.Lens.TH (makeLenses)
import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), (<+>), brackets, comma)

import safe CMM.Data.Lattice (Lattice, join, meet)

-- | An object having a lower bound and upper bound, usable for intervals, for example
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

instance Pretty a => Pretty (Bounds a) where
  pretty =
    \case
      low `Bounds` high -> brackets $ pretty low <> comma <+> pretty high

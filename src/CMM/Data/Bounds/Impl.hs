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
import safe Text.Show (Show)

data Bounds a =
  Bounds
    { _lowerBound :: a
    , _upperBound :: a
    }
  deriving (Show, Eq, Ord, Functor, Data)

makeLenses ''Bounds

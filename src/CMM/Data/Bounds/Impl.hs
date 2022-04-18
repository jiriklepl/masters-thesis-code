{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Data.Bounds.Impl (Bounds(..), lowerBound, upperBound) where

import safe Prelude

import safe Data.Data (Data)
import safe Control.Lens.TH (makeLenses)

data Bounds a =
  Bounds
    { _lowerBound :: a
    , _upperBound :: a
    }
  deriving (Show, Eq, Ord, Functor, Data)

makeLenses ''Bounds

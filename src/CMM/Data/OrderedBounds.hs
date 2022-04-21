{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Data.OrderedBounds
  (
  ) where

import safe Data.Eq (Eq)
import safe Data.Monoid ((<>))
import safe Data.Ord (Ord(compare))

import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Ordered (Ordered(Ordered))

instance (Ord (Ordered a), Eq a) => Ord (Ordered (Bounds a)) where
  Ordered (low `Bounds` high) `compare` Ordered (low' `Bounds` high') =
    Ordered low `compare` Ordered low' <> Ordered high `compare` Ordered high'

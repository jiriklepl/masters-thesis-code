{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Data.Dioid where

import safe Data.Bool (Bool(False, True), (&&), (||))
import safe Data.Function (($))
import safe qualified Data.Map as Map
import safe Data.Map (Map)
import safe Data.Monoid (All(All), Any(Any), Monoid(mempty), (<>))
import safe Data.Ord (Ord)
import safe qualified Data.Set as Set
import safe Data.Set (Set)

class Monoid a =>
      Dioid a
  where
  (<+>) :: a -> a -> a
  (<+>) = (<>)
  (<.>) :: a -> a -> a
  mfull :: a

instance Dioid Any where
  Any x <.> Any y = Any $ x && y
  mfull = Any True

instance Dioid All where
  All x <.> All y = All $ x || y
  mfull = All False

instance Ord a => Dioid (Set a) where
  (<.>) = Set.union
  mfull = mempty

instance Ord k => Dioid (Map k v) where
  (<.>) = Map.union
  mfull = mempty

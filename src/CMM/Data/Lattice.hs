{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Data.Lattice where

import safe qualified Data.Map as Map
import safe Data.Map (Map)
import safe qualified Data.Set as Set
import safe Data.Set (Set)

class Lattice a where
  (/\) :: a -> a -> a
  (\/) :: a -> a -> a

meet :: Lattice a => a -> a -> a
meet = (/\)

join :: Lattice a => a -> a -> a
join = (\/)

instance Ord a => Lattice a where
  (/\) = min
  (\/) = max

instance {-# OVERLAPPING #-}  Ord a => Lattice (Set a) where
  (/\) = Set.intersection
  (\/) = Set.union

instance {-# OVERLAPPING #-} Ord k => Lattice (Map k v) where
  (/\) = Map.intersection
  (\/) = Map.union

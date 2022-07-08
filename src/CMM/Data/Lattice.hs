{-# LANGUAGE Safe #-}

module CMM.Data.Lattice where

import safe Data.PartialOrd (PartialOrd)

-- | A class representing a lattice
class PartialOrd a =>
      Lattice a
  where
  (/\) :: a -> a -> a
  (\/) :: a -> a -> a

-- | An alias to the (/\) operation
meet :: Lattice a => a -> a -> a
meet = (/\)

-- | An alias to the (\/) operation
join :: Lattice a => a -> a -> a
join = (\/)

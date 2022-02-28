{-# LANGUAGE Safe #-}

module CMM.Data.Lattice where

import safe Data.PartialOrd (PartialOrd)

class PartialOrd a =>
      Lattice a
  where
  (/\) :: a -> a -> a
  (\/) :: a -> a -> a

meet :: Lattice a => a -> a -> a
meet = (/\)

join :: Lattice a => a -> a -> a
join = (\/)

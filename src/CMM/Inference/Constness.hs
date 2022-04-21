{-# LANGUAGE Safe #-}

module CMM.Inference.Constness where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Ord as Ord (Ord((<=), compare, max, min), Ordering(EQ, GT, LT))
import safe Data.PartialOrd (PartialOrd((<=)))
import safe Text.Show (Show)

import safe CMM.Data.Bounded (Bounded(maxBound, minBound))
import safe CMM.Data.Lattice (Lattice((/\), (\/)))
import safe CMM.Data.Ordered (Ordered(Ordered))

data Constness
  = Regular
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Data)

deriving instance Ord (Ordered Constness)

instance Ord Constness where
  Regular `compare` Regular = EQ
  LinkExpr `compare` LinkExpr = EQ
  ConstExpr `compare` ConstExpr = EQ
  Regular `compare` _ = GT
  _ `compare` Regular = LT
  LinkExpr `compare` _ = GT
  _ `compare` LinkExpr = LT

instance PartialOrd Constness where
  (<=) = (Ord.<=)

instance Lattice Constness where
  (/\) = min
  (\/) = max

instance Bounded Constness where
  minBound = ConstExpr
  maxBound = Regular

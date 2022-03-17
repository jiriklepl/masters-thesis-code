{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Inference.Constness where

import safe Data.Data (Data)
import safe Data.PartialOrd ( PartialOrd((<=)) )

import safe CMM.Data.Ordered (Ordered(Ordered))
import safe CMM.Data.Lattice (Lattice (..))

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
  (<=) = (Prelude.<=)

instance Lattice Constness where
  (/\) = min
  (\/) = max

instance Bounded Constness where
  minBound = ConstExpr
  maxBound = Regular

{-# LANGUAGE Safe #-}

module CMM.Inference.Constness where

import safe Data.Data (Data)
import safe Data.Ord as Ord (Ord((<=)))
import safe Data.PartialOrd (PartialOrd((<=)))

import safe Prettyprinter (Pretty(pretty))

import safe CMM.Data.Lattice (Lattice((/\), (\/)))
import safe CMM.Data.Ordered (Ordered(Ordered))

-- | The constness object representing constness constants
data Constness
  = Regular -- ^ represents runtime values
  | LinkExpr -- ^ represents values known during link-time
  | ConstExpr -- ^ represents values known during compile-time
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

instance Pretty Constness where
  pretty =
    \case
      Regular -> "Regular"
      LinkExpr -> "Link"
      ConstExpr -> "Const"

{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Data.Ordered where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Ord (Ord)
import safe Text.Show (Show)

data Ordered a where
  Ordered :: { unOrdered :: a} -> Ordered a

deriving instance Show a => Show (Ordered a)

deriving instance Eq a => Eq (Ordered a)

deriving instance (Data a, Ord (Ordered a)) => Data (Ordered a)

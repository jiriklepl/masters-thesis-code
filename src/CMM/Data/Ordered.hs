{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Data.Ordered where

import safe Data.Data (Data)

data Ordered a where
  Ordered :: { unOrdered :: a} -> Ordered a

deriving instance Show a => Show (Ordered a)

deriving instance Eq a => Eq (Ordered a)

deriving instance (Data a, Ord (Ordered a)) => Data (Ordered a)

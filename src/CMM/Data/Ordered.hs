{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module CMM.Data.Ordered where

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty))

data Ordered a where
  Ordered :: { unOrdered :: a} -> Ordered a

deriving instance Show a => Show (Ordered a)

deriving instance Eq a => Eq (Ordered a)

deriving instance (Data a, Ord (Ordered a)) => Data (Ordered a)

instance Pretty a => Pretty (Ordered a) where
  pretty = pretty . unOrdered

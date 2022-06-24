{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module CMM.Data.Ordered where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Ord (Ord)
import safe Text.Show (Show)
import safe Data.Function ( (.) )

import safe Prettyprinter ( Pretty(pretty) )

data Ordered a where
  Ordered :: Ord (Ordered a) => { unOrdered :: a} -> Ordered a

deriving instance Show a => Show (Ordered a)

deriving instance Eq a => Eq (Ordered a)

deriving instance (Data a, Ord (Ordered a)) => Data (Ordered a)

instance Pretty a => Pretty (Ordered a) where
  pretty = pretty . unOrdered

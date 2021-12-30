{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Data.Orderable where

class Orderable a b where
  makeOrdered :: Ord b => a -> b
  unmakeOrdered :: b -> a

instance Ord a => Orderable a a where
  makeOrdered = id
  unmakeOrdered = id

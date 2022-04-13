{-# LANGUAGE Safe #-}

module CMM.Data.Nullable where

infixr 5 ??

class Fallbackable a where
  (??) :: a -> a -> a

class Nullable a where
  nullVal :: a

instance Fallbackable (Maybe a) where
  Nothing ?? a = a
  a ?? _ = a

instance Nullable (Maybe a) where
  nullVal = Nothing

instance Fallbackable Bool where
  (??) = (||)

instance Nullable Bool where
  nullVal = False

instance Fallbackable (Either a b) where
  Left _ ?? a = a
  a ?? _ = a

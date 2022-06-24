{-# LANGUAGE Safe #-}

module CMM.Data.Nullable where

import safe Data.Monoid ( Monoid(mempty), Sum )
import safe Data.Bool (Bool(False), (||))
import safe Data.Either (Either(Left))
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Map ( Map )
import safe Data.Set (Set)

import safe CMM.Data.Bimap ( Bimap )
import safe qualified CMM.Data.Bimap as Bimap
import safe qualified Data.Set as Set
import safe qualified Data.Map as Map
import safe CMM.Data.Num ( Num )

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

instance Nullable (Map k a) where
  nullVal = Map.empty

instance Nullable (Set k) where
  nullVal = Set.empty

instance Nullable (Bimap k a) where
  nullVal = Bimap.empty

instance Num a => Nullable (Sum a) where
  nullVal = mempty

instance Fallbackable (Either a b) where
  Left _ ?? a = a
  a ?? _ = a

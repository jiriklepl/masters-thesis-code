{-# LANGUAGE Trustworthy #-}

module CMM.Data.MapLike where

import safe Data.Maybe ( Maybe, isJust )
import safe Data.Bool
import safe Data.Function
import safe qualified Data.List as List
import safe Data.Eq
import safe Data.Ord
import safe Data.Functor
import safe Data.Tuple
import safe GHC.Err
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import safe Data.Kind (Constraint)

type family Adjust n :: Constraint
type family Lookup n :: Constraint
type family ToList n :: Constraint
type family FromList n :: Constraint
type family Elems n :: Constraint
type family Empty n :: Constraint
type family Singleton n :: Constraint
type family Keys n :: Constraint
type family Filter n :: Constraint
type family InsertWith n :: Constraint
type family Insert n :: Constraint
type family Member n :: Constraint

class MapLike k a n | n -> k a where
  adjust :: Adjust n => (a -> a) -> k -> n -> n
  lookup :: Lookup n => k -> n -> Maybe a
  toList :: ToList n => n -> [(k, a)]
  fromList :: FromList n => [(k, a)] -> n
  elems :: Elems n => n -> [a]
  empty :: Empty n => n
  singleton :: Singleton n => k -> a -> n
  keys :: Keys n => n -> [k]
  filter :: Filter n => (a -> Bool) -> n -> n
  insertWith :: InsertWith n => (a -> a -> a) -> k -> a -> n -> n
  insert :: Insert n => k -> a -> n -> n
  member :: Member n => k -> n -> Bool

type instance Lookup [(k, a)] = Eq k
type instance Insert [(k, a)] = (Ord k, Ord a)
type instance Member [(k, a)] = Eq k

instance MapLike k a [(k, a)] where
  adjust = undefined
  lookup = List.lookup
  toList = id
  fromList = id
  elems = fmap snd
  empty = []
  singleton k a = [(k,a)]
  keys = fmap fst
  filter pred = List.filter $ pred . snd
  insertWith = undefined
  insert k a = List.insert (k, a)
  member = (isJust .) . lookup

type instance Adjust (Map k a) = Ord k
type instance Lookup (Map k a) = Ord k
type instance FromList (Map k a) = Ord k
type instance Insert (Map k a) = Ord k
type instance InsertWith (Map k a) = Ord k
type instance Member (Map k a) = Ord k

instance MapLike k a (Map k a)  where
  adjust = Map.adjust
  lookup = Map.lookup
  toList = Map.toList
  fromList = Map.fromList
  elems = Map.elems
  empty = Map.empty
  singleton = Map.singleton
  keys = Map.keys
  filter = Map.filter
  insertWith = Map.insertWith
  insert = Map.insert
  member = Map.member

type instance Adjust (Bimap k a) = (Ord k, Ord a)
type instance Lookup (Bimap k a) = (Ord k, Ord a)
type instance FromList (Bimap k a) = (Ord k, Ord a)
type instance Filter (Bimap k a) = (Ord k, Ord a)
type instance Insert (Bimap k a) = (Ord k, Ord a)
type instance Member (Bimap k a) = (Ord k, Ord a)

instance MapLike k a (Bimap k a)  where
  adjust = Bimap.adjust
  lookup = Bimap.lookup
  toList = Bimap.toList
  fromList = Bimap.fromList
  elems = Bimap.elems
  empty = Bimap.empty
  singleton = Bimap.singleton
  keys = Bimap.keys
  filter = Bimap.filter . const
  insertWith = undefined
  insert = Bimap.insert
  member = Bimap.member

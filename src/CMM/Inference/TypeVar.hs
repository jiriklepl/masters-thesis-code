{-# LANGUAGE Safe #-}

module CMM.Inference.TypeVar where

import safe Prelude

import safe Data.Data (Data)

import safe CMM.Data.Nullable (Fallbackable(..), Nullable(..))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(..)
  , TypeKind(GenericType)
  , setTypeKindInvariantLogicError
  )

data TypeVar
  = NoType
  | TypeVar
      { tVarId :: Int
      , tVarKind :: TypeKind
      , tVarParent :: TypeVar
      }
  deriving (Show, Data)

class FromTypeVar a where
  fromTypeVar :: TypeVar -> a

instance FromTypeVar TypeVar where
  fromTypeVar = id

instance Eq TypeVar where
  NoType == NoType = True
  TypeVar int _ _ == TypeVar int' _ _ = int == int'
  _ == _ = False

instance Ord TypeVar where
  NoType `compare` NoType = EQ
  TypeVar int _ _ `compare` TypeVar int' _ _ = int `compare` int'
  NoType `compare` _ = LT
  _ `compare` NoType = GT

instance Fallbackable TypeVar where
  NoType ?? tVar = tVar
  tVar ?? _ = tVar

instance Nullable TypeVar where
  nullVal = NoType

instance HasTypeKind TypeVar where
  getTypeKind NoType {} = GenericType
  getTypeKind (TypeVar _ kind _) = kind
  setTypeKind GenericType NoType {} = NoType {}
  setTypeKind kind t@NoType {} = setTypeKindInvariantLogicError t kind
  setTypeKind kind tVar@TypeVar {} = tVar {tVarKind = kind}

familyDepth :: TypeVar -> Int
familyDepth NoType = 0
familyDepth TypeVar {tVarParent = parent} = familyDepth parent + 1

predecessor :: TypeVar -> TypeVar -> Bool
predecessor NoType NoType = True
predecessor NoType _ = False
predecessor whose@TypeVar {tVarParent = parent} who
  | whose == who = True
  | otherwise = predecessor parent who

noType :: FromTypeVar a => a
noType = fromTypeVar NoType

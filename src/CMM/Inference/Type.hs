{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Prelude

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe CMM.Data.Nullable (Fallbackable(..))
import safe CMM.Inference.TypeCompl (TypeCompl(..))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(..)
  , TypeKind(..)
  , setTypeKindInvariantLogicError
  )
import safe CMM.Inference.TypeVar (FromTypeVar(..), TypeVar(..))

data Type
  = ErrorType Text
  | VarType TypeVar
  | ComplType (TypeCompl Type)
  deriving (Show, Eq, Ord, Data)

instance Fallbackable Type where
  ErrorType {} ?? a = a
  a ?? _ = a

instance HasTypeKind Type where
  getTypeKind ErrorType {} = GenericType
  getTypeKind (VarType t) = getTypeKind t
  getTypeKind (ComplType t) = getTypeKind t
  setTypeKind GenericType err@ErrorType {} = err
  setTypeKind kind err@ErrorType {} = setTypeKindInvariantLogicError err kind
  setTypeKind kind (VarType t) = VarType $ setTypeKind kind t
  setTypeKind kind (ComplType t) = ComplType $ setTypeKind kind t

class ToType a where
  toType :: a -> Type

instance ToType Type where
  toType = id

instance FromTypeVar Type where
  fromTypeVar = toType

instance ToType TypeVar where
  toType = VarType

instance ToType a => ToType (TypeCompl a) where
  toType = ComplType . fmap toType

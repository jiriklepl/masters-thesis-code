{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Data.Bool (otherwise)
import safe Data.Data (Data)
import safe Data.Eq (Eq((==)))
import safe Data.Function (($), (.), id)
import safe Data.Functor (Functor(fmap))
import safe Data.Ord (Ord)
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.Annot (Annot, takeAnnot)
import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.TypeCompl (TypeCompl)
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind, setTypeKind)
  , TypeKind(GenericType)
  , setTypeKindInvariantLogicError
  )
import safe CMM.Inference.TypeVar (FromTypeVar(fromTypeVar), TypeVar)

data Type
  = ErrorType Text
  | VarType TypeVar
  | ComplType (TypeCompl Type)
  deriving (Show, Eq, Ord, Data)

instance Fallbackable Type where
  ErrorType {} ?? a = a
  a ?? _ = a

instance HasTypeKind Type where
  getTypeKind =
    \case
      ErrorType {} -> GenericType
      VarType t -> getTypeKind t
      ComplType t -> getTypeKind t
  setTypeKind kind =
    \case
      err@ErrorType {}
        | kind == GenericType -> err
        | otherwise -> setTypeKindInvariantLogicError err kind
      VarType t -> VarType $ setTypeKind kind t
      ComplType t -> ComplType $ setTypeKind kind t

class ToType a where
  toType :: a -> Type

instance ToType a => ToType (Annot n a) where
  toType = toType . takeAnnot

instance ToType Type where
  toType = id

instance FromTypeVar Type where
  fromTypeVar = toType

instance ToType TypeVar where
  toType = VarType

instance ToType a => ToType (TypeCompl a) where
  toType = ComplType . fmap toType

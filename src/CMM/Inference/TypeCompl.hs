{-# LANGUAGE Safe #-}

module CMM.Inference.TypeCompl where

import safe Data.Text ( Text )
import safe Data.Data ( Data )
import safe qualified Data.Text as T

import safe CMM.Inference.TypeKind
    ( HasTypeKind(..),
      TypeKind(Star, GenericType, ErrorKind, (:->)),
      setTypeKindInvariantLogicError )
import safe CMM.Inference.TypeVar ( TypeVar(TypeVar, NoType) )
import safe CMM.Utils (backQuote)

data TypeCompl a
  = TupleType [a]
  | FunctionType [a] a
  | AppType a a
  | AddrType a
  | LamType Int TypeKind TypeVar
  | ConstType Text TypeKind TypeVar
  | StringType
  | String16Type
  | LabelType
  | TBitsType Int
  | BoolType
  | VoidType
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data)

toLam :: TypeVar -> TypeCompl a
toLam NoType = undefined
toLam (TypeVar int kind parent) = LamType int kind parent

type PrimType = TypeCompl TypeVar

instance (HasTypeKind a, Show a) => HasTypeKind (TypeCompl a) where
  getTypeKind (AppType t _) =
    case getTypeKind t of
      _ :-> k -> k
      GenericType -> GenericType
      _ ->
        ErrorKind $
        T.pack ("Kind " ++ backQuote (show t) ++ " cannot be applied.")
  getTypeKind (LamType _ kind _) = kind
  getTypeKind (ConstType _ kind _) = kind
  getTypeKind _ = Star
  setTypeKind kind (AppType t t') =
    AppType (setTypeKind (kind :-> getTypeKind t') t) t'
  setTypeKind kind (LamType int _ parent) = LamType int kind parent
  setTypeKind kind (ConstType int _ parent) = ConstType int kind parent
  setTypeKind Star tCompl = tCompl
  setTypeKind kind tCompl = setTypeKindInvariantLogicError tCompl kind

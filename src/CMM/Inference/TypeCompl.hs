{-# LANGUAGE Safe #-}

module CMM.Inference.TypeCompl where

import safe Data.Data (Data)
import safe Data.Eq (Eq ((==)))
import safe Data.Foldable (Foldable)
import safe Data.Function (($))
import safe Data.Functor (Functor)
import safe Data.Int (Int)
import safe Data.List ((++))
import safe Data.Ord (Ord)
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (Traversable)
import safe GHC.Err (undefined)
import safe Text.Show (Show(show))
import safe Data.Bool ( otherwise )

import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind, setTypeKind)
  , TypeKind((:->), ErrorKind, GenericType, Star)
  , setTypeKindInvariantLogicError
  )
import safe CMM.Inference.TypeVar (TypeVar(NoType, TypeVar))
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
toLam = \case
  NoType -> undefined -- logic error
  TypeVar int kind parent -> LamType int kind parent

type PrimType = TypeCompl TypeVar

instance (HasTypeKind a, Show a) => HasTypeKind (TypeCompl a) where
  getTypeKind = \case
    AppType t _ ->
      case getTypeKind t of
        _ :-> k -> k
        GenericType -> GenericType
        _ ->
          ErrorKind $
          T.pack ("Kind " ++ backQuote (show t) ++ " cannot be applied.")
    LamType _ kind _ -> kind
    ConstType _ kind _ -> kind
    _ -> Star
  setTypeKind kind = \case
    AppType t t' ->
      AppType (setTypeKind (kind :-> getTypeKind t') t) t'
    LamType int _ parent -> LamType int kind parent
    ConstType int _ parent -> ConstType int kind parent
    tCompl
      | kind == Star -> tCompl
      | otherwise -> setTypeKindInvariantLogicError tCompl kind

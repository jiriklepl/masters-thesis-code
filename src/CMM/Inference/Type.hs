{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Data.Data (Data)
import safe Data.List (foldl')

import safe Prettyprinter (Pretty(pretty))

import safe CMM.Inference.TypeCompl (TypeCompl(AddrType, AppType, TBitsType, VoidType, LabelType, BoolType))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind, setTypeKind)
  )
import safe CMM.Inference.TypeVar (FromTypeVar(fromTypeVar), TypeVar)

-- | type is either a type variable or a primitive pattern applied to some types
data Type
  = VarType TypeVar
  | ComplType (TypeCompl Type)
  deriving (Show, Eq, Ord, Data)

instance HasTypeKind Type where
  getTypeKind =
    \case
      VarType t -> getTypeKind t
      ComplType t -> getTypeKind t
  setTypeKind kind =
    \case
      VarType t -> VarType $ setTypeKind kind t
      ComplType t -> ComplType $ setTypeKind kind t

-- | class for objects that can be "cast" to type
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

instance Pretty Type where
  pretty =
    \case
      VarType tVar -> pretty tVar
      ComplType tCompl -> pretty tCompl

-- | creates a type application from the given two objects that represent some types
makeAppType :: ToType a => a -> a -> Type
makeAppType f a = ComplType $ toType f `AppType` toType a

-- | creates an address type from the given object representing a type
makeAddrType :: ToType a => a -> Type
makeAddrType = ComplType . AddrType . toType

-- | creates a type that represents a "bits_n" type of the given width "n"
makeTBitsType :: Int -> Type
makeTBitsType = ComplType . TBitsType

-- | creates a type that represents "void"
makeVoidType :: Type
makeVoidType = ComplType VoidType

-- | creates a type that represents "label"
makeLabelType :: Type
makeLabelType = ComplType LabelType

-- | creates a type that represents "bool"
makeBoolType :: Type
makeBoolType = ComplType BoolType

-- | left-associatively folds the given types into a single chain of `makeAppType`
foldApp :: [Type] -> Type
foldApp =
  \case
    t:ts -> foldl' makeAppType t ts
    [] -> ComplType VoidType

-- | unfolds the given chain of type applications into a list of types
unfoldApp :: Type -> [Type]
unfoldApp = reverse . go
  where
    go =
      \case
        ComplType (AppType l r) -> r : go l
        t -> [t]

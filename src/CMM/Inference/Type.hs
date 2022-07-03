{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Data.Data (Data)
import safe Data.List (foldl')
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty), dquotes)

import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.TypeCompl (TypeCompl(AddrType, AppType, TBitsType))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind, setTypeKind)
  , TypeKind(GenericType)
  , setTypeKindInvariantLogicError
  )
import safe CMM.Inference.TypeVar (FromTypeVar(fromTypeVar), TypeVar)

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

makeAppType :: ToType a => a -> a -> Type
makeAppType f a = ComplType $ toType f `AppType` toType a

makeAddrType :: ToType a => a -> Type
makeAddrType = ComplType . AddrType . toType

makeTBitsType :: Int -> Type
makeTBitsType = ComplType . TBitsType

foldApp :: [Type] -> Type
foldApp =
  \case
    t:ts -> foldl' ((ComplType .) . AppType) t ts

unfoldApp :: Type -> [Type]
unfoldApp = reverse . go
  where
    go =
      \case
        ComplType (AppType l r) -> r : go l
        t -> [t]

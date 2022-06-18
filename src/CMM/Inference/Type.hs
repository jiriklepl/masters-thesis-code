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
import safe Data.List ( foldl', reverse )
import safe Data.Int ( Int )

import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.TypeCompl (TypeCompl (AppType, AddrType, TBitsType))
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

instance ToType Type where
  toType = id

instance FromTypeVar Type where
  fromTypeVar = toType

instance ToType TypeVar where
  toType = VarType

instance ToType a => ToType (TypeCompl a) where
  toType = ComplType . fmap toType

makeAppType :: ToType a => a -> a -> Type
makeAppType f a = ComplType $ toType f `AppType` toType a

makeAddrType :: ToType a => a -> Type
makeAddrType = ComplType . AddrType . toType

makeTBitsType :: Int -> Type
makeTBitsType = ComplType . TBitsType

foldApp :: [Type] -> Type
foldApp = \case
  t:ts -> foldl' ((ComplType .) . AppType) t ts
  [] -> ErrorType "Illegal type fold"

unfoldApp :: Type -> [Type]
unfoldApp = reverse . go
  where
    go = \case
      ComplType (AppType l r) -> r : go l
      t -> [t]

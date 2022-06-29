{-# LANGUAGE Safe #-}

module CMM.Inference.TypeVar where

import safe Data.Data (Data)
import safe Data.Foldable ()
import safe Data.Functor ()

import safe Prettyprinter (Pretty(pretty), (<+>), parens)

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind, setTypeKind)
  , TypeKind(GenericType)
  , setTypeKindInvariantLogicError
  )
import safe CMM.Pretty (dollar, emptySet)

data TypeVar
  = NoType
  | TypeVar
      { tVarId :: Int
      , tVarKind :: TypeKind
      , tVarParent :: TypeVar
      }
  deriving (Show, Data)

typeVarIdLast :: TypeKind -> TypeVar -> Int -> TypeVar
typeVarIdLast kind parent int = TypeVar int kind parent

class FromTypeVar a where
  fromTypeVar :: TypeVar -> a

class ToTypeVar a where
  toTypeVar :: a -> TypeVar

instance FromTypeVar TypeVar where
  fromTypeVar = id

instance ToTypeVar TypeVar where
  toTypeVar = id

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
  getTypeKind =
    \case
      NoType {} -> GenericType
      TypeVar _ kind _ -> kind
  setTypeKind kind =
    \case
      t@NoType {}
        | kind == GenericType -> NoType {}
        | otherwise -> setTypeKindInvariantLogicError t kind
      tVar@TypeVar {} -> tVar {tVarKind = kind}

instance Pretty TypeVar where
  pretty =
    parens . \case
      NoType -> emptySet
      TypeVar tId kind parent ->
        case parent of
          NoType -> base
          _ -> base <+> "in" <+> go parent
        where base = dollar <> pretty tId <+> pretty kind
    where
      go =
        \case
          NoType -> mempty
          parent -> dollar <> pretty (tVarId parent) <> go (tVarParent parent)

familyDepth :: TypeVar -> Int
familyDepth =
  \case
    NoType -> 0
    TypeVar {tVarParent = parent} -> familyDepth parent + 1

predecessor :: TypeVar -> TypeVar -> Bool
predecessor NoType NoType = True
predecessor NoType _ = False
predecessor whose@TypeVar {tVarParent = parent} who
  | whose == who = True
  | otherwise = predecessor parent who

overLeaf :: TypeVar -> TypeVar -> Bool
overLeaf NoType = const False
overLeaf x =
  \case
    NoType -> False
    leaf
      | x == leaf -> True
      | otherwise -> x `overLeaf` tVarParent leaf

noType :: FromTypeVar a => a
noType = fromTypeVar NoType

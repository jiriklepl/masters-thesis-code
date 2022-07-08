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

-- | representation of a type variable
data TypeVar
  = NoType
  | TypeVar
      { tVarId :: Int -- ^ the unique identifier of a type variable
      , tVarKind :: TypeKind -- ^ the kind of a type variable
      , tVarParent :: TypeVar -- ^ the parent of the type variable (that represents a context in which the type variable was generated)
      }
  deriving (Show, Data)

-- | a helper class that generates a type variable, but receives the identifier as a last argument
typeVarIdLast :: TypeKind -> TypeVar -> Int -> TypeVar
typeVarIdLast kind parent int = TypeVar int kind parent

-- | class for objects that can be generated from a type variable
class FromTypeVar a where
  fromTypeVar :: TypeVar -> a

-- | class for objects that contain a type variable
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

-- | returns the depth of the sequence of parents of the given type variable
familyDepth :: TypeVar -> Int
familyDepth =
  \case
    NoType -> 0
    TypeVar {tVarParent = parent} -> familyDepth parent + 1

-- | returns `True` iff the right-hand operand is a predecessor of the left-hand operand
predecessor :: TypeVar -> TypeVar -> Bool
predecessor NoType = const False
predecessor x =
  \case
    NoType -> False
    leaf
      | x == leaf -> True
      | otherwise -> x `predecessor` tVarParent leaf

-- | returns an object generated from the NoType variable
noType :: FromTypeVar a => a
noType = fromTypeVar NoType

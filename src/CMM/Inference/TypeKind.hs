{-# LANGUAGE Safe #-}

module CMM.Inference.TypeKind where

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), parens)

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Inference.Arity (Arity(arity))
import safe CMM.Pretty (arrowNice, deltaBig, question, star)
import safe CMM.Utils (HasCallStack, backQuote, logicError)

infixr 6 :->

-- | represents the kind of a type
data TypeKind
  = Star
  | Constraint
  | GenericType -- wildCard
  | TypeKind :-> TypeKind
  deriving (Show, Data)

-- | class for all objects that have a kind
class HasTypeKind a where
  getTypeKind :: a -> TypeKind
  setTypeKind :: TypeKind -> a -> a

instance HasTypeKind TypeKind where
  getTypeKind = id
  setTypeKind = const

instance Eq TypeKind where
  Star == Star = True
  Constraint == Constraint = True
  GenericType == GenericType = True
  (l :-> r) == (l' :-> r') = l == l' && r == r'
  _ == _ = False

instance Ord TypeKind where
  Star `compare` Star = EQ
  Star `compare` _ = LT
  _ `compare` Star = GT
  Constraint `compare` Constraint = EQ
  Constraint `compare` _ = LT
  _ `compare` Constraint = GT
  GenericType `compare` GenericType = EQ
  GenericType `compare` _ = LT
  _ `compare` GenericType = GT
  (l :-> r) `compare` (l' :-> r') =
    case l `compare` l' of
      LT -> LT
      GT -> GT
      EQ -> r `compare` r'

instance Fallbackable TypeKind where
  GenericType ?? kind = kind
  kind ?? _ = kind

instance Nullable TypeKind where
  nullVal = GenericType

instance Arity TypeKind where
  arity =
    \case
      Star -> 0
      Constraint -> 0
      GenericType -> 0 -- if it gets defaulted into Star
      _ :-> kind -> arity kind + 1

instance Pretty TypeKind where
  pretty =
    \case
      Star -> star
      Constraint -> deltaBig
      GenericType -> question
      left :-> right
        | arity left == 0 -> pretty left <> arrowNice <> pretty right
        | otherwise -> parens (pretty left) <> arrowNice <> pretty right

-- | logic error for illegal type kind updates
setTypeKindInvariantLogicError ::
     (HasCallStack, HasTypeKind a, Show a) => a -> TypeKind -> a
setTypeKindInvariantLogicError what kind =
  error $
  "(internal) " ++
  backQuote (show what) ++
  " has to be given the " ++
  backQuote (show (getTypeKind what)) ++
  " kind; attempting to set to: " ++ backQuote (show kind) ++ "."

-- | returns `True` iff the given objects have matching type kinds
matchKind :: (HasTypeKind a, HasTypeKind b) => a -> b -> Bool
matchKind a b = getTypeKind a `go` getTypeKind b
  where
    GenericType `go` _ = True
    _ `go` GenericType = True
    Constraint `go` Constraint = True
    Star `go` Star = True
    (l :-> r) `go` (l' :-> r') = go l l' && go r r'
    _ `go` _ = False

-- | combines the type kinds of the given objects
combineTypeKind ::
     (HasCallStack, HasTypeKind a, HasTypeKind b) => a -> b -> TypeKind
combineTypeKind a b = getTypeKind a `go` getTypeKind b
  where
    kind `go` kind'
      | kind == kind' = kind
    GenericType `go` kind = kind
    kind `go` GenericType = kind
    (l :-> r) `go` (l' :-> r') = go l l' :-> go r r'
    _ `go` _ = logicError

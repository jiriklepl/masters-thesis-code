{-# LANGUAGE Safe #-}

module CMM.Inference.TypeKind where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty), parens)

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))
import safe CMM.Inference.Arity (Arity(arity))
import safe CMM.Pretty (arrowNice, bang, deltaBig, question, star)
import safe CMM.Utils (backQuote)

infixr 6 :->

data TypeKind
  = Star
  | Constraint
  | GenericType -- wildCard
  | ErrorKind Text
  | TypeKind :-> TypeKind
  deriving (Show, Data)

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
  ErrorKind _ == _ = False
  _ == ErrorKind _ = False
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
  ErrorKind s `compare` ErrorKind s' = s `compare` s'
  ErrorKind _ `compare` _ = LT
  _ `compare` ErrorKind _ = GT
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
      ErrorKind {} -> 0 -- it does not matter
      _ :-> kind -> arity kind + 1

instance Pretty TypeKind where
  pretty =
    \case
      Star -> star
      Constraint -> deltaBig
      GenericType -> question
      ErrorKind {} -> bang
      left :-> right
        | arity left == 0 -> pretty left <> arrowNice <> pretty right
        | otherwise -> parens (pretty left) <> arrowNice <> pretty right

setTypeKindInvariantLogicError :: (HasTypeKind a, Show a) => a -> TypeKind -> a
setTypeKindInvariantLogicError what kind =
  error $
  "(internal) " ++
  backQuote (show what) ++
  " has to be given the " ++
  backQuote (show (getTypeKind what)) ++
  " kind; attempting to set to: " ++ backQuote (show kind) ++ "."

matchKind :: (HasTypeKind a, HasTypeKind b) => a -> b -> Bool
matchKind a b = getTypeKind a `go` getTypeKind b
  where
    ErrorKind _ `go` _ = False
    _ `go` ErrorKind _ = False
    GenericType `go` _ = True
    _ `go` GenericType = True
    Constraint `go` Constraint = True
    Star `go` Star = True
    (l :-> r) `go` (l' :-> r') = go l l' && go r r'
    _ `go` _ = False

combineTypeKind :: (HasTypeKind a, HasTypeKind b) => a -> b -> TypeKind
combineTypeKind a b = getTypeKind a `go` getTypeKind b
  where
    kind `go` kind'
      | kind == kind' = kind
    kind@ErrorKind {} `go` _ = kind
    _ `go` kind@ErrorKind {} = kind
    GenericType `go` kind = kind
    kind `go` GenericType = kind
    (l :-> r) `go` (l' :-> r') = go l l' :-> go r r'
    _ `go` _ = undefined

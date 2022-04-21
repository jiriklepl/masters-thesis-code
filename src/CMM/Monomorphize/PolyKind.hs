{-# LANGUAGE Safe #-}

module CMM.Monomorphize.PolyKind where

import safe Data.Eq (Eq)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Ord (Ord)
import safe Data.Semigroup (Semigroup((<>)))

import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))

data PolyKind
  = Mono
  | Poly
  | Absurd
  deriving (Eq, Ord)

instance Semigroup PolyKind where
  Absurd <> _ = Absurd
  _ <> Absurd = Absurd
  Poly <> _ = Poly
  _ <> Poly = Poly
  Mono <> Mono = Mono

instance Monoid PolyKind where
  mempty = Mono

instance Fallbackable PolyKind where
  Absurd ?? kind = kind
  kind ?? _ = kind

instance Nullable PolyKind where
  nullVal = Absurd

{-# LANGUAGE Safe #-}

module CMM.Monomorphize.PolyKind where

import safe Prelude

import safe CMM.Data.Nullable (Fallbackable(..), Nullable(..))

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

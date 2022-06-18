{-# LANGUAGE Safe #-}

module CMM.Monomorphize.PolyKind where

import safe Data.Eq (Eq)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Ord (Ord)
import safe Data.Semigroup (Semigroup((<>)))
import safe Text.Show ( Show )
import safe Data.Function ( ($) )

import safe CMM.Data.Nullable (Fallbackable((??)))

data Absurdity
  = AbsurdKind
  | AbsurdConst
  | AbsurdKindConst
  deriving (Eq, Ord, Show)

instance Semigroup Absurdity where
  AbsurdKind <> AbsurdKind = AbsurdKind
  AbsurdConst <> AbsurdConst = AbsurdConst
  AbsurdKindConst <> _ = AbsurdKindConst
  _ <> AbsurdKindConst = AbsurdKindConst
  AbsurdKind <> AbsurdConst = AbsurdKindConst
  AbsurdConst <> AbsurdKind = AbsurdKindConst

data PolyKind
  = Mono
  | Poly
  | Absurd Absurdity
  deriving (Eq, Ord, Show)

instance Semigroup PolyKind where
  Absurd l' <> Absurd r' = Absurd $ l' <> r'
  l@Absurd {} <> _ = l
  _ <> r@Absurd {} = r
  Poly <> _ = Poly
  _ <> Poly = Poly
  Mono <> Mono = Mono

instance Monoid PolyKind where
  mempty = Mono

instance Fallbackable PolyKind where
  Absurd {} ?? kind = kind
  kind ?? _ = kind

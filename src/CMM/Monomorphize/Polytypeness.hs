{-# LANGUAGE Safe #-}

module CMM.Monomorphize.Polytypeness where

import safe Data.Eq (Eq)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Ord (Ord)
import safe Data.Semigroup (Semigroup((<>)))
import safe Text.Show ( Show )
import safe Data.Function ( ($) )
import safe Data.Bool

import safe CMM.Data.Nullable (Fallbackable((??)))

data Absurdity
  = Absurdity { absurdKind :: Bool, absurdConst :: Bool }
  deriving (Eq, Ord, Show)

instance Semigroup Absurdity where
  Absurdity {absurdKind = kind, absurdConst = const} <> Absurdity {absurdKind = kind', absurdConst = const'} =
    Absurdity{absurdKind = kind || kind', absurdConst = const || const'}

kindAbsurdity :: Polytypeness
kindAbsurdity = Absurd $ Absurdity {absurdKind = True, absurdConst = False}

constAbsurdity :: Polytypeness
constAbsurdity = Absurd $ Absurdity {absurdKind = False, absurdConst = True}

data PolyWhat
  = PolyWhat {polyKind :: Bool, polyConst :: Bool, polyType :: Bool}
  deriving (Eq, Ord, Show)

instance Semigroup PolyWhat where
  PolyWhat {polyKind = kind, polyConst = const, polyType = typ} <> PolyWhat {polyKind = kind', polyConst = const', polyType = typ'} =
    PolyWhat {polyKind = kind || kind', polyConst = const || const', polyType = typ || typ'}

kindPolymorphism :: Polytypeness
kindPolymorphism = Poly $ PolyWhat {polyKind = True, polyConst = False, polyType = False}

constPolymorphism :: Polytypeness
constPolymorphism = Poly $ PolyWhat {polyKind = False, polyConst = True, polyType = False}

typePolymorphism :: Polytypeness
typePolymorphism = Poly $ PolyWhat {polyKind = False, polyConst = False, polyType = True}

data Polytypeness
  = Mono
  | Poly PolyWhat
  | Absurd Absurdity
  deriving (Eq, Ord, Show)

instance Semigroup Polytypeness where
  Absurd l <> Absurd r = Absurd $ l <> r
  l@Absurd {} <> _ = l
  _ <> r@Absurd {} = r
  Poly l <> Poly r = Poly $ l <> r
  l@Poly {} <> _ = l
  _ <> r@Poly {} = r
  Mono <> Mono = Mono

instance Monoid Polytypeness where
  mempty = Mono

instance Fallbackable Polytypeness where
  Absurd {} ?? kind = kind
  kind ?? _ = kind

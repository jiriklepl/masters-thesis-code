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
import CMM.Inference.DataKind
import CMM.Data.Bounds
import CMM.Inference.Constness
import CMM.Inference.TypeVar
import Data.Set
import Control.Applicative
import CMM.Inference.Type

data Absurdity
  = Absurdity { absurdKind :: [(TypeVar, Bounds DataKind)], absurdConst :: [(TypeVar, Bounds Constness)] }
  deriving (Eq, Show)

instance Semigroup Absurdity where
  Absurdity {absurdKind = kind, absurdConst = const} <> Absurdity {absurdKind = kind', absurdConst = const'} =
    Absurdity{absurdKind = kind <> kind', absurdConst = const <> const'}

kindAbsurdity :: TypeVar -> Bounds DataKind -> Polytypeness
kindAbsurdity tVar bounds = Absurd $ Absurdity {absurdKind = pure (tVar, bounds), absurdConst = mempty}

constAbsurdity :: TypeVar -> Bounds Constness -> Polytypeness
constAbsurdity tVar bounds = Absurd $ Absurdity {absurdKind = mempty, absurdConst = pure (tVar, bounds)}

data PolyWhat
  = PolyWhat {polyKind :: [(TypeVar, Bounds DataKind)], polyConst :: [(TypeVar, Bounds Constness)], polyType :: [(Type, Set TypeVar)]}
  deriving (Eq, Show)

instance Semigroup PolyWhat where
  PolyWhat {polyKind = kind, polyConst = const, polyType = typ} <> PolyWhat {polyKind = kind', polyConst = const', polyType = typ'} =
    PolyWhat {polyKind = kind <> kind', polyConst = const <> const', polyType = typ <> typ'}

kindPolymorphism :: TypeVar -> Bounds DataKind -> Polytypeness
kindPolymorphism tVar bounds = Poly $ PolyWhat {polyKind = pure (tVar, bounds), polyConst = mempty, polyType = mempty}

constPolymorphism :: TypeVar -> Bounds Constness -> Polytypeness
constPolymorphism tVar bounds = Poly $ PolyWhat {polyKind = mempty, polyConst = pure (tVar, bounds), polyType = mempty}

typePolymorphism :: Type -> Set TypeVar -> Polytypeness
typePolymorphism t tVars = Poly $ PolyWhat {polyKind = mempty, polyConst = mempty, polyType = pure (t, tVars)}

data Polytypeness
  = Mono
  | Poly PolyWhat
  | Absurd Absurdity
  deriving (Eq, Show)

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

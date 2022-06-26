{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Polytypeness where

import safe Control.Applicative (Applicative(pure))
import safe Data.Eq (Eq)
import safe Data.Function (($), (.))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Set (Set)
import safe Text.Show (Show)
import safe Data.Data (Data)
import safe qualified Data.Set as Set
import safe Data.Functor ( Functor(fmap), (<$>) )
import safe Data.Semigroup ( Semigroup((<>)) )

import safe Prettyprinter
    ( Pretty(pretty), (<+>), list, Doc )

import safe CMM.Data.Bounds (Bounds)
import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Type (Type, ToType)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Fact ( constnessBounds, kindingBounds )
import safe CMM.Pretty ( lambda )
import safe CMM.Inference.BuiltIn()

data Absurdity =
  Absurdity
    { absurdKind :: [(TypeVar, Bounds DataKind)]
    , absurdConst :: [(TypeVar, Bounds Constness)]
    }
  deriving (Eq, Show, Data)

instance Semigroup Absurdity where
  Absurdity {absurdKind = kind, absurdConst = const} <> Absurdity { absurdKind = kind'
                                                                  , absurdConst = const'
                                                                  } =
    Absurdity {absurdKind = kind <> kind', absurdConst = const <> const'}

instance Pretty Absurdity where
  pretty Absurdity {absurdKind, absurdConst} =
    "unsolvable type constraints:" <+> list goAll
    where
      goAll = fmap goKind absurdKind <> fmap goConst absurdConst

kindAbsurdity :: TypeVar -> Bounds DataKind -> Polytypeness
kindAbsurdity tVar bounds =
  Absurd $ Absurdity {absurdKind = pure (tVar, bounds), absurdConst = mempty}

constAbsurdity :: TypeVar -> Bounds Constness -> Polytypeness
constAbsurdity tVar bounds =
  Absurd $ Absurdity {absurdKind = mempty, absurdConst = pure (tVar, bounds)}

data PolyWhat =
  PolyWhat
    { polyKind :: [(TypeVar, Bounds DataKind)]
    , polyConst :: [(TypeVar, Bounds Constness)]
    , polyType :: [(Type, Set TypeVar)]
    }
  deriving (Eq, Show, Data)

instance Semigroup PolyWhat where
  PolyWhat {polyKind = kind, polyConst = const, polyType = typ} <> PolyWhat { polyKind = kind'
                                                                            , polyConst = const'
                                                                            , polyType = typ'
                                                                            } =
    PolyWhat
      { polyKind = kind <> kind'
      , polyConst = const <> const'
      , polyType = typ <> typ'
      }

instance Pretty PolyWhat where
  pretty PolyWhat {polyKind, polyConst, polyType} =
    "allowed type freedom:" <+> list goAll
    where
      goAll = fmap goKind polyKind <> fmap goConst polyConst <> fmap goTyping polyType
      goTyping (_, tVars) = list $ (lambda<>) . pretty <$> Set.toList tVars

goKind :: (ToType a, Pretty DataKind) => (a, Bounds DataKind) -> Doc ann
goKind (tVar, kindBounds) = pretty $ kindingBounds kindBounds tVar

goConst :: (ToType a, Pretty DataKind) => (a, Bounds Constness) -> Doc ann
goConst (tVar, constBounds) = pretty $ constnessBounds constBounds tVar

kindPolymorphism :: TypeVar -> Bounds DataKind -> Polytypeness
kindPolymorphism tVar bounds =
  Poly $
  PolyWhat
    {polyKind = pure (tVar, bounds), polyConst = mempty, polyType = mempty}

constPolymorphism :: TypeVar -> Bounds Constness -> Polytypeness
constPolymorphism tVar bounds =
  Poly $
  PolyWhat
    {polyKind = mempty, polyConst = pure (tVar, bounds), polyType = mempty}

typePolymorphism :: Type -> Set TypeVar -> Polytypeness
typePolymorphism t tVars =
  Poly $
  PolyWhat {polyKind = mempty, polyConst = mempty, polyType = pure (t, tVars)}

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

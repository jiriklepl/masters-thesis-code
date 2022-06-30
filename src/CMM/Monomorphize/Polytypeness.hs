{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Polytypeness where

import safe Data.Set (Set)
import safe Data.Data (Data)
import safe qualified Data.Set as Set
import safe qualified Data.PartialOrd as PartialOrd

import safe Prettyprinter
    ( Pretty(pretty), (<+>), list, Doc )

import safe CMM.Data.Bounds (Bounds (Bounds))
import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.Constness (Constness (LinkExpr))
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Type (Type, ToType)
import safe CMM.Inference.TypeVar (TypeVar, ToTypeVar (toTypeVar))
import safe CMM.Inference.Fact ( constnessBounds, kindingBounds )
import safe CMM.Pretty ( lambda )
import safe CMM.Inference.BuiltIn()
import safe CMM.Inference.State (Inferencer)
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.Subst ( Apply(apply), Subst )
import safe CMM.Inference ( simplify )
import safe CMM.Inference.FreeTypeVars ( freeTypeVars )
import Data.Functor ((<&>))
import CMM.Inference.Preprocess.TypeHole

data Absurdity =
  Absurdity
    { absurdKind :: [(TypeVar, Bounds DataKind)]
    , absurdConst :: [(TypeVar, Bounds Constness)]
    }
  deriving (Eq, Show, Data)

instance Semigroup Absurdity where
  Absurdity {absurdKind = kind, absurdConst = const'} <> Absurdity { absurdKind = kind'
                                                                  , absurdConst = const''
                                                                  } =
    Absurdity {absurdKind = kind <> kind', absurdConst = const' <> const''}

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
  PolyWhat {polyKind = kind, polyConst = const', polyType = typ} <> PolyWhat { polyKind = kind'
                                                                            , polyConst = const''
                                                                            , polyType = typ'
                                                                            } =
    PolyWhat
      { polyKind = kind <> kind'
      , polyConst = const' <> const''
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

typingPolytypeness :: Type -> Polytypeness
typingPolytypeness t =
  if null free
    then Mono
    else typePolymorphism t free
  where
    free = freeTypeVars t

constnessPolytypeness :: TypeVar -> Inferencer Polytypeness
constnessPolytypeness tVar = go <$> State.readConstingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      case low `compare` high of
        LT ->
          if low >= LinkExpr
            then Mono
            else constPolymorphism tVar bounds
        EQ -> Mono
        GT -> constAbsurdity tVar bounds

kindingPolytypeness :: TypeVar -> Inferencer Polytypeness
kindingPolytypeness tVar = go <$> State.readKindingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else kindPolymorphism tVar bounds
        else kindAbsurdity tVar bounds

reconstructHole :: HasTypeHole a => Subst Type -> a -> Inferencer a
reconstructHole subst holed =
  case getTypeHole holed of
    EmptyTypeHole -> return holed
    hole -> do
      newHandle <- reconstructType subst hole >>= simplify >>= State.getHandle
      return $ setTypeHole (setHoleHandle newHandle hole) holed

reconstructBase :: ToTypeVar a => Subst Type -> a -> Inferencer TypeVar
reconstructBase subst tVar =
  State.reconstructOld (toTypeVar tVar) >>= simplify . apply subst

reconstructType :: ToTypeVar a => Subst Type -> a -> Inferencer Type
reconstructType subst tVar =
  reconstructBase subst tVar >>= fmap (apply subst) . State.getTyping

typePolytypeness :: ToTypeVar a => Subst Type -> a -> Inferencer Polytypeness
typePolytypeness subst tVar =
  reconstructType subst tVar <&> typingPolytypeness

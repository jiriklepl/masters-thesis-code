{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Polytypeness where

import safe Control.Applicative (Applicative(liftA2))
import safe Data.Set (Set)
import safe Data.Data (Data)
import safe qualified Data.Set as Set
import safe qualified Data.PartialOrd as PartialOrd
import safe Control.Lens.Getter ((^.))

import safe Prettyprinter
    ( Pretty(pretty), (<+>), list, Doc )

import safe CMM.Data.Bounds (Bounds (Bounds))
import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference.Constness (Constness (LinkExpr))
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Type (Type, ToType)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Fact ( constnessBounds, kindingBounds )
import safe CMM.Pretty ( lambda )
import safe CMM.Inference.BuiltIn()
import safe CMM.Inference.State (Inferencer)
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.Subst ( Apply(apply), Subst )
import safe CMM.Inference.Preprocess.HasTypeHandle
    ( getTypeHandleId, HasTypeHandle )
import safe CMM.Inference.TypeHandle ( typing, kinding, consting )
import safe CMM.Inference ( simplify )
import safe CMM.Utils ( backQuote )
import safe CMM.Inference.FreeTypeVars ( freeTypeVars )

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
            else Mono
        EQ -> Mono
        GT -> Mono

kindingPolytypeness :: TypeVar -> Inferencer Polytypeness
kindingPolytypeness tVar = go <$> State.readKindingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else Mono
        else Mono

typePolytypeness :: Subst Type -> TypeVar -> Inferencer Polytypeness
typePolytypeness subst tVar =
  State.reconstructOld tVar >>= simplify . apply subst >>= State.tryGetHandle >>= \case
    Just handle ->
      mappend (typingPolytypeness $ apply subst handle ^. typing) <$>
      liftA2
        mappend
        (kindingPolytypeness $ apply subst handle ^. kinding)
        (constnessPolytypeness $ apply subst handle ^. consting)
    Nothing ->
      error $
      "(internal logic error) Type variable " <>
      backQuote (show tVar) <> " not registered by the inferencer."

getTypeHandleIdPolytypeness ::
     HasTypeHandle a => Subst Type -> a -> Inferencer Polytypeness
getTypeHandleIdPolytypeness subst = typePolytypeness subst . getTypeHandleId

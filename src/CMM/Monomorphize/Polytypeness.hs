{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize.Polytypeness where

import safe Data.Data (Data)
import safe Data.Functor ((<&>))
import safe qualified Data.PartialOrd as PartialOrd
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe Prettyprinter (Doc, Pretty(pretty), (<+>), list)

import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Nullable (Fallbackable((??)))
import safe CMM.Inference (simplify)
import safe CMM.Inference.BuiltIn ()
import safe CMM.Inference.Constness (Constness(LinkExpr))
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (constnessBounds, kindingBounds)
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.Preprocess.Elaboration
  ( Elaboration(EmptyElaboration)
  , HasElaboration(getElaboration, setElaboration)
  , setElabHandle
  )
import safe CMM.Inference.State (Inferencer)
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.Subst (Apply(apply), Subst)
import safe CMM.Inference.Type (ToType, Type)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (lambda)

-- | Contains the various absurd bounds (data kind or constness bounds) for an type
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
      goAll = fmap prettyKind absurdKind <> fmap prettyConst absurdConst

-- | initializes a `Polytypeness` object with an information about absurd data kind bounds for the given type represented by a type variable
kindAbsurdity :: TypeVar -> Bounds DataKind -> Polytypeness
kindAbsurdity tVar bounds =
  Absurd $ Absurdity {absurdKind = pure (tVar, bounds), absurdConst = mempty}

-- | initializes a `Polytypeness` object with an information about absurd constness bounds for the given type represented by a type variable
constAbsurdity :: TypeVar -> Bounds Constness -> Polytypeness
constAbsurdity tVar bounds =
  Absurd $ Absurdity {absurdKind = mempty, absurdConst = pure (tVar, bounds)}

-- | the object that contains the information about various polytype freedoms of a type
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
    "type freedom:" <+> list goAll
    where
      goAll =
        fmap prettyKind polyKind <>
        fmap prettyConst polyConst <> fmap goTyping polyType
      goTyping (_, tVars) = list $ (lambda <>) . pretty <$> Set.toList tVars

-- | prettyPrints an information about a type variable's data kind bounds
prettyKind :: (ToType a, Pretty DataKind) => (a, Bounds DataKind) -> Doc ann
prettyKind (tVar, kindBounds) = pretty $ kindingBounds kindBounds tVar

-- | prettyPrints an information about a type variable's constness bounds
prettyConst :: (ToType a, Pretty DataKind) => (a, Bounds Constness) -> Doc ann
prettyConst (tVar, constBounds) = pretty $ constnessBounds constBounds tVar

-- | initializes a `Polytypeness` object containing an information about a data kind freedom of a type represented by a type variable
kindPolymorphism :: TypeVar -> Bounds DataKind -> Polytypeness
kindPolymorphism tVar bounds =
  Poly $
  PolyWhat
    {polyKind = pure (tVar, bounds), polyConst = mempty, polyType = mempty}

-- | initializes a `Polytypeness` object containing an information about a constness freedom of a type represented by a type variable
constPolymorphism :: TypeVar -> Bounds Constness -> Polytypeness
constPolymorphism tVar bounds =
  Poly $
  PolyWhat
    {polyKind = mempty, polyConst = pure (tVar, bounds), polyType = mempty}

-- | initializes a `Polytypeness` object containing an information about a typing freedom of a type represented by a type variable
typePolymorphism :: Type -> Set TypeVar -> Polytypeness
typePolymorphism t tVars =
  Poly $
  PolyWhat {polyKind = mempty, polyConst = mempty, polyType = pure (t, tVars)}

-- | An object containing polytype freedoms,
--   absurdities (absurdities overwrite polytype freedoms),
--   or an information about a type being a monotype
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

-- | returns either `Mono` or `Poly` with `typePolymorphism` applied to the list of free type variables in the given type
typingPolytypeness :: Type -> Polytypeness
typingPolytypeness t =
  if null free
    then Mono
    else typePolymorphism t free
  where
    free = freeTypeVars t

-- | returns either `Mono`, `Poly` with `constPolymorphism` with the free constness bounds,
--   or similarly Absurd with `constAbsurdity`
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

-- | returns either `Mono`, `Poly` with `kindPolymorphism` with the free constness bounds,
--   or similarly Absurd with `kindAbsurdity`
kindingPolytypeness :: TypeVar -> Inferencer Polytypeness
kindingPolytypeness tVar = go <$> State.readKindingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else kindPolymorphism tVar bounds
        else kindAbsurdity tVar bounds

-- | reconstruct the elaboration of the given object according to the given substitution,
--   it is like applying the substitution on the corresponding type, but it uses the monadic `InferencerState`
reconstructHole :: HasElaboration a => Subst Type -> a -> Inferencer a
reconstructHole subst elab =
  case getElaboration elab of
    EmptyElaboration -> return elab
    hole -> do
      props <- reconstructType subst hole >>= simplify >>= State.getProps
      return $ setElaboration (setElabHandle props hole) elab

-- | reconstructs the type from the given type variable after renaming it to its alive representant
--   and applying the given substitution
reconstructBase :: ToTypeVar a => Subst Type -> a -> Inferencer TypeVar
reconstructBase subst tVar =
  State.reconstructOld (toTypeVar tVar) >>= simplify . apply subst

-- | uses `reconstructBase`, then gets the typing of the resulting type and applies substitution to that as well
reconstructType :: ToTypeVar a => Subst Type -> a -> Inferencer Type
reconstructType subst tVar =
  reconstructBase subst tVar >>= fmap (apply subst) . State.getTyping

-- | returns the `Polytypeness` object that contains the information about the type being monotype, polytype, or absurd
--   and it uses the fields of the respective cases of `Polytypeness` to give more reasons
typePolytypeness :: ToTypeVar a => Subst Type -> a -> Inferencer Polytypeness
typePolytypeness subst tVar = reconstructType subst tVar <&> typingPolytypeness

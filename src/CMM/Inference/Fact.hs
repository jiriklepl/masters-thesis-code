{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module CMM.Inference.Fact where

import safe Data.Data (Data)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe Prettyprinter
  ( Pretty(pretty)
  , (<+>)
  , dot
  , list
  , parens
  , squotes
  , tupled
  )

import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Ordered (Ordered(Ordered))
import safe CMM.Data.Trilean (Trilean)
import safe CMM.Inference.Constness (Constness(ConstExpr, LinkExpr, Regular))
import safe CMM.Inference.Type (ToType(toType), Type)
import safe CMM.Inference.TypeKind (HasTypeKind(getTypeKind, setTypeKind))
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Utils (trileanSeq)
import safe CMM.Pretty
  ( constingSymbol
  , darrow
  , instSymbol
  , isIn
  , kindingSymbol
  , lambda
  , regingSymbol
  , typingSymbol
  )
import safe CMM.Inference.DataKind ( DataKind )

infix 6 :=>

-- | Represents a qualified `a`, `a` with some constraints
data Qual a =
  FlatFacts :=> a
  deriving (Show, Eq, Ord, Data)

instance HasTypeKind a => HasTypeKind (Qual a) where
  getTypeKind (_ :=> t) = getTypeKind t
  setTypeKind kind (facts :=> t) = facts :=> setTypeKind kind t

instance (Pretty a, Pretty DataKind) => Pretty (Qual a) where
  pretty =
    \case
      facts :=> nested -> tupled (pretty <$> facts) <+> darrow <+> pretty nested

infix 5 :.

-- | Qualified and Quantified `a`
data Scheme a =
  Set TypeVar :. Qual a
  deriving (Show, Eq, Ord, Data)

instance HasTypeKind a => HasTypeKind (Scheme a) where
  getTypeKind (_ :. t) = getTypeKind t
  setTypeKind kind (tVars :. t) = tVars :. setTypeKind kind t

instance (Pretty a, Pretty DataKind) => Pretty (Scheme a) where
  pretty =
    \case
      tVars :. qual ->
        lambda <+> list (pretty <$> Set.toList tVars) <+> dot <+> pretty qual

-- | Flat fact is a non-nested fact that specifies some flat constraints on type inference
data FlatFact a
  = SubType a a
  -- ^ Specifies that the left-hand operand is a super-type of the right-hand operand
  | Equality a a
  -- ^ TODO binds the type to a type
  | TypingEquality a a
  -- ^ states that the type follows a certain typing
  | FactComment Text
  -- ^ fact with zero semantics
  | KindBounds (Bounds (Ordered DataKind)) a
  -- ^ lower bound on the kind of the type variable
  | ConstnessBounds (Bounds Constness) a
  -- ^ lower bound on the constness of the type variable
  | OnRegister Text a
  -- ^ states that the type variable stores its data to a certain register
  | SubKind a a
  -- ^ TODO superKind; subKind
  | KindEquality a a
  | SubConst a a
  -- ^ TODO superConst; subConst
  | ConstEquality a a
  | Lock a
  | InstType a a
  -- ^ TODO polytype; monotype
  | ClassConstraint Text a
  -- ^ TODO
  | ClassFact Text a
  -- ^ TODO
  | ClassFunDeps Text [[Trilean]]
  -- ^ TODO: False = from, True = to, Unknown = invariant
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data)

instance (Pretty a, Pretty DataKind) => Pretty (FlatFact a) where
  pretty =
    \case
      SubType sup sub -> pretty sub <+> "≤" <+> pretty sup
      Equality t t' -> pretty t <+> "~" <+> pretty t'
      TypingEquality t t' -> pretty t <+> "~" <> typingSymbol <+> pretty t'
      KindBounds bounds t ->
        pretty t <+> isIn <> kindingSymbol <+> pretty bounds
      ConstnessBounds bounds t ->
        pretty t <+> isIn <> constingSymbol <+> pretty bounds
      OnRegister reg t -> pretty t <+> "~" <> regingSymbol <+> pretty reg
      SubKind sup sub -> pretty sub <+> "≤" <> kindingSymbol <+> pretty sup
      KindEquality sup sub -> pretty sub <+> "~" <> kindingSymbol <+> pretty sup
      FactComment txt -> squotes . squotes . squotes $ pretty txt
      SubConst sup sub -> pretty sub <+> "≤" <> constingSymbol <+> pretty sup
      ConstEquality sup sub -> pretty sub <+> "~" <> constingSymbol <+> pretty sup
      Lock t -> "lock" <+> pretty t
      InstType poly mono -> pretty poly <+> instSymbol <+> pretty mono
      ClassConstraint name t -> pretty name <> "?" <> parens (pretty t)
      ClassFact name t -> pretty name <> "!" <> parens (pretty t)
      ClassFunDeps name rules ->
        pretty name <> ":" <+> pretty (fmap trileanSeq rules)

-- | A nested fact that specifies constraints on type inference, typically used for schemes
data NestedFact a
  = Fact (FlatFact a)
  | NestedFact (Scheme [NestedFact a])
  deriving (Show, Eq, Ord, Data)

type FlatFacts = [FlatFact Type]

type Fact = NestedFact Type

type Facts = [Fact]

instance (Pretty a, Pretty DataKind) => Pretty (NestedFact a) where
  pretty =
    \case
      Fact flatFact -> pretty flatFact
      NestedFact scheme -> pretty scheme

forall :: Set TypeVar -> FlatFacts -> Facts -> Fact
forall s fs f
  | null s = NestedFact $ mempty :. fs :=> f -- the trivial case
forall s fs f = NestedFact $ s :. fs :=> f

-- | States that the given `TypeVar` type variable is unified with the given `Type`
typeEquality :: (ToType a, ToType b) => a -> b -> FlatFact Type
typeEquality t t' = toType t `Equality` toType t'

lockFact :: ToType a => a -> FlatFact Type
lockFact = Lock . toType

-- | States that the given `TypeVar` type variable follows the typing dictated by the `Type` (note that this does not imply type unification nor any subtyping)
typingEquality :: (ToType a, ToType b) => a -> b -> FlatFact Type
typingEquality t t' = toType t `TypingEquality` toType t'

-- | States that the given `TypeVar` type variable is subtyped by another `TypeVar` type variable
subType :: (ToType a, ToType b) => a -> b -> FlatFact Type
subType t t' = toType t `SubType` toType t'

subKind :: (ToType a, ToType b) => a -> b -> FlatFact Type
subKind t t' = toType t `SubKind` toType t'

kindEquality :: (ToType a, ToType b) => a -> b -> FlatFact Type
kindEquality t t' = toType t `KindEquality` toType t'

subConst :: (ToType a, ToType b) => a -> b -> FlatFact Type
subConst t t' = toType t `SubConst` toType t'

constEquality :: (ToType a, ToType b) => a -> b -> FlatFact Type
constEquality t t' = toType t `ConstEquality` toType t'

-- | States that the given `TypeVar` type variable is instantiated into another `TypeVar` type variable
instType :: (ToType a, ToType b) => a -> b -> FlatFact Type
instType t t' = toType t `InstType` toType t'

kindingBounds :: ToType a => Bounds DataKind -> a -> FlatFact Type
kindingBounds (low `Bounds` high) = (. toType) . KindBounds $ Ordered low `Bounds` Ordered high

-- | States the minimum kind `DataKind` of a `TypeVar` type variable
minKindConstraint :: ToType a => DataKind -> a -> FlatFact Type
minKindConstraint = kindingBounds . (`Bounds` maxBound)

-- | States the maximum kind `DataKind` of a `TypeVar` type variable
maxKindConstraint :: ToType a => DataKind -> a -> FlatFact Type
maxKindConstraint = kindingBounds . (minBound `Bounds`)

unstorableConstraint :: ToType a => a -> FlatFact Type
unstorableConstraint = KindBounds (minBound `Bounds` minBound) . toType

constnessBounds :: ToType a => Bounds Constness -> a -> FlatFact Type
constnessBounds = (. toType) . ConstnessBounds

-- | States that the given `TypeVar` (type variable) is a compile-time expression
constExprConstraint :: ToType a => a -> FlatFact Type
constExprConstraint = constnessBounds $ Bounds ConstExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a link-time expression
linkExprConstraint :: ToType a => a -> FlatFact Type
linkExprConstraint = constnessBounds $ Bounds ConstExpr LinkExpr

-- | States that the given `TypeVar` (type variable) is a regular (aka. run-time) expression
regularExprConstraint :: ToType a => a -> FlatFact Type
regularExprConstraint = constnessBounds $ Bounds Regular Regular

-- | States that the given `TypeVar` type variables is to be allocated to the register given by the given `Text` (this is a stronger version of `minKindConstraint`)
registerConstraint :: ToType a => Text -> a -> FlatFact Type
registerConstraint = (. toType) . OnRegister

kindConstraint :: ToType a => DataKind -> a -> FlatFact Type
kindConstraint kind = kindingBounds $ kind `Bounds` kind

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` properties
classConstraint :: ToType a => Text -> a -> FlatFact Type
classConstraint name t = name `ClassConstraint` toType t

classFunDeps :: Text -> [[Trilean]] -> FlatFact Type
classFunDeps = ClassFunDeps

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` properties
classFact :: ToType a => Text -> a -> FlatFact Type
classFact name t = name `ClassFact` toType t

#ifdef FACT_COMMENTS
factComment :: Text -> [FlatFact Type]
factComment = pure . FactComment
#else
factComment :: Text -> [FlatFact Type]
factComment _ = []
#endif

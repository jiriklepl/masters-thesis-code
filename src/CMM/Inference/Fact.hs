{-# LANGUAGE Safe #-}

module CMM.Inference.Fact where

import safe Prelude (Bounded(maxBound, minBound))

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable(null))
import safe Data.Function (($), (.))
import safe Data.Functor (Functor)
import safe Data.Int (Int)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Ord (Ord)
import safe Data.Set (Set)
import safe Data.Text (Text)
import safe Data.Traversable (Traversable)
import safe Text.Show (Show)

import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Ordered (Ordered(Ordered))
import safe CMM.Inference.Constness (Constness(..))
import safe CMM.Inference.DataKind (DataKind(FunctionKind, TupleKind))
import safe CMM.Inference.Type (ToType(..), Type)
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AppType, FunctionType, TupleType)
  )
import safe CMM.Inference.TypeKind (HasTypeKind(..))
import safe CMM.Inference.TypeVar (TypeVar)

infix 6 :=>

data Qual a =
  FlatFacts :=> a
  deriving (Show, Eq, Ord, Data)

instance HasTypeKind a => HasTypeKind (Qual a) where
  getTypeKind (_ :=> t) = getTypeKind t
  setTypeKind kind (facts :=> t) = facts :=> setTypeKind kind t

infix 5 :.

data Scheme a =
  Set TypeVar :. Qual a
  deriving (Show, Eq, Ord, Data)

instance HasTypeKind a => HasTypeKind (Scheme a) where
  getTypeKind (_ :. t) = getTypeKind t
  setTypeKind kind (tVars :. t) = tVars :. setTypeKind kind t

data FlatFact a
  = SubType a a -- supertype; subtype
  | Union a a -- binds the type to a type
  | ClassUnion a a -- binds the type to a type
  | InstanceUnion a a -- binds the type to a type
  | Typing a a -- states that the type follows a certain typing
  | KindBounds (Bounds (Ordered DataKind)) a -- lower bound on the kind of the type variable
  | ConstnessBounds (Bounds Constness) a -- lower bound on the constness of the type variable
  | OnRegister Text a -- states that the type variable stores its data to a certain register
  | SubKind a a -- superKind; subKind
  | SubConst a a -- superConst; subConst
  | InstType a a -- polytype; monotype
  | ClassConstraint Text a
  | ClassFact Text a
  | ClassDetermine Text a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data)

data NestedFact a
  = Fact (FlatFact a)
  | NestedFact (Scheme [NestedFact a])
  deriving (Show, Eq, Ord, Data)

type FlatFacts = [FlatFact Type]

type Fact = NestedFact Type

type Facts = [Fact]

-- | Transforms the two given `Type`s into a function `Type`
makeFunction :: [a] -> a -> TypeCompl a
makeFunction = FunctionType

-- | Transforms the given list of `Type`s into a tuple `Type` (there are special cases for an empty list and for a singleton)
makeTuple :: [a] -> TypeCompl a
makeTuple = TupleType

makeApplication :: a -> a -> TypeCompl a
makeApplication = AppType

forall :: Set TypeVar -> FlatFacts -> Facts -> Fact
forall s fs f
  | null s = NestedFact $ mempty :. fs :=> f -- the trivial case
forall s fs f = NestedFact $ s :. fs :=> f

-- | States that the given `TypeVar` type variable is unified with the given `Type`
typeUnion :: (ToType a, ToType b) => a -> b -> FlatFact Type
typeUnion t t' = toType t `Union` toType t'

-- | States that the given `TypeVar` type variable is unified with the given `Type`
classUnion :: (ToType a, ToType b) => a -> b -> FlatFact Type
classUnion t t' = toType t `ClassUnion` toType t'

-- | States that the given `TypeVar` type variable is unified with the given `Type`
instanceUnion :: (ToType a, ToType b) => a -> b -> FlatFact Type
instanceUnion t t' = toType t `InstanceUnion` toType t'

-- | States that the given `TypeVar` type variable follows the typing dictated by the `Type` (note that this does not imply type unification nor any subtyping)
typeConstraint :: (ToType a, ToType b) => a -> b -> FlatFact Type
typeConstraint t t' = toType t `Typing` toType t'

-- | States that the given `TypeVar` type variable is subtyped by another `TypeVar` type variable
subType :: (ToType a, ToType b) => a -> b -> FlatFact Type
subType t t' = toType t `SubType` toType t'

subKind :: (ToType a, ToType b) => a -> b -> FlatFact Type
subKind t t' = toType t `SubKind` toType t'

subConst :: (ToType a, ToType b) => a -> b -> FlatFact Type
subConst t t' = toType t `SubConst` toType t'

-- | States that the given `TypeVar` type variable is instantiated into another `TypeVar` type variable
instType :: (ToType a, ToType b) => a -> b -> FlatFact Type
instType t t' = toType t `InstType` toType t'

-- | States the minimum kind `DataKind` of a `TypeVar` type variable
minKindConstraint :: ToType a => DataKind -> a -> FlatFact Type
minKindConstraint = (. toType) . KindBounds . (`Bounds` maxBound) . Ordered

-- | States the maximum kind `DataKind` of a `TypeVar` type variable
maxKindConstraint :: ToType a => DataKind -> a -> FlatFact Type
maxKindConstraint = (. toType) . KindBounds . (minBound `Bounds`) . Ordered

unstorableConstraint :: ToType a => a -> FlatFact Type
unstorableConstraint = KindBounds (minBound `Bounds` minBound) . toType

-- | States that the given `TypeVar` (type variable) is a compile-time expression
constExprConstraint :: ToType a => a -> FlatFact Type
constExprConstraint = (. toType) . ConstnessBounds $ Bounds ConstExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a link-time expression
linkExprConstraint :: ToType a => a -> FlatFact Type
linkExprConstraint = (. toType) . ConstnessBounds $ Bounds ConstExpr LinkExpr

-- | States that the given `TypeVar` (type variable) is a regular (aka. run-time) expression
regularExprConstraint :: ToType a => a -> FlatFact Type
regularExprConstraint = (. toType) . ConstnessBounds $ Bounds Regular Regular

-- | States that the given `TypeVar` type variables is to be allocated to the register given by the given `Text` (this is a stronger version of `minKindConstraint`)
registerConstraint :: ToType a => Text -> a -> FlatFact Type
registerConstraint = (. toType) . OnRegister

kindConstraint :: ToType a => DataKind -> a -> FlatFact Type
kindConstraint kind = KindBounds (Ordered kind `Bounds` Ordered kind) . toType

functionKind :: ToType a => Int -> a -> FlatFact Type
functionKind = kindConstraint . FunctionKind

tupleKind :: ToType a => Int -> a -> FlatFact Type
tupleKind = kindConstraint . TupleKind

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classConstraint :: ToType a => Text -> a -> FlatFact Type
classConstraint name t = name `ClassConstraint` toType t

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classFact :: ToType a => Text -> a -> FlatFact Type
classFact name t = name `ClassFact` toType t

-- | TODO
classDetermine :: ToType a => Text -> a -> FlatFact Type
classDetermine name t = name `ClassDetermine` toType t

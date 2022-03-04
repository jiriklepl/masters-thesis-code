{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CMM.Inference.Type where

import safe Data.Data (Data(gmapQ))
import safe Data.Generics.Aliases (extQ)

import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import qualified Data.Text as T

import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Dioid (Dioid((<.>), mfull))
import safe CMM.Data.Lattice (Lattice(..))
import safe CMM.Data.Nullable (Fallbackable(..), Nullable(..))
import safe CMM.Data.Ordered (Ordered(..))
import safe CMM.Parser.HasPos (SourcePos)
import safe Data.PartialOrd (PartialOrd)
import safe qualified Data.PartialOrd as PartialOrd

-- | DataKind specifies the semantics and register allocability of the types that map onto it via kinding
data DataKind
  = GenericData -- | The most generic data kind
  | Unstorable -- | The empty data kind
  | DataKind (Set Int) -- | The regular case for data kinds
  | FunctionKind Int
  | TupleKind Int
  deriving (Show, Eq, Data)

instance PartialOrd DataKind where
  _ <= GenericData = True
  GenericData <= _ = False
  Unstorable <= _ = True
  _ <= Unstorable = False
  DataKind rs <= DataKind rs' = rs PartialOrd.<= rs'
  FunctionKind int <= FunctionKind int' = int PartialOrd.<= int'
  TupleKind int <= TupleKind int' = int PartialOrd.<= int'
  _ <= _ = False

instance Fallbackable DataKind where
  Unstorable ?? a = a
  a ?? _ = a

instance Nullable DataKind where
  nullVal = Unstorable

instance Bounded DataKind where
  minBound = Unstorable
  maxBound = GenericData

-- | Transforms the given `Set` of `Int`s denoting physical registers into a corresponding `DataKind`
makeDataKind :: Set Int -> DataKind
makeDataKind set
  | null set = Unstorable
  | otherwise = DataKind set

instance Lattice DataKind where
  GenericData /\ a = a
  Unstorable /\ _ = Unstorable
  DataKind rs /\ DataKind rs' = makeDataKind $ rs `Set.intersection` rs'
  FunctionKind int /\ FunctionKind int' = FunctionKind $ min int int'
  TupleKind int /\ TupleKind int' = TupleKind $ min int int'
  FunctionKind{} /\ _ = Unstorable
  TupleKind{} /\ _ = Unstorable
  a /\ b = b /\ a
  GenericData \/ _ = GenericData
  Unstorable \/ a = a
  DataKind rs \/ DataKind rs' = makeDataKind $ rs <> rs'
  FunctionKind int \/ FunctionKind int' = FunctionKind $ max int int'
  TupleKind int \/ TupleKind int' = TupleKind $ max int int'
  FunctionKind{} \/ _ = GenericData
  TupleKind{} \/ _ = GenericData
  a \/ b = b \/ a

instance Semigroup DataKind where
  (<>) = (\/)

instance Monoid DataKind where
  mempty = Unstorable

instance Dioid DataKind where
  (<.>) = (/\)
  mfull = GenericData

instance Ord (Ordered DataKind) where
  Ordered GenericData `compare` Ordered GenericData = EQ
  Ordered Unstorable `compare` Ordered Unstorable = EQ
  Ordered (DataKind set) `compare` Ordered (DataKind set') = set `compare` set'
  Ordered (FunctionKind int) `compare` Ordered (FunctionKind int') = int `compare` int'
  Ordered (TupleKind int) `compare` Ordered (TupleKind int') = int `compare` int'
  Ordered GenericData `compare` _ = LT
  _ `compare` Ordered GenericData = GT
  Ordered Unstorable `compare` _ = LT
  _ `compare` (Ordered Unstorable) = GT
  Ordered DataKind{} `compare` _ = LT
  _ `compare` (Ordered DataKind{}) = GT
  Ordered FunctionKind{} `compare` _ = LT
  _ `compare` (Ordered FunctionKind{}) = GT

instance Bounded (Ordered DataKind) where
  minBound = Ordered minBound
  maxBound = Ordered maxBound

data Constness
  = Regular
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Data)

deriving instance Ord (Ordered Constness)

instance Ord Constness where
  Regular `compare` Regular = EQ
  LinkExpr `compare` LinkExpr = EQ
  ConstExpr `compare` ConstExpr = EQ
  Regular `compare` _ = GT
  _ `compare` Regular = LT
  LinkExpr `compare` _ = GT
  _ `compare` LinkExpr = LT

instance PartialOrd Constness where
  (<=) = (Prelude.<=)

instance Lattice Constness where
  (/\) = min
  (\/) = max

instance Bounded Constness where
  minBound = ConstExpr
  maxBound = Regular

data TypeKind
  = Star
  | Constraint
  | GenericType -- wildCard
  | ErrorKind Text
  | TypeKind :-> TypeKind
  deriving (Show, Data)

instance Eq TypeKind where
  Star == Star = True
  Constraint == Constraint = True
  GenericType == GenericType = True
  ErrorKind _ == _ = False
  _ == ErrorKind _ = False
  (l :-> r) == (l' :-> r') = l == l' && r == r'
  _ == _ = False

instance Ord TypeKind where
  Star `compare` Star = EQ
  Star `compare` _ = LT
  _ `compare` Star = GT
  Constraint `compare` Constraint = EQ
  Constraint `compare` _ = LT
  _ `compare` Constraint = GT
  GenericType `compare` GenericType = EQ
  GenericType `compare` _ = LT
  _ `compare` GenericType = GT
  ErrorKind s `compare` ErrorKind s' = s `compare` s'
  ErrorKind _ `compare` _ = LT
  _ `compare` ErrorKind _ = GT
  (l :-> r) `compare` (l' :-> r') =
    case l `compare` l' of
      LT -> LT
      GT -> GT
      EQ -> r `compare` r'

instance Fallbackable TypeKind where
  GenericType ?? kind = kind
  kind ?? _ = kind

instance Nullable TypeKind where
  nullVal = GenericType

data TypeAnnot
  = NoTypeAnnot
  | TypeInst TypeVar
  | TypeAST SourcePos
  | TypeNamedAST Text SourcePos
  | TypeBuiltIn Text
  deriving (Show, Data)

instance Fallbackable TypeAnnot where
  NoTypeAnnot ?? kind = kind
  kind ?? _ = kind

instance Nullable TypeAnnot where
  nullVal = NoTypeAnnot

data TypeVar
  = NoType
  | TypeVar { tVarId :: Int, tVarKind :: TypeKind, tVarParent :: TypeVar }
  deriving (Show, Data, IsTyped)

familyDepth :: TypeVar -> Int
familyDepth NoType = 0
familyDepth TypeVar{tVarParent = parent} = familyDepth parent + 1

toLam :: TypeVar -> TypeCompl a
toLam NoType = undefined
toLam (TypeVar int kind parent) = LamType int kind parent

instance Eq TypeVar where
  TypeVar int _ _ == TypeVar int' _ _ = int == int'
  _ == _ = False

instance Ord TypeVar where
  NoType `compare` NoType = EQ
  TypeVar int _ _ `compare` TypeVar int' _ _ = int `compare` int'
  NoType `compare` _ = LT
  _ `compare` NoType = GT

instance Fallbackable TypeVar where
  NoType ?? tVar = tVar
  tVar ?? _ = tVar

instance Nullable TypeVar where
  nullVal = NoType

instance HasTypeKind TypeVar where
  getTypeKind NoType {} = GenericType
  getTypeKind (TypeVar _ kind _) = kind

type PrimType = TypeCompl TypeVar

data TypeCompl a
  = TupleType [a]
  | FunctionType [a] a
  | AppType a a
  | AddrType a
  | LamType Int TypeKind TypeVar
  | ConstType Int TypeKind TypeVar
  | StringType
  | String16Type
  | LabelType
  | TBitsType Int
  | BoolType
  | VoidType
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data, IsTyped)

instance (HasTypeKind a, Show a) => HasTypeKind (TypeCompl a) where
  getTypeKind (AppType t _) =
    case getTypeKind t of
      _ :-> k -> k
      GenericType -> GenericType
      _ -> ErrorKind $ T.pack ("Kind " ++ show t ++ " cannot be applied")
  getTypeKind (LamType _ kind _) = kind
  getTypeKind (ConstType _ kind _) = kind
  getTypeKind _ = Star

data Type
  = ErrorType Text
  | VarType TypeVar
  | ComplType (TypeCompl Type)
  deriving (Show, Eq, Ord, Data, IsTyped)

instance Fallbackable Type where
  ErrorType {} ?? a = a
  a ?? _ = a

instance HasTypeKind Type where
  getTypeKind ErrorType {} = GenericType
  getTypeKind (VarType t) = getTypeKind t
  getTypeKind (ComplType t) = getTypeKind t

class ToType a where
  toType :: a -> Type

instance ToType Type where
  toType = id

instance ToType TypeVar where
  toType = VarType

instance ToType a => ToType (TypeCompl a) where
  toType = ComplType . fmap toType

infix 6 :=>

data Qual a =
  FlatFacts :=> a
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasTypeKind a => HasTypeKind (Qual a) where
  getTypeKind (_ :=> t) = getTypeKind t

infix 5 :.

data Scheme a =
  Set TypeVar :. Qual a
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasTypeKind a => HasTypeKind (Scheme a) where
  getTypeKind (_ :. t) = getTypeKind t

-- TODO: add methods and their instances
newtype Class =
  Class (Scheme [Inst])
  deriving (Show, Data)

newtype Inst =
  Inst (Scheme [Type])
  deriving (Show, Data)

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
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data, IsTyped)

data NestedFact a
  = Fact (FlatFact a)
  | NestedFact (Scheme [NestedFact a])
  deriving (Show, Eq, Ord, Data, IsTyped)

type FlatFacts = [FlatFact Type]

type Fact = NestedFact Type

type Facts = [Fact]

class HasTypeKind a where
  getTypeKind :: a -> TypeKind

class Data a =>
      IsTyped a
  where
  freeTypeVars :: a -> Set TypeVar
  freeTypeVars = go
    where
      go :: Data d => d -> Set TypeVar
      go = (Set.unions . gmapQ go) `extQ` factCase `extQ` leaf
      factCase =
        \case
          Fact (InstType _ t') -> freeTypeVars t'
          NestedFact (tVars :. flatFacts :=> nestedFacts) ->
            (freeTypeVars flatFacts <> freeTypeVars nestedFacts) `Set.difference`
            tVars
          (fact :: Fact) -> Set.unions $ gmapQ go fact
      leaf tVar@TypeVar {} = Set.singleton tVar
      leaf _ = mempty

deriving instance IsTyped a => IsTyped [a]

-- | Transforms the two given `Type`s into a function `Type`
makeFunction :: [a] -> a -> TypeCompl a
makeFunction = FunctionType

-- | Transforms the given list of `Type`s into a tuple `Type` (there are special cases for an empty list and for a singleton)
makeTuple :: [a] -> TypeCompl a
makeTuple = TupleType

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

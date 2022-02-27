{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import safe CMM.Data.Orderable (Orderable(..))
import safe CMM.Parser.HasPos (SourcePos)

-- | DataKind specifies the semantics and register allocability of the types that map onto it via kinding
data DataKind
  = GenericData -- | The most generic data kind
  | DataKind (Set Int) -- | The regular case for data kinds
  | RegisterKind Int -- | The kind that denotes a singleton set of registers
  | FalseData -- | The empty data kind
  deriving (Show, Eq, Data)

instance Orderable DataKind OrdDataKind where
  makeOrdered = OrdDataKind
  unmakeOrdered = getDataKind

instance Fallbackable DataKind where
  FalseData ?? a = a
  a ?? _ = a

instance Nullable DataKind where
  nullVal = FalseData

instance Bounded DataKind where
  minBound = GenericData
  maxBound = FalseData

-- | Transforms the given `Set` of `Int`s denoting physical registers into a corresponding `DataKind`
makeDataKind :: Set Int -> DataKind
makeDataKind set
  | null set = FalseData
  | Set.size set > 1 = DataKind set
  | otherwise = RegisterKind $ Set.findMin set

instance {-# OVERLAPPING #-} Lattice DataKind where
  GenericData /\ a = a
  FalseData /\ _ = FalseData
  DataKind rs /\ DataKind rs' = makeDataKind $ rs `Set.intersection` rs'
  DataKind rs /\ b@(RegisterKind r)
    | r `Set.member` rs = b
    | otherwise = FalseData
  a@(RegisterKind r) /\ RegisterKind r'
    | r == r' = a
    | otherwise = FalseData
  a /\ b = b /\ a
  GenericData \/ _ = GenericData
  FalseData \/ a = a
  DataKind rs \/ DataKind rs' = makeDataKind $ rs <> rs'
  DataKind rs \/ RegisterKind r = DataKind $ r `Set.insert` rs
  RegisterKind r \/ RegisterKind r' =
    makeDataKind $ Set.singleton r `Set.union` Set.singleton r'
  a \/ b = b \/ a

instance Semigroup DataKind where
  (<>) = (\/)

instance Monoid DataKind where
  mempty = FalseData

instance Dioid DataKind where
  (<.>) = (/\)
  mfull = GenericData

newtype OrdDataKind =
  OrdDataKind
    { getDataKind :: DataKind
    }
  deriving (Show, Eq, Data)

instance Ord OrdDataKind where
  OrdDataKind GenericData `compare` OrdDataKind GenericData = EQ
  OrdDataKind FalseData `compare` OrdDataKind FalseData = EQ
  OrdDataKind (DataKind set) `compare` OrdDataKind (DataKind set') =
    set `compare` set'
  OrdDataKind (RegisterKind int) `compare` OrdDataKind (RegisterKind int') =
    int `compare` int'
  OrdDataKind GenericData `compare` _ = LT
  _ `compare` OrdDataKind GenericData = GT
  OrdDataKind FalseData `compare` _ = LT
  _ `compare` (OrdDataKind FalseData) = GT
  OrdDataKind (DataKind _) `compare` _ = LT
  _ `compare` OrdDataKind (DataKind _) = GT

instance Bounded OrdDataKind where
  minBound = OrdDataKind minBound
  maxBound = OrdDataKind maxBound

data Constness
  = Regular
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Data)

instance Ord Constness where
  Regular `compare` Regular = EQ
  LinkExpr `compare` LinkExpr = EQ
  ConstExpr `compare` ConstExpr = EQ
  Regular `compare` _ = LT
  _ `compare` Regular = GT
  ConstExpr `compare` _ = GT
  _ `compare` ConstExpr = LT

instance Bounded Constness where
  minBound = Regular
  maxBound = ConstExpr

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
  | TypeAST Text SourcePos
  | TypeBuiltIn Text
  deriving (Show, Data)

instance Fallbackable TypeAnnot where
  NoTypeAnnot ?? kind = kind
  kind ?? _ = kind

instance Nullable TypeAnnot where
  nullVal = NoTypeAnnot

data TypeVar
  = NoType
  | TypeVar Int TypeKind
  | TypeLam Int TypeKind
  | TypeConst Int TypeKind
  deriving (Show, Data, IsTyped)

varId :: TypeVar -> Int
varId NoType = undefined
varId (TypeVar int _) = int
varId (TypeLam int _) = int
varId (TypeConst int _) = int

instance Eq TypeVar where
  TypeVar int _ == TypeVar int' _ = int == int'
  TypeLam int _ == TypeLam int' _ = int == int'
  TypeConst int _ == TypeConst int' _ = int == int'
  _ == _ = False

instance Ord TypeVar where
  NoType `compare` NoType = EQ
  TypeVar int _ `compare` TypeVar int' _ = int `compare` int'
  TypeLam int _ `compare` TypeLam int' _ = int `compare` int'
  TypeConst int _ `compare` TypeConst int' _ = int `compare` int'
  NoType `compare` _ = LT
  _ `compare` TypeConst {} = LT
  _ `compare` NoType = GT
  TypeConst {} `compare` _ = GT
  TypeVar {} `compare` TypeLam {} = LT
  TypeLam {} `compare` TypeVar {} = GT

instance Fallbackable TypeVar where
  NoType ?? tVar = tVar
  tVar ?? _ = tVar

instance Nullable TypeVar where
  nullVal = NoType

instance HasTypeKind TypeVar where
  getTypeKind NoType {} = GenericType
  getTypeKind (TypeVar _ kind) = kind
  getTypeKind (TypeLam _ kind) = kind
  getTypeKind (TypeConst _ kind) = kind

type PrimType = TypeCompl TypeVar

data TypeCompl a
  = TupleType [a]
  | FunctionType [a] a
  | AppType a a
  | AddrType a
  | StringType
  | String16Type
  | LabelType
  | TBitsType Int
  | BoolType
  | VoidType
  deriving (Show, Eq, Ord, Data, IsTyped)

instance (HasTypeKind a, Show a) => HasTypeKind (TypeCompl a) where
  getTypeKind TupleType {} = Star -- TODO: check if all types are `Star` (when creating)
  getTypeKind FunctionType {} = Star -- TODO: ditto
  getTypeKind (AppType t _) = case getTypeKind t of
    _ :-> k -> k
    GenericType -> GenericType
    _ -> ErrorKind $ T.pack ("Kind " ++ show t ++ " cannot be applied")
  getTypeKind AddrType {} = Star -- TODO: ditto
  getTypeKind StringType {} = Star
  getTypeKind String16Type {} = Star
  getTypeKind LabelType {} = Star
  getTypeKind TBitsType {} = Star
  getTypeKind BoolType {} = Star
  getTypeKind VoidType {} = Star

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
  getTypeKind (ComplType t)  = getTypeKind t

class ToType a where
  toType :: a -> Type

instance ToType Type where
  toType = id

instance ToType TypeVar where
  toType = VarType

instance ToType a => ToType (TypeCompl a) where
  toType (TupleType ts) = ComplType . TupleType $ toType <$> ts
  toType (FunctionType ts t) = ComplType $ FunctionType (toType <$> ts) (toType t)
  toType (AppType t t') = ComplType $ AppType (toType t) (toType t')
  toType (AddrType t) = ComplType $ AddrType (toType t)

infix 6 :=>

data Qual a =
  Facts :=> a
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

data Fact
  = SubType Type Type -- supertype; subtype
  | TypeUnion Type Type -- binds the type to a type
  | ClassUnion Type Type -- binds the type to a type
  | InstanceUnion Type Type -- binds the type to a type
  | Typing Type Type -- states that the type follows a certain typing
  | KindLimit (Bounds OrdDataKind) Type -- lower bound on the kind of the type variable
  | ConstnessLimit (Bounds Constness) Type -- lower bound on the constness of the type variable
  | OnRegister Text TypeVar -- states that the type variable stores its data to a certain register
  | SubKind Type Type -- superKind; subKind
  | SubConst Type Type -- superConst; subConst
  | InstType Type Type -- polytype; monotype
  | ClassConstraint Text Type
  | ClassFact Text Type
  | NestedFact (Scheme Fact)
  deriving (Show, Eq, Ord, Data, IsTyped)

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
          InstType _ tVar' -> freeTypeVars tVar'
          fact -> Set.unions $ gmapQ go fact
      leaf tVar@TypeVar {} = Set.singleton tVar
      leaf _ = mempty

deriving instance IsTyped a => IsTyped [a]

-- | Transforms the two given `Type`s into a function `Type`
makeFunction :: [a] -> a -> TypeCompl a
makeFunction = FunctionType

-- | Transforms the given list of `Type`s into a tuple `Type` (there are special cases for an empty list and for a singleton)
makeTuple :: [a] -> TypeCompl a
makeTuple = TupleType

forall :: Set TypeVar -> Facts -> Fact -> Fact
forall s fs f
  | null s = NestedFact $ mempty :. fs :=> f -- the trivial case
forall s fs f = NestedFact $ s :. fs :=> f

-- | States that the given `TypeVar` type variable is unified with the given `Type`

typeUnion :: (ToType a, ToType b) => a -> b -> Fact
typeUnion t t' = toType t `TypeUnion` toType t'

-- | States that the given `TypeVar` type variable is unified with the given `Type`
classUnion :: (ToType a, ToType b) => a -> b -> Fact
classUnion t t' = toType t `ClassUnion` toType t'

-- | States that the given `TypeVar` type variable is unified with the given `Type`
instanceUnion :: (ToType a, ToType b) => a -> b -> Fact
instanceUnion t t' = toType t `InstanceUnion` toType t'

-- | States that the given `TypeVar` type variable follows the typing dictated by the `Type` (note that this does not imply type unification nor any subtyping)
typeConstraint :: (ToType a, ToType b) => a -> b -> Fact
typeConstraint t t' = toType t `Typing` toType t'

-- | States that the given `TypeVar` type variable is subtyped by another `TypeVar` type variable
subType :: (ToType a, ToType b) => a -> b -> Fact
subType t t' = toType t `SubType` toType t'

-- | TODO
subKind :: (ToType a, ToType b) => a -> b -> Fact
subKind t t' = toType t `SubKind` toType t'

-- | TODO
subConst :: (ToType a, ToType b) => a -> b -> Fact
subConst t t' = toType t `SubConst` toType t'

-- | States that the given `TypeVar` type variable is instantiated into another `TypeVar` type variable
instType :: (ToType a, ToType b) => a -> b -> Fact
instType t t' = toType t `InstType` toType t'

-- | States the minimum kind `DataKind` of a `TypeVar` type variable
minKindConstraint :: ToType a => DataKind -> a -> Fact
minKindConstraint = (. toType) . KindLimit . (`Bounds` maxBound) . OrdDataKind

-- | States the maximum kind `DataKind` of a `TypeVar` type variable
maxKindConstraint :: ToType a => DataKind -> a -> Fact
maxKindConstraint = (. toType) . KindLimit . (minBound `Bounds`) . OrdDataKind

-- | States that the given `TypeVar` (type variable) is a compile-time expression
constExprConstraint :: ToType a => a -> Fact
constExprConstraint = (. toType) . ConstnessLimit $ Bounds ConstExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a link-time expression
linkExprConstraint :: ToType a => a -> Fact
linkExprConstraint = (. toType) . ConstnessLimit $ Bounds LinkExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a regular (aka. run-time) expression
regularExprConstraint :: ToType a => a -> Fact
regularExprConstraint = (. toType) . ConstnessLimit $ Bounds Regular Regular

-- | States that the given `TypeVar` type variables is to be allocated to the register given by the given `Text` (this is a stronger version of `minKindConstraint`)
registerConstraint :: Text -> TypeVar -> Fact
registerConstraint = OnRegister

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classConstraint :: ToType a => Text -> a -> Fact
classConstraint name t = name `ClassConstraint` toType t

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classFact :: ToType a => Text -> a -> Fact
classFact name t = name `ClassFact` toType t

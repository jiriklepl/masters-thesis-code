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

instance Lattice DataKind where
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

data TVarAnnot
  = NoTVarAnnot
  | TVarInst TypeVar
  | TVarAST Text SourcePos
  | TVarBuiltIn Text
  deriving (Show, Data)

instance Fallbackable TVarAnnot where
  NoTVarAnnot ?? kind = kind
  kind ?? _ = kind

instance Nullable TVarAnnot where
  nullVal = NoTVarAnnot

data TypeVar
  = NoType
  | TypeVar Int TypeKind TVarAnnot
  | TypeLam Int TypeKind TVarAnnot
  | TypeConst Int TypeKind TVarAnnot
  deriving (Show, Data, IsTyped)

typeVarId :: TypeVar -> Int
typeVarId NoType = undefined
typeVarId (TypeVar int _ _) = int
typeVarId (TypeLam int _ _) = int
typeVarId (TypeConst int _ _) = int

instance Eq TypeVar where
  TypeVar int _ _ == TypeVar int' _ _ = int == int'
  TypeLam int _ _ == TypeLam int' _ _ = int == int'
  TypeConst int _ _ == TypeConst int' _ _ = int == int'
  _ == _ = False

instance Ord TypeVar where
  NoType `compare` NoType = EQ
  TypeVar int _ _ `compare` TypeVar int' _ _ = int `compare` int'
  TypeLam int _ _ `compare` TypeLam int' _ _ = int `compare` int'
  TypeConst int _ _ `compare` TypeConst int' _ _ = int `compare` int'
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
  getTypeKind (TypeVar _ kind _) = kind
  getTypeKind (TypeLam _ kind _) = kind
  getTypeKind (TypeConst _ kind _) = kind

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

data Type
  = ErrorType Text
  | VarType TypeVar
  | TBitsType Int
  | BoolType
  | TupleType [Type]
  | FunctionType Type Type
  | AppType Type [Type]
  | AddrType Type
  | LabelType
  | StringType
  | String16Type
  | VoidType
  deriving (Show, Eq, Ord, Data, IsTyped)

instance Fallbackable Type where
  ErrorType {} ?? a = a
  a ?? _ = a

instance HasTypeKind Type where
  getTypeKind ErrorType {} = GenericType
  getTypeKind (VarType t) = getTypeKind t
  getTypeKind TBitsType {} = Star
  getTypeKind BoolType {} = Star
  getTypeKind TupleType {} = Star -- TODO: check if all types are `Star` (when creating)
  getTypeKind FunctionType {} = Star -- TODO: ditto
  getTypeKind (AppType t _) = case getTypeKind t of
    _ :-> k -> k
    GenericType -> GenericType
    _ -> ErrorKind $ T.pack ("Kind " ++ show t ++ " cannot be applied")
  getTypeKind AddrType {} = Star -- TODO: ditto
  getTypeKind LabelType {} = Star
  getTypeKind StringType {} = Star
  getTypeKind String16Type {} = Star
  getTypeKind VoidType {} = Star

-- TODO: add methods and their instances
newtype Class =
  Class (Scheme [Inst])
  deriving (Show, Data)

newtype Inst =
  Inst (Scheme [Type])
  deriving (Show, Data)

data Fact
  = SubType TypeVar TypeVar -- supertype; subtype
  | TypeUnion TypeVar Type -- binds the type variable to a type
  | ClassUnion TypeVar Type -- binds the type variable to a type
  | InstanceUnion TypeVar Type -- binds the type variable to a type
  | Typing TypeVar Type -- states that the type variable follows a certain typing
  | KindLimit (Bounds OrdDataKind Lattice) TypeVar -- lower bound on the kind of the type variable
  | ConstnessLimit (Bounds Constness Ord) TypeVar -- lower bound on the constness of the type variable
  | OnRegister Text TypeVar -- states that the type variable stores its data to a certain register
  | SubKind TypeVar TypeVar -- superKind; subKind
  | SubConst TypeVar TypeVar -- superConst; subConst
  | InstType TypeVar TypeVar -- polytype; monotype
  | ClassConstraint Text TypeVar
  | ClassFact Text TypeVar
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
          InstType _ tVar' -> Set.singleton tVar'
          fact -> Set.unions $ gmapQ go fact
      leaf tVar@TypeVar {} = Set.singleton tVar
      leaf _ = mempty

deriving instance IsTyped a => IsTyped [a]

-- | Transforms the two given `Type`s into a function `Type`
makeFunction :: Type -> Type -> Type
makeFunction = FunctionType

-- | Transforms the given list of `Type`s into a tuple `Type` (there are special cases for an empty list and for a singleton)
makeTuple :: [Type] -> Type
makeTuple [] = VoidType
makeTuple [t] = t
makeTuple ts = TupleType ts

forall :: Set TypeVar -> Facts -> Fact -> Fact
forall s fs f
  | null s = NestedFact $ mempty :. fs :=> f -- the trivial case
forall s fs f = NestedFact $ s :. fs :=> f

-- | States that the given `TypeVar` type variable is unified with the given `Type`
typeUnion :: TypeVar -> Type -> Fact
typeUnion = TypeUnion

-- | States that the given `TypeVar` type variable is unified with the given `Type`
classUnion :: TypeVar -> Type -> Fact
classUnion = ClassUnion

-- | States that the given `TypeVar` type variable is unified with the given `Type`
instanceUnion :: TypeVar -> Type -> Fact
instanceUnion = InstanceUnion

-- | States that the given `TypeVar` type variable follows the typing dictated by the `Type` (note that this does not imply type unification nor any subtyping)
typeConstraint :: TypeVar -> Type -> Fact
typeConstraint = Typing

-- | States that the given `TypeVar` type variable is subtyped by another `TypeVar` type variable
subType :: TypeVar -> TypeVar -> Fact
subType = SubType

-- | States that the given `TypeVar` type variable is instantiated into another `TypeVar` type variable
instType :: TypeVar -> TypeVar -> Fact
instType = InstType

-- | States the minimum kind `DataKind` of a `TypeVar` type variable
minKindConstraint :: DataKind -> TypeVar -> Fact
minKindConstraint = KindLimit . (`Bounds` maxBound) . OrdDataKind

-- | States the maximum kind `DataKind` of a `TypeVar` type variable
maxKindConstraint :: DataKind -> TypeVar -> Fact
maxKindConstraint = KindLimit . (minBound `Bounds`) . OrdDataKind

-- | States that the given `TypeVar` (type variable) is a compile-time expression
constExprConstraint :: TypeVar -> Fact
constExprConstraint = ConstnessLimit $ Bounds ConstExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a link-time expression
linkExprConstraint :: TypeVar -> Fact
linkExprConstraint = ConstnessLimit $ Bounds LinkExpr ConstExpr

-- | States that the given `TypeVar` (type variable) is a regular (aka. run-time) expression
regularExprConstraint :: TypeVar -> Fact
regularExprConstraint = ConstnessLimit $ Bounds Regular Regular

-- | States that the given `TypeVar` type variables is to be allocated to the register given by the given `Text` (this is a stronger version of `minKindConstraint`)
registerConstraint :: Text -> TypeVar -> Fact
registerConstraint = OnRegister

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classConstraint :: Text -> TypeVar -> Fact
classConstraint = ClassConstraint

-- | States that the given list of `TypeVar` type variables is to be an instance of the class given by the `ClassHandle` handle
classFact :: Text -> TypeVar -> Fact
classFact = ClassFact

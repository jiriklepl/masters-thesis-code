{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}


-- TODO: create a special type for Kinds

module CMM.Inference.Type where

import safe Data.Data
import safe Data.Generics.Aliases (extQ)

import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe Data.Text (Text)

newtype ClassHandle
  = ClassHandle Text
  deriving (Show, Eq, Ord, Data)


data Constness
  = Regular
  | Unknown
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Ord, Data) -- TODO: make sure that they follow the correctus order

-- (Kind, Constness, Register)
type TypeAnnotations = (Maybe Text, Constness, Maybe Text)

data TypeKind
  = Star
  | TypeKind :-> TypeKind
  | Generic
  deriving (Show, Data)

instance Eq TypeKind where
  Generic == _ = True
  _ == Generic = True

  Star == Star = True

  (l :-> r) == (l' :-> r') = l == l' && r == r'
  _ == _ = False

instance Ord TypeKind where
  Generic `compare` Generic = EQ
  Generic `compare` _ = LT
  _ `compare` Generic = GT

  Star `compare` Star = EQ
  Star `compare` _ = LT
  _ `compare` Star = GT

  (l :-> r) `compare` (l' :-> r') =
    case l `compare` l' of
      LT -> LT
      GT -> GT
      EQ -> r `compare` r'

data TypeVar
  = NoType
  | TypeVar Int TypeKind (Maybe Text) -- TODO: change text to something even more useful
  | TypeLam Int TypeKind
  | TypeConst Text TypeKind
  deriving (Show, Data, IsTyped)

instance Eq TypeVar where
  TypeVar int _ _ == TypeVar int' _ _ = int == int'
  TypeLam int _ == TypeLam int' _ = int == int'
  TypeConst name _ == TypeConst name' _ = name == name'
  _ == _ = False

instance Ord TypeVar where
  NoType `compare` NoType = EQ
  TypeVar int _ _ `compare` TypeVar int' _ _ = int `compare` int'
  TypeLam int _ `compare` TypeLam int' _ = int `compare` int'
  TypeConst name _ `compare` TypeConst name' _ = name `compare` name'

  NoType `compare` _ = LT
  _ `compare` TypeConst {} = LT

  _ `compare` NoType = GT
  TypeConst {} `compare` _ = GT

  TypeVar {} `compare` TypeLam {} = LT
  TypeLam {} `compare` TypeVar {} = GT

instance HasKind TypeVar where
  getKind NoType{} = Generic
  getKind (TypeVar _ kind _) = kind
  getKind (TypeLam _ kind) = kind
  getKind (TypeConst _ kind) = kind

infix 6 :=>

data Qual a =
  Facts :=> a
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasKind a => HasKind (Qual a) where
  getKind (_ :=> t) = getKind t

infix 5 :.

data Scheme a =
  [TypeKind] :. Qual a
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasKind a => HasKind (Scheme a) where
  getKind (_ :. t) = getKind t

data Type
  = ErrorType Text
  | Forall (Scheme Type)
  | VarType TypeVar
  | TBitsType Int
  | BoolType
  | TupleType [Type]
  | FunctionType Type Type
  | AddrType Type
  | LabelType
  | StringType
  | String16Type
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasKind Type where
  getKind ErrorType{} = Generic
  getKind (Forall scheme) = getKind scheme
  getKind (VarType t) = getKind t
  getKind TBitsType{} = Star
  getKind BoolType{} = Star
  getKind TupleType{} = Star -- TODO: check if all types are `Star`
  getKind FunctionType{} = Star -- TODO: ditto
  getKind AddrType{} = Star -- TODO: ditto
  getKind LabelType{} = Star
  getKind StringType{} = Star
  getKind String16Type{} = Star

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
  | Typing TypeVar Type -- states that the type variable follows a certain typing
  | KindLimit Text TypeVar -- lower bound on the kind of the type variable
  | ConstnessLimit Constness TypeVar -- lower bound on the constness of the type variable
  | OnRegister Text TypeVar -- states that the type variable stores its data to a certain register
  | SubKind TypeVar TypeVar -- superKind; subKind
  | SubConst TypeVar TypeVar -- superConst; subConst
  | InstType TypeVar TypeVar -- polytype; monotype
  | Constraint ClassHandle [TypeVar]
  | NestedFacts Facts Facts
  deriving (Show, Eq, Ord, Data)

type Facts = [Fact]

class HasKind a where
  getKind :: a -> TypeKind

class Data a =>
      IsTyped a
  where
  freeTypeVars :: a -> Set TypeVar
  freeTypeVars = go
    where
      go :: Data d => d -> Set TypeVar
      go = (Set.unions . gmapQ go) `extQ` leaf
      leaf tVar@TypeVar {} = Set.singleton tVar
      leaf _ = mempty

makeFunction :: Type -> Type -> Type
makeFunction = FunctionType

makeTuple :: [Type] -> Type
makeTuple = TupleType

forall :: Set TypeVar -> Facts -> Type -> Type
forall s [] t
  | null s = Forall $ [] :. [] :=> t
forall _ _ t = Forall $ [] :. [] :=> t -- TODO: continue from here by replacing this placeholder

typeUnion :: TypeVar -> Type -> Fact
typeUnion = TypeUnion

typeConstraint :: TypeVar -> Type -> Fact
typeConstraint = Typing

subType :: TypeVar -> TypeVar -> Fact
subType = SubType

instType :: TypeVar -> TypeVar -> Fact
instType = InstType

kindedConstraint :: Text -> TypeVar -> Fact
kindedConstraint = KindLimit

constExprConstraint :: TypeVar -> Fact
constExprConstraint = ConstnessLimit ConstExpr

linkExprConstraint :: TypeVar -> Fact
linkExprConstraint = ConstnessLimit LinkExpr

registerConstraint :: Text -> TypeVar -> Fact
registerConstraint = OnRegister

classConstraint :: ClassHandle -> [TypeVar] -> Fact
classConstraint = Constraint

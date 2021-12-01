{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Type where

import safe Data.Data
import safe Data.Generics.Aliases (extQ)

import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe Data.Text (Text)
import safe qualified Data.Text as T

newtype ClassHandle
  = ClassHandle Text
  deriving (Show, Eq, Ord, Data)


data Constness
  = Regular -- TODO: maybe connect with unknown?
  | Unknown
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Ord, Data)

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

data TypeVar =
  TypeVar Int TypeKind
  deriving (Show, Data, IsTyped)

instance Eq TypeVar where
  TypeVar int _ == TypeVar int' _ = int == int'

instance Ord TypeVar where
  TypeVar int _ `compare` TypeVar int' _ = int `compare` int'

instance HasKind TypeVar where
  getKind (TypeVar _ kind) = kind

data TypeLam =
  TypeLam Int TypeKind
  deriving (Show, Data, IsTyped)

instance Eq TypeLam where
  TypeLam int _ == TypeLam int' _ = int == int'

instance Ord TypeLam where
  TypeLam int _ `compare` TypeLam int' _ = int `compare` int'

instance HasKind TypeLam where
  getKind (TypeLam _ kind) = kind

data TypeConst =
  TypeConst Text TypeKind
  deriving (Show, Data, IsTyped)

instance Eq TypeConst where
  TypeConst name _ == TypeConst name' _ = name == name'

instance Ord TypeConst where
  TypeConst name _ `compare` TypeConst name' _ = name `compare` name'

instance HasKind TypeConst where
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
  = NoType
  | ErrorType Text
  | SimpleType SimpleType
  | AnnotType TypeAnnotations SimpleType
  | Forall (Scheme SimpleType)
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasKind Type where
  getKind NoType = Generic
  getKind ErrorType{} = Generic
  getKind (SimpleType t) = getKind t
  getKind (AnnotType _ t) = getKind t
  getKind (Forall scheme) = getKind scheme

data SimpleType
  = VarType TypeVar
  | ConstType TypeConst
  | LamType TypeLam
  | TBitsType Int
  | BoolType
  | TupleType [Type]
  | FunctionType Type Type
  | AddrType Type
  | LabelType
  | StringType
  | String16Type
  deriving (Show, Eq, Ord, Data, IsTyped)

instance HasKind SimpleType where
  getKind (VarType t) = getKind t
  getKind (ConstType t) = getKind t
  getKind (LamType t) = getKind t
  getKind TBitsType{} = Star
  getKind BoolType{} = Star
  getKind TupleType{} = Star -- TODO: check if all types are `Star`
  getKind FunctionType{} = Star -- TODO: ditto
  getKind AddrType{} = Star -- TODO: ditto
  getKind LabelType{} = Star
  getKind StringType{} = Star
  getKind String16Type{} = Star

newtype Class =
  Class (Scheme [Inst])
  deriving (Show, Data)

newtype Inst =
  Inst (Scheme [Type])
  deriving (Show, Data)

data Fact
  = Union Type Type
  | SubType Type Type -- supertype; subtype
  | InstType Type Type -- polytype; monotype
  | Constraint ClassHandle [Type]
  | ConstnessLimit Constness Type
  | HasKind Text Type
  | OnRegister Text Type
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

makeFunction :: Type -> Type -> SimpleType
makeFunction = FunctionType

makeTuple :: [Type] -> Type
makeTuple = SimpleType . TupleType

forall :: Set TypeVar -> Facts -> SimpleType -> Type
forall s [] t
  | null s = Forall $ [] :. [] :=> t
forall _ _ t = Forall $ [] :. [] :=> t -- TODO: continue from here by replacing this placeholder

unifyConstraint :: Type -> Type -> Fact
unifyConstraint = Union

subType :: Type -> Type -> Fact
subType = SubType

instType :: Type -> Type -> Fact
instType = InstType

kindedType :: Text -> Type -> Type
kindedType _ NoType = NoType
kindedType _ type'@Forall {} = ErrorType . T.pack $ "Attempted to give a kind to a polytype `" <> show type' <> "`" -- TODO: prettify
kindedType _ type'@ErrorType {} = type' -- propagating the error
kindedType kind (SimpleType type') =
  AnnotType (Just kind, Unknown, Nothing) type'
kindedType kind (AnnotType (Nothing, constness, mReg) type') =
  AnnotType (Just kind, constness, mReg) type'
kindedType kind type'@(AnnotType (Just kind', _, _) _)
  | kind == kind' = type'
  | otherwise =
    ErrorType . T.pack $
    "Kinds `" <> show kind <> "` and `" <> show kind' <> "` do not match"

kindedConstraint :: Text -> Type -> Fact
kindedConstraint = HasKind

linkExprType :: Type -> Type
linkExprType NoType = NoType -- `NoType` is trivially a link-time constant
linkExprType type'@Forall{} = type' -- all polytypes are trivially link-time constants
linkExprType type'@ErrorType {} = type' -- propagating the error
linkExprType (SimpleType type') = AnnotType (Nothing, LinkExpr, Nothing) type'
linkExprType type'@(AnnotType (_, ConstExpr, _) _) = type'
linkExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, LinkExpr, mReg) type'

constExprType :: Type -> Type
constExprType NoType = NoType -- `NoType` is trivially a compile-time constant
constExprType type'@Forall{} = type' -- all polytypes are trivially compile-time constants
constExprType type'@ErrorType {} = type' -- propagating the error
constExprType (SimpleType type') = AnnotType (Nothing, ConstExpr, Nothing) type'
constExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, ConstExpr, mReg) type'

constExprConstraint :: Type -> Fact
constExprConstraint = ConstnessLimit ConstExpr

linkExprConstraint :: Type -> Fact
linkExprConstraint = ConstnessLimit LinkExpr

registerType :: Text -> Type -> Type
registerType _ NoType = NoType
registerType _ type'@Forall {} = ErrorType . T.pack $ "Attempted to give a hardware register to a polytype `" <> show type' <> "`" -- TODO: prettify
registerType _ type'@ErrorType {} = type'
registerType reg (SimpleType type') =
  AnnotType (Nothing, Unknown, Just reg) type'
registerType reg (AnnotType (mKind, constness, Nothing) type') =
  AnnotType (mKind, constness, Just reg) type'
registerType reg type'@(AnnotType (_, _, Just reg') _)
  | reg == reg' = type'
  | otherwise =
    ErrorType . T.pack $
    "Registers `" <> show reg <> "` and `" <> show reg' <> "` do not match" -- TODO: prettify

registerConstraint :: Text -> Type -> Fact
registerConstraint = OnRegister

classConstraint :: ClassHandle -> [Type] -> Fact
classConstraint = Constraint

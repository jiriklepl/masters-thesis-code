{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Type where

import safe Data.Data
import safe Data.Generics.Aliases

import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe Data.Text (Text)
import safe qualified Data.Text as T

data ClassHandle
  = NumericClass
  | RealClass
  | CharacterClass
  | AddressClass
  | LabelClass
  | Class Text
  deriving (Show, Eq, Ord, Data)

data Constness
  = Regular
  | Unknown
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Ord, Data)

type TypeAnnotations = (Maybe Text, Constness, Maybe Text)

newtype TypeVar
  = TypeVar Int
  deriving (Show, Eq, Ord, Data, IsTyped)

newtype TypeLam
  = TypeLam Int
  deriving (Show, Eq, Ord, Data, IsTyped)

data Type
  = NoType
  | ErrorType Text
  | SimpleType SimpleType
  | AnnotType TypeAnnotations SimpleType
  | Forall Int Facts Type
  deriving (Show, Eq, Ord, Data, IsTyped)

data SimpleType
  = VarType TypeVar
  | LamType TypeLam
  | TBitsType Int
  | BoolType
  | TupleType [Type]
  | FunctionType Type Type --TODO: change to [Type]?
  | AddrType Type
  | LabelType
  | StringType
  | String16Type
  deriving (Show, Eq, Ord, Data, IsTyped)

data Fact
  = Union Type Type
  | SubType Type Type -- supertype; subtype
  | InstType Type Type -- polytype; monotype
  | Constraint ClassHandle [Type]
  | ConstnessLimit Constness Type
  | HasKind Text Type
  | OnRegister Text Type
  deriving (Show, Eq, Ord, Data)

type Facts = [Fact]

class Data a => IsTyped a where
  freeTypeVars :: a -> Set TypeVar
  freeTypeVars = go
    where go :: Data d => d -> Set TypeVar
          go = (Set.unions . gmapQ go) `extQ` leaf
          leaf tVar@TypeVar{} = Set.singleton tVar

makeFunction :: Type -> Type -> Type
makeFunction args ret = SimpleType $ FunctionType args ret

makeTuple :: [Type] -> Type
makeTuple = SimpleType . TupleType

forall :: Set TypeVar -> Facts -> Type -> Type
forall s [] t
  | null s = Forall 0 [] t
forall _ _ t = Forall 0 [] t -- TODO: continue from here by replacing this placeholder

unifyConstraint :: Type -> Type -> Fact
unifyConstraint = Union

subType :: Type -> Type -> Fact
subType = SubType

instType :: Type -> Type -> Fact
instType = InstType

kindedType :: Text -> Type -> Type
kindedType _ NoType = NoType
kindedType _ type'@ErrorType {} = type'
kindedType kind (SimpleType type') =
  AnnotType (Just kind, Unknown, Nothing) type'
kindedType kind (AnnotType (Nothing, constness, mReg) type') =
  AnnotType (Just kind, constness, mReg) type'
kindedType kind type'@(AnnotType (Just kind', _, _) _)
  | kind == kind' = type'
  | otherwise =
    ErrorType . T.pack $
    "kinds `" <> show kind <> "` and `" <> show kind' <> "` do not match"

kindedConstraint :: Text -> Type -> Fact
kindedConstraint = HasKind

linkExprType :: Type -> Type
linkExprType NoType = NoType -- `NoType` is trivially a link-time constant
linkExprType type'@ErrorType {} = type'
linkExprType (SimpleType type') = AnnotType (Nothing, LinkExpr, Nothing) type'
linkExprType type'@(AnnotType (_, ConstExpr, _) _) = type'
linkExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, LinkExpr, mReg) type'

constExprType :: Type -> Type
constExprType NoType = NoType -- `NoType` is trivially a compile-time constant
constExprType type'@ErrorType {} = type'
constExprType (SimpleType type') = AnnotType (Nothing, ConstExpr, Nothing) type'
constExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, ConstExpr, mReg) type'

constExprConstraint :: Type -> Fact
constExprConstraint = ConstnessLimit ConstExpr

linkExprConstraint :: Type -> Fact
linkExprConstraint = ConstnessLimit LinkExpr

registerType :: Text -> Type -> Type
registerType _ NoType = NoType
registerType _ type'@ErrorType {} = type'
registerType reg (SimpleType type') =
  AnnotType (Nothing, Unknown, Just reg) type'
registerType reg (AnnotType (mKind, constness, Nothing) type') =
  AnnotType (mKind, constness, Just reg) type'
registerType reg type'@(AnnotType (_, _, Just reg') _)
  | reg == reg' = type'
  | otherwise =
    ErrorType . T.pack $
    "registers `" <> show reg <> "` and `" <> show reg' <> "` do not match"

registerConstraint :: Text -> Type -> Fact
registerConstraint = OnRegister

classConstraint :: ClassHandle -> [Type] -> Fact
classConstraint = Constraint

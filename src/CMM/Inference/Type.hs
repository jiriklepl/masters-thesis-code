{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference.Type where

import safe Data.Text (Text)
import safe qualified Data.Text as T

data ClassHandle
  = NumericClass
  | RealClass
  | CharacterClass
  | AddressClass
  | LabelClass
  | Class Text

data Constness
  = Regular
  | Unknown
  | LinkExpr
  | ConstExpr
  deriving (Show, Eq, Ord)

type TypeAnnotations = (Maybe Text, Constness, Maybe Text)

data TypeHandle
  = NoType
  | ErrorType Text
  | SimpleType SimpleHandle
  | AnnotType TypeAnnotations SimpleHandle
  deriving (Show, Eq, Ord)

data SimpleHandle
  = VarType Int
  | TBitsType Int
  | BoolType
  | TupleType [TypeHandle]
  | FunctionType TypeHandle TypeHandle
  | AddrType TypeHandle
  | LabelType
  | StringType
  | String16Type
  deriving (Show, Eq, Ord)

data Fact
  = Union TypeHandle TypeHandle
  | SubType TypeHandle TypeHandle
  | Constraint ClassHandle [TypeHandle]
  | ConstnessLimit Constness TypeHandle
  | HasKind Text TypeHandle
  | OnRegister Text TypeHandle

type Facts = [Fact]

makeFunction :: TypeHandle -> TypeHandle -> TypeHandle
makeFunction args ret = SimpleType $ FunctionType args ret

makeTuple :: [TypeHandle] -> TypeHandle
makeTuple = SimpleType . TupleType

freeTypeVars :: TypeHandle -> [TypeHandle]
freeTypeVars _ = [] -- TODO: continue from here (this is just a placeholder)

forallType :: [TypeHandle] -> TypeHandle -> TypeHandle
forallType [] t = t
forallType _ _ = undefined -- TODO: continue from here

unifyConstraint :: TypeHandle -> TypeHandle -> Fact
unifyConstraint = Union

subType :: TypeHandle -> TypeHandle -> Fact
subType = SubType

kindedType :: Text -> TypeHandle -> TypeHandle
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

kindedConstraint :: Text -> TypeHandle -> Fact
kindedConstraint = HasKind

linkExprType :: TypeHandle -> TypeHandle
linkExprType NoType = NoType -- `NoType` is trivially a link-time constant
linkExprType type'@ErrorType {} = type'
linkExprType (SimpleType type') = AnnotType (Nothing, LinkExpr, Nothing) type'
linkExprType type'@(AnnotType (_, ConstExpr, _) _) = type'
linkExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, LinkExpr, mReg) type'

constExprType :: TypeHandle -> TypeHandle
constExprType NoType = NoType -- `NoType` is trivially a compile-time constant
constExprType type'@ErrorType {} = type'
constExprType (SimpleType type') = AnnotType (Nothing, ConstExpr, Nothing) type'
constExprType (AnnotType (mKind, _, mReg) type') =
  AnnotType (mKind, ConstExpr, mReg) type'

constExprConstraint :: TypeHandle -> Fact
constExprConstraint = ConstnessLimit ConstExpr

linkExprConstraint :: TypeHandle -> Fact
linkExprConstraint = ConstnessLimit LinkExpr

registerType :: Text -> TypeHandle -> TypeHandle
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

registerConstraint :: Text -> TypeHandle -> Fact
registerConstraint = OnRegister

classConstraint :: ClassHandle -> [TypeHandle] -> Fact
classConstraint = Constraint

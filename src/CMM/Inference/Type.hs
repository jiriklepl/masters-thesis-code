{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Data.Text (Text)

data ClassHandle
  = NumericClass
  | RealClass
  | CharacterClass
  | AddressClass
  | LabelClass
  | Class Text

data TypeHandle
  = NoType
  | VarType Int
  | TBitsType Int
  | BoolType
  | AddrType TypeHandle
  | LabelType
  | StringType
  | String16Type
  deriving (Show, Eq, Ord)

data Fact

type Facts = [Fact]

makeFunction :: TypeHandle -> TypeHandle -> TypeHandle
makeFunction = undefined -- TODO: continue from here

makeTuple :: [TypeHandle] -> TypeHandle
makeTuple = undefined

freeTypeVars :: TypeHandle -> [TypeHandle]
freeTypeVars = undefined -- TODO: continue from here

forallType :: [TypeHandle] -> TypeHandle -> TypeHandle
forallType = undefined -- TODO: continue from here

unifyConstraint :: TypeHandle -> TypeHandle -> Fact
unifyConstraint = undefined -- TODO: continue from here

subType :: TypeHandle -> TypeHandle -> Fact
subType = undefined -- TODO: continue from here

kindedType :: Text -> TypeHandle -> TypeHandle -> Fact
kindedType = undefined -- TODO: continue from here

kindedConstraint :: Text -> TypeHandle -> Fact
kindedConstraint = undefined -- TODO: continue from here

linkExprType :: TypeHandle -> TypeHandle -> Fact
linkExprType = undefined -- TODO: continue from here

constExprType :: TypeHandle -> TypeHandle -> Fact
constExprType = undefined -- TODO: continue from here

constExprConstraint :: TypeHandle -> Fact
constExprConstraint = undefined

linkExprConstraint :: TypeHandle -> Fact
linkExprConstraint = undefined

registerType :: Text -> TypeHandle -> TypeHandle -> Fact
registerType = undefined -- TODO: continue from here

registerConstraint :: Text -> TypeHandle -> Fact
registerConstraint = undefined -- TODO: continue from here

classConstraint :: ClassHandle -> [TypeHandle] -> Fact
classConstraint = undefined -- TODO: continue from here

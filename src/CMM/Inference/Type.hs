{-# LANGUAGE Safe #-}

module CMM.Inference.Type where

import safe Data.Text (Text)

data ClassHandle
    = Numeric
    | Real
    | Character
    | Address
    | Class Text

data TypeHandle
    = NoType
    | VarType Int
    | TBitsType Int
    | BoolType
    | AddrType TypeHandle
    | StringType
    | String16Type

data Fact
type Facts = [Fact]

makeFunction :: TypeHandle -> TypeHandle -> TypeHandle
makeFunction = undefined -- TODO: continue from here

makeTuple :: [TypeHandle] -> TypeHandle
makeTuple = undefined

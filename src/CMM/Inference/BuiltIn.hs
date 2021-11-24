{-# LANGUAGE Safe #-}

module CMM.Inference.BuiltIn where

import safe Data.Text (Text)

import safe CMM.AST
import safe CMM.Inference.Type

getNamedOperator :: Text -> (Maybe ClassHandle, TypeHandle)
getNamedOperator = undefined -- TODO: continue from here

getSymbolicOperator :: Op -> (Maybe ClassHandle, TypeHandle)
getSymbolicOperator = undefined -- TODO: continue from here

numericConstraint :: TypeHandle -> Fact
numericConstraint = classConstraint NumericClass . pure

realConstraint :: TypeHandle -> Fact
realConstraint = classConstraint RealClass . pure

characterConstraint :: TypeHandle -> Fact
characterConstraint = classConstraint CharacterClass . pure

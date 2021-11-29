{-# LANGUAGE Safe #-}

module CMM.Inference.BuiltIn where

import safe Data.Text (Text)

import safe CMM.AST as AST
import safe CMM.Inference.Type as Infer

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined -- TODO: continue from here

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined -- TODO: continue from here

numericConstraint :: Infer.Type -> Fact
numericConstraint = classConstraint NumericClass . pure

realConstraint :: Infer.Type -> Fact
realConstraint = classConstraint RealClass . pure

characterConstraint :: Infer.Type -> Fact
characterConstraint = classConstraint CharacterClass . pure

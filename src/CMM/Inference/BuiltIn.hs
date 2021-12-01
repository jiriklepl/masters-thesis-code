{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference.BuiltIn where

import safe Data.Text (Text)

import safe CMM.AST as AST
import safe CMM.Inference.Type as Infer

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined -- TODO: continue from here

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined -- TODO: continue from here

labelConstraint :: Infer.Type -> Fact
labelConstraint = classConstraint (ClassHandle "Label") . pure

numericConstraint :: Infer.Type -> Fact
numericConstraint = classConstraint (ClassHandle "Num") . pure

realConstraint :: Infer.Type -> Fact
realConstraint = classConstraint (ClassHandle "Real") . pure

characterConstraint :: Infer.Type -> Fact
characterConstraint = classConstraint (ClassHandle "Char") . pure

addressConstraint :: Infer.Type -> Fact
addressConstraint = classConstraint (ClassHandle "Address") . pure

-- TODO: add to `Preprocess`
addressKind :: Text
addressKind = "address"

-- TODO: add to `Preprocess`
floatKind :: Text
floatKind = "float"

-- TODO: add to `Preprocess`
integerKind :: Text
integerKind = ""

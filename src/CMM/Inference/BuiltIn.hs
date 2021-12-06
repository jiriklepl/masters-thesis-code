{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference.BuiltIn where

import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Text (Text)

import safe CMM.AST as AST
import safe CMM.Inference.Type as Infer

labelClassName :: Text
labelClassName = "Label"

numClassName :: Text
numClassName = "Num"

realClassName :: Text
realClassName = "Real"

charClassName :: Text
charClassName = "Char"

addressClassName :: Text
addressClassName = "Address"

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined -- TODO: continue from here

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined -- TODO: continue from here

labelConstraint :: TypeVar -> Fact
labelConstraint = classConstraint (ClassHandle labelClassName) . pure

numericConstraint :: TypeVar -> Fact
numericConstraint = classConstraint (ClassHandle numClassName) . pure

realConstraint :: TypeVar -> Fact
realConstraint = classConstraint (ClassHandle realClassName) . pure

characterConstraint :: TypeVar -> Fact
characterConstraint = classConstraint (ClassHandle charClassName) . pure

addressConstraint :: TypeVar -> Fact
addressConstraint = classConstraint (ClassHandle addressClassName) . pure

-- TODO: add to `Preprocess`
addressKind :: Text
addressKind = "address"

-- TODO: add to `Preprocess`
floatKind :: Text
floatKind = "float"

-- TODO: add to `Preprocess`
integerKind :: Text
integerKind = ""

builtInContext :: Facts
builtInContext = [] -- undefined

-- the constrained type has to be a subtype of an instance type
builtInClasses :: Map Text Class
builtInClasses = Map.fromList
  [ (numClassName, Class $ [Star] :. [] :=> [int 32, int 16, int 8, int 64])
  , (labelClassName, Class $ [Star] :. [] :=> [Inst $ [] :. [] :=> [LabelType]])
  , (addressClassName, Class $ [Star] :. [] :=> [Inst $ [Star] :. [] :=> [AddrType . VarType $ TypeLam 0 Star]])
  ]
  where int n = Inst $ [Star] :. [integerKind `KindLimit` TypeLam 0 Star, TypeLam 0 Star `typeConstraint` TBitsType n] :=> [VarType $ TypeLam 0 Star]

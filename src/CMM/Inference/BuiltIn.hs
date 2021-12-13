{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference.BuiltIn where

import safe Data.Map (Map)
import safe qualified Data.Map as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import safe Data.Set (Set)
import safe qualified Data.Set as Set
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

builtInKinds :: Bimap Text DataKind
builtInKinds = Bimap.fromList
  [ ("!false", FalseData)
  , ("!generic", GenericData)
  , ("address", addressKind)
  , ("float", floatKind)
  , ("", integerKind)
  ]

getDataKind :: Text -> DataKind
getDataKind name = if name `Bimap.member` builtInKinds
  then builtInKinds Bimap.! name
  else mempty

translateDataKind :: DataKind -> Maybe Text
translateDataKind name = if name `Bimap.memberR` builtInKinds
  then Just $ builtInKinds Bimap.!> name
  else Nothing

builtInRegisters :: Bimap Text Int
builtInRegisters = Bimap.fromList $
  zip
    [
    ]
    [0..]

-- TODO: add to `Preprocess`
addressKind :: DataKind
addressKind = DataKind addressRegisters

addressRegisters :: Set Int
addressRegisters = Set.fromList [0] -- TODO: just a placeholder

-- TODO: add to `Preprocess`
floatKind :: DataKind
floatKind = DataKind floatRegisters

floatRegisters :: Set Int
floatRegisters = Set.fromList [1] -- TODO: just a placeholder

-- TODO: add to `Preprocess`
integerKind :: DataKind
integerKind = DataKind integerRegisters

integerRegisters :: Set Int
integerRegisters = Set.fromList [0, 2] -- TODO: just a placeholder

builtInContext :: Facts
builtInContext = [] -- undefined

-- the constrained type has to be a subtype of an instance type
builtInClasses :: Map Text Class
builtInClasses = Map.fromList
  [ (numClassName, Class $ Set.singleton (TypeLam 0 Star Nothing) :. [] :=> [int 32, int 16, int 8, int 64])
  , (labelClassName, Class $ Set.singleton (TypeLam 0 Star Nothing) :. [] :=> [Inst $ mempty :. [] :=> [LabelType]])
  , (addressClassName, Class $ Set.singleton (TypeLam 0 Star Nothing) :. [] :=> [Inst $ Set.singleton (TypeLam 0 Star Nothing) :. [] :=> [AddrType . VarType $ TypeLam 0 Star Nothing]])
  ]
  where int n = Inst $ Set.singleton (TypeLam 0 Star Nothing) :. [integerKind `KindLimit` TypeLam 0 Star Nothing, TypeLam 0 Star Nothing `typeConstraint` TBitsType n] :=> [VarType $ TypeLam 0 Star Nothing]

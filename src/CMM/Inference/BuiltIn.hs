{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Inference.BuiltIn where

import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_2)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.String (IsString(fromString))
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty), dquotes)

import safe CMM.AST as AST (Op)
import safe CMM.Data.Bimap (Bimap)
import safe qualified CMM.Data.Bimap as Bimap
import safe CMM.Data.Ordered (Ordered(Ordered, unOrdered))
import safe CMM.Inference.DataKind (DataKind(DataKind, GenericData, Unstorable))
import safe CMM.Inference.Fact
  ( Facts
  , FlatFacts
  , kindConstraint
  , lockFact
  , regularExprConstraint
  )
import safe CMM.Inference.Type as Infer (Type)
import safe CMM.Inference.TypeCompl
  ( PrimType
  , TypeCompl(BoolType, ConstType, LabelType, String16Type, StringType,
          TBitsType, VoidType)
  )
import safe CMM.Inference.TypeKind (TypeKind((:->), Constraint, Star))
import safe CMM.Inference.TypeVar as Infer (TypeVar(NoType))

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined -- TODO

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined -- TODO

builtInKinds :: Bimap Text (Ordered DataKind)
builtInKinds =
  Bimap.fromList $
  (_2 %~ Ordered) <$>
  [ (unstorableKindName, Unstorable)
  , (genericKindName, GenericData)
  , (addressKindName, addressKind)
  , (floatKindName, floatKind)
  , (boolKindName, boolKind)
  , (integerKindName, integerKind)
  ]

instance Pretty DataKind where
  pretty = maybe errorKindName (dquotes . pretty) . translateDataKind

getDataKind :: Text -> DataKind
getDataKind name = maybe mempty unOrdered $ name `Bimap.lookup` builtInKinds

translateDataKind :: DataKind -> Maybe Text
translateDataKind name = Ordered name `Bimap.lookupR` builtInKinds

builtInRegisters :: Bimap Text Int
builtInRegisters = Bimap.fromList $ zip [] [0 ..]

errorKindName :: (IsString a, Semigroup a) => a
errorKindName = builtInPrefix <> "error"

genericKindName :: (IsString a, Semigroup a) => a
genericKindName = builtInPrefix <> "generic"

unstorableKindName :: (IsString a, Semigroup a) => a
unstorableKindName = builtInPrefix <> "unstorable"

addressKindName :: IsString a => a
addressKindName = "address"

-- TODO: add to `Preprocess` (after adding the typed labels)
addressKind :: DataKind
addressKind = DataKind addressRegisters

addressRegisters :: Set Int
addressRegisters = Set.fromList [0] -- TODO: just a placeholder

floatKindName :: IsString a => a
floatKindName = "float"

-- TODO: add to `Preprocess`
floatKind :: DataKind
floatKind = DataKind floatRegisters

floatRegisters :: Set Int
floatRegisters = Set.fromList [1] -- TODO: just a placeholder

integerKindName :: IsString a => a
integerKindName = ""

-- TODO: add to `Preprocess`
integerKind :: DataKind
integerKind = DataKind integerRegisters

integerRegisters :: Set Int
integerRegisters = Set.fromList [0, 2] -- TODO: just a placeholder

boolKindName :: IsString a => a
boolKindName = "bool"

boolKind :: DataKind
boolKind = DataKind boolRegisters

boolRegisters :: Set Int
boolRegisters = Set.fromList [3] -- TODO: just a placeholder

builtInContext :: Facts
builtInContext = [] -- undefined

builtInTypeFacts :: FlatFacts
builtInTypeFacts =
  (kindConstraint GenericData <$> abstractTypes) <>
  (regularExprConstraint <$> abstractTypes) <> (lockFact <$> abstractTypes)
  where
    abstractTypes :: [PrimType]
    abstractTypes =
      [ LabelType
      , StringType
      , String16Type
      , BoolType
      , VoidType
      , TBitsType 8
      , TBitsType 16
      , TBitsType 32
      , TBitsType 64
      ]

builtInPrefix :: IsString a => a
builtInPrefix = "!"

constraintWitness :: (IsString a) => a
constraintWitness = fromString $ builtInPrefix <> "constraintWitness"

getConstType :: String -> TypeCompl a
getConstType name
  | name == constraintWitness =
    ConstType constraintWitness (Constraint :-> Star) NoType
  | otherwise = error "(internal) Tried to retrieve a nonexistent type constant"

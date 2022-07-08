{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Inference.BuiltIn where

import safe Control.Lens ( (%~), _2 )
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
  ( FlatFacts
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

-- | gets a named operator supported on the architecture
getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined -- NOTE: the imaginary architecture does not support any named operators

-- | gets a symbolic operator supported on the architecture
getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined -- NOTE: the imaginary architecture does not support any symbolic operators on top of the standard ones

-- | Maps data kind names to the corresponding data kinds and back
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

-- | returns a data kind on the architecture from the given name
getDataKind :: Text -> DataKind
getDataKind name = maybe mempty unOrdered $ name `Bimap.lookup` builtInKinds

-- | returns a data kind name representing the given data kind
translateDataKind :: DataKind -> Maybe Text
translateDataKind name = Ordered name `Bimap.lookupR` builtInKinds

-- | returns a bijection between register names of the architecture and their numeric identifiers
builtInRegisters :: Bimap Text Int
builtInRegisters = Bimap.fromList $ zip [integerRegisterName, floatRegisterName] [0 ..]

-- | name of the imaginary integer register
integerRegisterName :: IsString a => a
integerRegisterName = "integer"

-- | name of the imaginary float register
floatRegisterName :: IsString a => a
floatRegisterName = "float"

-- | name of an unknown data kind
errorKindName :: (IsString a, Semigroup a) => a
errorKindName = builtInPrefix <> "error"

-- | name of the generic data kind (contains all registers)
genericKindName :: (IsString a, Semigroup a) => a
genericKindName = builtInPrefix <> "generic"

-- | name of the unstorable data kind (contains no registers)
unstorableKindName :: (IsString a, Semigroup a) => a
unstorableKindName = builtInPrefix <> "unstorable"

-- | name of the address data kind, see `addressKind`
addressKindName :: IsString a => a
addressKindName = "address"

-- | the address data kind, see `addressRegisters`
addressKind :: DataKind
addressKind = DataKind addressRegisters

-- | list of registers that can hold addresses
addressRegisters :: Set Int
addressRegisters = Set.fromList [0] -- NOTE: the imaginary architecture supports only one address register

-- | name of the float data kind, see `floatKind`
floatKindName :: IsString a => a
floatKindName = "float"

-- | the float data kind, see `floatRegisters`
floatKind :: DataKind
floatKind = DataKind floatRegisters

-- | list of registers that can hold floating point numbers
floatRegisters :: Set Int
floatRegisters = Set.fromList [1] -- NOTE: the imaginary architecture supports only one floating point number register

-- | name of the integer data kind, see `integerKind`
integerKindName :: IsString a => a
integerKindName = ""

-- | the integer data kind, see `integerRegisters`
integerKind :: DataKind
integerKind = DataKind integerRegisters

-- | list of registers that can hold integers
integerRegisters :: Set Int
integerRegisters = Set.fromList [0, 2] -- NOTE: the imaginary architecture supports two integer registers, one of which is also address

-- | name of the bool data kind, see `boolKind`
boolKindName :: IsString a => a
boolKindName = "bool"

-- | the bool data kind, see `boolRegisters`
boolKind :: DataKind
boolKind = DataKind boolRegisters

-- | list of registers that can hold boolean values
boolRegisters :: Set Int
boolRegisters = Set.fromList [3] -- NOTE: the imaginary architecture supports only one boolean register

-- | The constraints constraining the known types
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

-- | Prefix given to the builtin helper objects to avoid name collisions
builtInPrefix :: IsString a => a
builtInPrefix = "!"

-- | The name of the type constructor that transforms a constraint to a value
constraintWitnessName :: (IsString a) => a
constraintWitnessName = fromString $ builtInPrefix <> "constraintWitness"

-- | Retrieves a builtin constant type constructor; NOTE: currently, only "constraintWitness"
getConstType :: String -> TypeCompl a
getConstType name
  | name == constraintWitnessName =
    ConstType constraintWitnessName (Constraint :-> Star) NoType
  | otherwise = error "(internal) Tried to retrieve a nonexistent type constant"

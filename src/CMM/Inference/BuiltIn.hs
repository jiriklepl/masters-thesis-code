{-# LANGUAGE Trustworthy #-}

module CMM.Inference.BuiltIn where

import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_2)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe CMM.AST as AST (Op)
import safe CMM.Data.Ordered (Ordered(..))
import safe CMM.Inference.DataKind (DataKind(DataKind, GenericData, Unstorable))
import safe CMM.Inference.Type as Infer
  ( Type
  )
import safe CMM.Inference.TypeVar as Infer ( TypeVar(NoType) )
import safe CMM.Inference.TypeKind (TypeKind(..))
import safe CMM.Inference.TypeCompl
    ( TypeCompl(..),
      PrimType )
import safe CMM.Inference.Fact
    ( Facts, FlatFacts, regularExprConstraint, kindConstraint )

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined

builtInKinds :: Bimap Text (Ordered DataKind)
builtInKinds =
  Bimap.fromList $
  (_2 %~ Ordered) <$>
  [ ("!unstorable", Unstorable)
  , ("!generic", GenericData)
  , ("address", addressKind)
  , ("float", floatKind)
  , ("bool", boolKind)
  , ("", integerKind)
  ]

getDataKind :: Text -> DataKind
getDataKind name =
  if name `Bimap.member` builtInKinds
    then unOrdered $ builtInKinds Bimap.! name
    else mempty

translateDataKind :: DataKind -> Maybe Text
translateDataKind name =
  if Ordered name `Bimap.memberR` builtInKinds
    then Just $ builtInKinds Bimap.!> Ordered name
    else Nothing

builtInRegisters :: Bimap Text Int
builtInRegisters = Bimap.fromList $ zip [] [0 ..]

-- TODO: add to `Preprocess` (after adding the typed labels)
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

boolKind :: DataKind
boolKind = DataKind boolRegisters

boolRegisters :: Set Int
boolRegisters = Set.fromList [3] -- TODO: just a placeholder

builtInContext :: Facts
builtInContext = [] -- undefined

builtInTypeFacts :: FlatFacts
builtInTypeFacts = (kindFact <$> types) <> (constFact <$> types)
  where
    types =
      [ LabelType
      , StringType
      , String16Type
      , BoolType
      , LabelType
      , VoidType
      , TBitsType 8
      , TBitsType 16
      , TBitsType 32
      , TBitsType 64
      ]
    kindFact primType = GenericData `kindConstraint` (primType :: PrimType)
    constFact primType = regularExprConstraint (primType :: PrimType)

getConstType :: String -> TypeCompl a
getConstType "constraintWitness" =
  ConstType "constraintWitness" (Constraint :-> Star) NoType
getConstType _ =
  error "(internal) Tried to retrieve a nonexistent type constant"

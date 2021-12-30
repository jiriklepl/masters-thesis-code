{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference.BuiltIn where

import Data.Bimap (Bimap)
import safe Control.Lens.Tuple
import safe Control.Lens.Setter
import qualified Data.Bimap as Bimap
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe CMM.AST as AST (Op)
import safe CMM.Data.Orderable
import safe CMM.Inference.Type as Infer
  ( DataKind(DataKind, FalseData, GenericData)
  , Facts
  , Type
  , OrdDataKind
  )

getNamedOperator :: Text -> Infer.Type
getNamedOperator = undefined

getSymbolicOperator :: Op -> Infer.Type
getSymbolicOperator = undefined

builtInKinds :: Bimap Text OrdDataKind
builtInKinds =
  Bimap.fromList $ (_2 %~ makeOrdered) <$>
    [ ("!false", FalseData)
    , ("!generic", GenericData)
    , ("address", addressKind)
    , ("float", floatKind)
    , ("", integerKind)
    ]

getDataKind :: Text -> DataKind
getDataKind name =
  if name `Bimap.member` builtInKinds
    then unmakeOrdered $ builtInKinds Bimap.! name
    else mempty

translateDataKind :: DataKind -> Maybe Text
translateDataKind name =
  if makeOrdered name `Bimap.memberR` builtInKinds
    then Just $ builtInKinds Bimap.!> makeOrdered name
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

builtInContext :: Facts
builtInContext = [] -- undefined

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Translator.State where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Text (Text)

import safe qualified LLVM.AST.Operand as L

import safe CMM.AST.BlockAnnot (BlockData)
import safe CMM.Err.State (ErrorState)
import qualified CMM.Inference.Type as Ty
import safe qualified LLVM.AST.Name as L
import safe CMM.Inference.State ( InferencerState )

data TranslState =
  TranslState
    { _controlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , _blockData :: BlockData -- We need the block data to create the phi nodes
    , _currentBlock :: Maybe Int
    , _blocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , _errorState :: ErrorState
    , _offSets :: Map Int Int
    , _rename :: Map L.Operand L.Operand
    , _inferencer :: InferencerState
    , _structs :: Map Text ([(Text, Int)], [Ty.Type])
    }

makeFieldsNoPrefix ''TranslState

initTranslState :: TranslState
initTranslState =
  TranslState -- FIXME: this is just DUMMY
    { _controlFlow = mempty
    , _blockData = mempty
    , _currentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , _blocksTable = mempty
    , _errorState = mempty
    , _offSets = mempty
    , _rename = mempty
    , _inferencer = undefined
    , _structs = undefined
    }

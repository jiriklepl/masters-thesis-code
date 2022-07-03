{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Translator.State where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Text (Text)

import safe qualified LLVM.AST.Operand as L

import safe CMM.AST.BlockAnnot (BlockData)
import safe CMM.Err.State (ErrorState)
import safe CMM.Inference.State ( InferencerState )
import qualified LLVM.AST.Type as L
import Control.Lens
import Control.Monad.State

data TranslState =
  TranslState
    { _controlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , _blockData :: BlockData -- We need the block data to create the phi nodes
    , _currentBlock :: Maybe Int
    , _blocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , _errorState :: ErrorState
    , _offSets :: Map Int Int
    , _inferencer :: InferencerState
    , _records :: Map Text L.Operand
    , _structs :: Map Text ([(Text, Int)], [L.Type])
    }

makeFieldsNoPrefix ''TranslState

initTranslState :: TranslState
initTranslState =
  TranslState -- FIXME: this is just DUMMY
    { _controlFlow = undefined
    , _blockData = undefined
    , _currentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , _blocksTable = undefined
    , _offSets = undefined
    , _inferencer = undefined
    , _records = undefined
    , _errorState = mempty
    , _structs = mempty
    }

clearTranslState :: MonadState TranslState m => m ()
clearTranslState = do
  records .= mempty

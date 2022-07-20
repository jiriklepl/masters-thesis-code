{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Translator.State where

import safe Control.Lens ((.=), (^.), makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Text (Text)

import safe qualified LLVM.AST.Operand as L

import safe CMM.AST.BlockAnnot (BlockData)
import safe CMM.AST.Blockifier.State
  ( BlockifierState
  , allBlocks
  , allData
  , allFlow
  , numBlocks
  )
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.State (InferencerState)
import safe Control.Monad.State (MonadState)
import qualified LLVM.AST.Type as L

data TranslState =
  TranslState
    { _controlFlow :: [(Int, Int)] -- ^ We need the control flow to create the phi nodes
    , _blockData :: BlockData -- ^ We need the block data to create the phi nodes
    , _blocksTable :: Map Int Text -- ^ All GOTOs etc call blocks by their names
    , _errorState :: ErrorState -- ^ The errors generated during translation
    , _offSets :: Map Int Int -- ^ For each entry point of a procedure, the count of the procedure's basic blocks
    , _inferState :: InferencerState -- ^ Inferencer state
    , _records :: Map Text L.Operand -- ^ Mapping from names to stack-allocated objects represented by operands
    , _structs :: Map Text ([(Text, Int)], [L.Type]) -- ^ For each struct, contains pairs ([(fieldAlias,fieldIndex)], [field])
    }

makeFieldsNoPrefix ''TranslState

initTranslState :: InferencerState -> BlockifierState -> TranslState
initTranslState iState bState =
  TranslState
    { _controlFlow = bState ^. allFlow
    , _blockData = bState ^. allData
    , _blocksTable = bState ^. allBlocks
    , _offSets = bState ^. numBlocks
    , _inferState = iState
    , _records = undefined
    , _errorState = mempty
    , _structs = mempty
    }

clearTranslState :: MonadState TranslState m => m ()
clearTranslState = do
  records .= mempty

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Blockifier.State.Impl where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Int (Int)
import safe Data.Map (Map)
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.BlockAnnot (BlockData, BlockVars)
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Parser.HasPos (SourcePos)

data BlockifierState =
  BlockifierState
    { _controlFlow :: [(Int, Int)] -- ^ [(from, to)] edges in the control-flow graph
    , _blocksTable :: Map Text Int -- ^ Maps block names to their respective indices
    , _currentBlock :: Maybe Int -- ^ Contains the index of the current block
    , _currentData :: BlockVars -- ^ Contains information about variables inside the current block
    , _blockData :: BlockData
    , _registers :: Map Text SourcePos
    , _imports :: Map Text SourcePos
    , _constants :: Map Text SourcePos
    , _stackLabels :: Map Text SourcePos
    , _labels :: Map Text SourcePos
    , _continuations :: Map Text SourcePos
    , _errorState :: ErrorState
    }
  deriving (Show)

initBlockifier :: BlockifierState
initBlockifier =
  BlockifierState
    { _controlFlow = mempty
    , _blocksTable = mempty
    , _currentBlock = Nothing
    , _currentData = mempty
    , _blockData = mempty
    , _registers = mempty
    , _imports = mempty
    , _constants = mempty
    , _stackLabels = mempty
    , _labels = mempty
    , _continuations = mempty
    , _errorState = mempty
    }

makeFieldsNoPrefix ''BlockifierState

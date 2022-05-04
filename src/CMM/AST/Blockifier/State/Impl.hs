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

-- | Contains various data used by `Blockifier`
data BlockifierState =
  BlockifierState
    { _controlFlow :: [(Int, Int)] -- ^ [(from, to)] edges in the control-flow graph
    , _blocksTable :: Map Text Int -- ^ Maps block names to their respective indices
    , _currentBlock :: Maybe Int -- ^ Contains the index of the current block
    , _currentData :: BlockVars -- ^ Contains information for variables in the current block
    , _blockData :: BlockData -- ^ Contains information for variables in all closed blocks
    , _registers :: Map Text SourcePos -- ^ `Register` variables valid inside the current scope
    , _imports :: Map Text SourcePos -- ^ `Import` variables valid inside the current scope
    , _constants :: Map Text SourcePos -- ^ Constants valid inside the current scope
    , _stackLabels :: Map Text SourcePos -- ^ Stack labels valid inside the current scope
    , _labels :: Map Text SourcePos -- ^ Regular labels inside the current scope
    , _continuations :: Map Text SourcePos -- ^ Continuations inside the current scope
    , _errorState :: ErrorState -- ^ The current `ErrorState`
    }
  deriving (Show)

-- | Initiates an empty `BlockifierState`
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

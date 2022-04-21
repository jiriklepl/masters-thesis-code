{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Blockifier.State.Impl where

import safe Control.Lens.TH (makeLenses)
import safe Data.Int (Int)
import safe Data.Map (Map)
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Set (Set)
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.BlockAnnot (BlockData, BlockVars)

data Blockifier =
  Blockifier
    { _controlFlow :: [(Int, Int)] -- | [(from, to)] edges in the control-flow graph
    , _blocksTable :: Map Text Int -- | Maps block names to their respective indices
    , _currentBlock :: Maybe Int -- | Contains the index of the current block
    , _currentData :: BlockVars -- | Contains information about variables inside the current block
    , _blockData :: BlockData
    , _registers :: Set Text
    , _imports :: Set Text
    , _constants :: Set Text
    , _stackLabels :: Set Text
    , _labels :: Set Text
    , _continuations :: Set Text
    , _errors :: Int -- TODO: move to a separate state ?
    , _warnings :: Int -- TODO: move to a separate state ?
    }
  deriving (Show)

initBlockifier :: Blockifier
initBlockifier =
  Blockifier
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
    , _errors = 0
    , _warnings = 0
    }

makeLenses ''Blockifier

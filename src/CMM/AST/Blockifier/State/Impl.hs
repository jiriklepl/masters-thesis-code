{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Blockifier.State.Impl where

import safe Prelude

import safe Control.Lens.TH (makeLenses)
import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Text (Text)

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

makeLenses ''Blockifier

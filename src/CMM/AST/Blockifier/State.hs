{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Blockifier.State where

import safe Control.Lens ((%=), (.=), (<>=), makeFieldsNoPrefix, use, uses)
import safe Control.Monad.State (State)
import safe Data.Data (Data)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Text (Text)
import safe Data.Tuple (swap)

import safe CMM.AST.BlockAnnot (BlockData, BlockVars, WithBlockAnnot)
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Parser.GetPos (GetPos, SourcePos)

-- | Contains various data used by `Blockifier`
data BlockifierState =
  BlockifierState
    { _currentFlow :: [(Int, Int)] -- ^ [(from, to)] edges in the control-flow graph
    , _allFlow :: [(Int, Int)] -- ^ [(from, to)] edges in the control-flow graph
    , _blocksCache :: Map Text Int -- ^ Maps block names to their respective indices
    , _drops :: Map Int (Set Text) -- ^ Maps block indexes to sets of dropped contracts
    , _allBlocks :: Map Int Text -- ^ Maps block names to their respective indices
    , _numBlocks :: Map Int Int -- ^ Maps each procedure's entry block to the number of blocks of that procedure
    , _currentBlock :: Maybe Int -- ^ Contains the index of the current block
    , _currentData :: BlockVars -- ^ Contains information for variables in the current block
    , _allData :: BlockData -- ^ Contains information for variables in all closed blocks
    , _cacheData :: BlockData -- ^ Contains information for variables in all closed blocks
    , _registers :: Map Text SourcePos -- ^ `Register` variables valid inside the current scope
    , _imports :: Map Text SourcePos -- ^ `Import` variables valid inside the current scope
    , _constants :: Map Text SourcePos -- ^ Constants valid inside the current scope
    , _stackLabels :: Map Text SourcePos -- ^ Stack labels valid inside the current scope
    , _labels :: Map Text SourcePos -- ^ Regular labels inside the current scope
    , _continuations :: Map Text SourcePos -- ^ Continuations inside the current scope
    , _procedureCounter :: Int -- ^ Counts procedures
    , _errorState :: ErrorState -- ^ The current `ErrorState`
    }
  deriving (Show)

-- | Initiates an empty `BlockifierState`
initBlockifier :: BlockifierState
initBlockifier =
  BlockifierState
    { _currentFlow = mempty
    , _allFlow = mempty
    , _allBlocks = mempty
    , _blocksCache = mempty
    , _currentBlock = Nothing
    , _numBlocks = mempty
    , _currentData = mempty
    , _cacheData = mempty
    , _allData = mempty
    , _drops = mempty
    , _registers = mempty
    , _imports = mempty
    , _constants = mempty
    , _stackLabels = mempty
    , _procedureCounter = 0
    , _labels = mempty
    , _continuations = mempty
    , _errorState = mempty
    }

makeFieldsNoPrefix ''BlockifierState

-- | Type constructor for blockifier function return types
type Blockifier = State BlockifierState

type BlockifyAssumps a b = (Data b, WithBlockAnnot a b, GetPos a, GetPos b)

-- | Resets `Blockifier` between different functions
clearBlockifier :: Blockifier ()
clearBlockifier = do
  labels .= mempty
  stackLabels .= mempty
  continuations .= mempty
  registers .= mempty
  currentBlock .= Nothing
  currentData .= mempty
  drops .= mempty
  controlFlow' <- use currentFlow
  allFlow <>= controlFlow'
  currentFlow .= mempty
  blocksCache' <- use blocksCache
  off <- uses allBlocks Map.size
  numBlocks %= Map.insert off (Map.size blocksCache')
  allBlocks <>= (Map.fromList . fmap swap . Map.toList) blocksCache'
  blocksCache .= mempty
  cacheData' <- use cacheData
  allData <>= cacheData'
  cacheData .= mempty

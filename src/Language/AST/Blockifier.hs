{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Language.AST.Blockifier where

import Control.Lens.TH
import safe Control.Monad.State.Lazy
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe Language.AST.BlockAnnot

type BlockifierMonad = MonadState Blockifier

data Blockifier =
  Blockifier
    { _controlFlow :: [(Int, Int)] -- [(from, to)]
    , _blocksTable :: Map Text Int
    , _currentBlock :: Maybe Int
    , _currentData :: BlockVars
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

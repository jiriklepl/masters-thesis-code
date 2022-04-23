{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Translator.State where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Int (Int)
import safe Data.Map (Map)
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)

import safe qualified LLVM.AST.Operand as L

import safe CMM.AST.BlockAnnot (BlockData)
import safe CMM.Err.State (ErrorState)

data TranslState =
  TranslState
    { _variables :: [Map Text L.Operand]
    , _controlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , _blockData :: BlockData -- We need the block data to create the phi nodes
    , _currentBlock :: Maybe Int
    , _blocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , _errorState :: ErrorState
    }

makeFieldsNoPrefix ''TranslState

initTranslState :: TranslState
initTranslState =
  TranslState -- FIXME: this is just DUMMY
    { _variables = mempty
    , _controlFlow = mempty
    , _blockData = mempty
    , _currentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , _blocksTable = mempty
    , _errorState = mempty
    }

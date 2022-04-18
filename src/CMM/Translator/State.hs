{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Translator.State where

import safe Prelude

import safe Control.Lens.TH (makeLenses)
import safe Data.Map (Map)
import safe Data.Text (Text)
import safe qualified LLVM.AST.Operand as L

import safe CMM.AST.BlockAnnot (BlockData)

data TranslState =
  TranslState
    { _variables :: [Map Text L.Operand]
    , _controlFlow :: [(Int, Int)] -- We need the control flow to create the phi nodes
    , _blockData :: BlockData -- We need the block data to create the phi nodes
    , _currentBlock :: Maybe Int
    , _blocksTable :: Map Int Text -- All GOTOs etc call blocks by their names
    , _errors :: Int
    , _warnings :: Int
    }

makeLenses ''TranslState

initTranslState :: TranslState
initTranslState =
  TranslState -- FIXME: this is just DUMMY
    { _variables = mempty
    , _controlFlow = mempty
    , _blockData = mempty
    , _currentBlock = Nothing -- TODO: change to (Just 0) in procedure translation
    , _blocksTable = mempty
    , _errors = 0
    , _warnings = 0
    }

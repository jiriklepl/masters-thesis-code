{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module CMM.AST.BlockifierState where

import Control.Lens.TH
import safe Control.Lens.Setter
import safe Control.Monad.State.Lazy
import safe Data.Text (Text)
import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Prettyprinter (Pretty)

import safe CMM.AST.BlockAnnot
import safe CMM.Parser.HasPos
import safe CMM.Warnings

type MonadBlockifier m = (MonadState Blockifier m, MonadIO m)

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

registerError :: (HasPos n, Pretty n, MonadBlockifier m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning :: (HasPos n, Pretty n, MonadBlockifier m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message


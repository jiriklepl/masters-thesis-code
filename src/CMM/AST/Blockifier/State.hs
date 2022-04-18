{-# LANGUAGE Safe #-}

module CMM.AST.Blockifier.State
  ( module CMM.AST.Blockifier.State
  , module CMM.AST.Blockifier.State.Impl
  ) where

import safe Control.Lens.Setter ((+=), (.=))
import safe Control.Monad.State.Lazy (MonadIO, MonadState)
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)
import safe Prettyprinter (Pretty)

import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Warnings (makeMessage, mkError, mkWarning)

import safe CMM.AST.Blockifier.State.Impl

type MonadBlockifier m = (MonadState Blockifier m, MonadIO m)

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

clearBlockifier :: MonadBlockifier m => m ()
clearBlockifier = do
  labels .= mempty
  stackLabels .= mempty
  continuations .= mempty
  registers .= mempty

registerError :: (HasPos n, Pretty n, MonadBlockifier m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning :: (HasPos n, Pretty n, MonadBlockifier m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message

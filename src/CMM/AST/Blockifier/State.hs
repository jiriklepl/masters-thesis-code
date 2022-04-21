{-# LANGUAGE Safe #-}

module CMM.AST.Blockifier.State
  ( module CMM.AST.Blockifier.State
  , module CMM.AST.Blockifier.State.Impl
  ) where

import safe Control.Lens.Setter ((+=), (.=))
import safe Control.Monad.State.Lazy (MonadIO, MonadState)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)
import safe Prettyprinter (Pretty)

import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Warnings (makeMessage, mkError, mkWarning)

import safe CMM.AST.Blockifier.State.Impl
  ( Blockifier(Blockifier)
  , blockData
  , blocksTable
  , constants
  , continuations
  , controlFlow
  , currentBlock
  , currentData
  , errors
  , imports
  , initBlockifier
  , labels
  , registers
  , stackLabels
  , warnings
  )

type MonadBlockifier m = (MonadState Blockifier m, MonadIO m)

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

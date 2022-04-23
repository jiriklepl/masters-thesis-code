{-# LANGUAGE Safe #-}

module CMM.AST.Blockifier.State
  ( module CMM.AST.Blockifier.State
  , module CMM.AST.Blockifier.State.Impl
  ) where

import safe Control.Lens.Setter ((.=))
import safe Control.Monad.State (MonadState)
import safe Data.Monoid (Monoid(mempty))

import safe CMM.AST.Blockifier.State.Impl
  ( Blockifier(Blockifier)
  , blockData
  , blocksTable
  , constants
  , continuations
  , controlFlow
  , currentBlock
  , currentData
  , imports
  , initBlockifier
  , labels
  , registers
  , stackLabels
  )

type MonadBlockifier m = MonadState Blockifier m

clearBlockifier :: MonadBlockifier m => m ()
clearBlockifier = do
  labels .= mempty
  stackLabels .= mempty
  continuations .= mempty
  registers .= mempty

{-# LANGUAGE Safe #-}

module CMM.AST.Blockifier.State
  ( module CMM.AST.Blockifier.State
  , module CMM.AST.Blockifier.State.Impl
  ) where

import safe Control.Lens.Setter ((.=))
import safe Control.Monad.State (State)
import safe Data.Monoid (Monoid(mempty))

import safe CMM.AST.Blockifier.State.Impl
  ( BlockifierState(BlockifierState)
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

type Blockifier = State BlockifierState

clearBlockifier :: Blockifier ()
clearBlockifier = do
  labels .= mempty
  stackLabels .= mempty
  continuations .= mempty
  registers .= mempty

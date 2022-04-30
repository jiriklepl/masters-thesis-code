{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Blockifier.Error where

import safe Data.Eq (Eq)
import safe Data.Function ((.))
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.HasName (HasName(getName))
import safe CMM.AST.Variables.SymbolType (SymbolType)
import safe CMM.Err.IsError (IsError)

data BlockifierError
  = UnreachableLabels [Text] -- ^ List of unreachable labels in a certain function
  | UnreachableContinuations [Text] -- ^ List of unreachable continuations in a certain function
  | UnreachableStatement -- ^ Unreachable statement in a function
  | UninitializedRegisters [Text] -- ^ List of uninitialized registers in a certain function
  | DuplicateSymbol SymbolType Text -- ^ Duplicate variable symbol of type `SymbolType` (in a function)
  | ContinuationFallthrough Text -- TODO: print "Fallthrough to a continuation is forbidden"
  | GotoWithoutTargets -- TODO: print "Indirect goto statement without specified targets is illegal"
  | FlatteningInconsistency -- TODO: print "Compilation internal failure in the flattening phase"
  deriving (Show, Eq, IsError)

-- | `DuplicateSymbol` generalized over inputs in `HasName`
duplicateSymbol :: HasName n => SymbolType -> n -> BlockifierError
duplicateSymbol s = DuplicateSymbol s . getName

-- | `ContinuationFallthrough` generalized over inputs in `HasName`
continuationFallthrough :: HasName n => n -> BlockifierError
continuationFallthrough = ContinuationFallthrough . getName

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Blockifier.Error where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty), (<+>))

import safe CMM.AST (Stmt)
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.AST.Variables.SymbolType (SymbolType)
import safe CMM.Err.IsError (IsError)
import safe CMM.Pretty (commaSep)

-- | The errors used by the `Blockifier`
data BlockifierError
  = UnreachableLabels [Text] -- ^ List of unreachable labels in a certain function
  | UnreachableContinuations [Text] -- ^ List of unreachable continuations in a certain function
  | UnreachableStatement (Stmt ()) -- ^ Unreachable statement in a function
  | UninitializedRegisters [Text] -- ^ List of uninitialized registers in a certain function
  | DuplicateSymbol SymbolType Text -- ^ Duplicate variable symbol of type `SymbolType` (in a function)
  | ContinuationFallthrough Text -- ^ Fallthrough to a continuation is forbidden
  | GotoWithoutTargets (Stmt ()) -- ^ Indirect goto statement without specified targets is illegal
  | FlatteningInconsistency (Stmt ()) -- ^ Compilation internal failure in the flattening phase
  | ProcedureFallthrough -- ^ Fallthrough to the end of procedure
  | DroppingOutsideBlock Text -- ^ Attempted to make `dropped` outside of control flow
  | ReDropping Text -- ^ Attempted to make `reDropped`
  | AlternativePath Text -- ^ Alternative path to a dropped item
  deriving (Eq, Show, IsError, Data)

instance Pretty BlockifierError where
  pretty =
    \case
      UnreachableLabels names
        | [name] <- names -> "Unreachable label:" <+> pretty name
        | otherwise -> "Unreachable labels:" <+> commaSep (pretty <$> names)
      UnreachableContinuations names
        | [name] <- names -> "Unreachable continuation:" <+> pretty name
        | otherwise ->
          "Unreachable continuations:" <+> commaSep (pretty <$> names)
      UnreachableStatement stmt -> "Unreachable statement:" <+> pretty stmt
      UninitializedRegisters names ->
        "Uninitialized registers:" <+> commaSep (pretty <$> names)
      DuplicateSymbol symbolType name ->
        "Duplicate" <+> pretty symbolType <> ":" <+> pretty name
      ContinuationFallthrough name ->
        "Fallthrough to a continuation:" <+> pretty name
      ProcedureFallthrough -> "Fallthrough to the end of procedure"
      GotoWithoutTargets stmt ->
        "Goto statement with no targets:" <+> pretty stmt
      FlatteningInconsistency stmt -> "Nonflat statement:" <+> pretty stmt
      DroppingOutsideBlock name ->
        "Attempted to make" <+>
        pretty name <+> " `dropped` outside of control flow"
      ReDropping name ->
        "Attempted to make" <+> pretty name <+> " `dropped` twice"
      AlternativePath name ->
        "Alternative path to a dropped item:" <+> pretty name

-- | `DuplicateSymbol` generalized over inputs in `GetName`
duplicateSymbol :: GetName n => SymbolType -> n -> BlockifierError
duplicateSymbol s = DuplicateSymbol s . getName

-- | `ContinuationFallthrough` generalized over inputs in `GetName`
continuationFallthrough :: GetName n => n -> BlockifierError
continuationFallthrough = ContinuationFallthrough . getName

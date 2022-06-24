{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Blockifier.Error where

import safe Data.Eq (Eq)
import safe Data.Function ((.))
import safe Data.Text (Text)
import safe Text.Show (Show)
import safe Data.Bool ( otherwise )
import safe Data.Functor ( (<$>) )
import safe Data.Data ( Data )

import safe Prettyprinter ( (<>), Pretty(pretty), (<+>) )

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.AST.Variables.SymbolType (SymbolType)
import safe CMM.Err.IsError (IsError)
import safe CMM.Pretty ( commaSep )
import safe CMM.AST ( Stmt )

-- | The errors used by the `Blockifier`
data BlockifierError
  = UnreachableLabels [Text]  -- ^ List of unreachable labels in a certain function
  | UnreachableContinuations [Text]  -- ^ List of unreachable continuations in a certain function
  | UnreachableStatement (Stmt ())  -- ^ Unreachable statement in a function
  | UninitializedRegisters [Text]  -- ^ List of uninitialized registers in a certain function
  | DuplicateSymbol SymbolType Text  -- ^ Duplicate variable symbol of type `SymbolType` (in a function)
  | ContinuationFallthrough Text  -- TODO: print "Fallthrough to a continuation is forbidden"
  | GotoWithoutTargets (Stmt ())   -- TODO: print "Indirect goto statement without specified targets is illegal"
  | FlatteningInconsistency (Stmt ())  -- TODO: print "Compilation internal failure in the flattening phase"
  deriving (Eq, Show, IsError, Data)

instance Pretty BlockifierError where
  pretty = \case
    UnreachableLabels names
      | [name] <- names -> "Unreachable label:" <+> pretty name
      | otherwise -> "Unreachable labels:" <+> commaSep (pretty <$> names)
    UnreachableContinuations names
      | [name] <- names -> "Unreachable continuation:" <+> pretty name
      | otherwise -> "Unreachable continuations:" <+> commaSep (pretty <$> names)
    UnreachableStatement stmt -> "Unreachable statement:" <+> pretty stmt
    UninitializedRegisters names -> "Uninitialized registers:" <+> commaSep (pretty <$> names)
    DuplicateSymbol symbolType name -> "Duplicate" <+> pretty symbolType <> ":" <+> pretty name
    ContinuationFallthrough name -> "Fallthrough to a continuation:" <+> pretty name
    GotoWithoutTargets stmt -> "Goto statement with no targets:" <+> pretty stmt
    FlatteningInconsistency stmt -> "Nonflat statement:" <+> pretty stmt

-- | `DuplicateSymbol` generalized over inputs in `GetName`
duplicateSymbol :: GetName n => SymbolType -> n -> BlockifierError
duplicateSymbol s = DuplicateSymbol s . getName

-- | `ContinuationFallthrough` generalized over inputs in `GetName`
continuationFallthrough :: GetName n => n -> BlockifierError
continuationFallthrough = ContinuationFallthrough . getName

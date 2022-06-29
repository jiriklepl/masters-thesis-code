{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Unify.Error where

import safe Data.Text (Text)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty), (<+>), parens )

import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)

-- | The errors that can happen during unification
data UnificationError
  = Occurs TypeVar Type -- ^ Occurs check failure (the given type variable occurs in the given type)
  | Mismatch Text Type Type -- ^ The two given types cannot be unified
  | GotErrorType Text -- ^ Encountered an error type during unification
  | BadKind Type Type -- ^ The type kinds of the given types do not match
  deriving (Show, Eq, IsError, Data)


instance Pretty UnificationError where
  pretty = \case
    Occurs tVar t -> "Occurs:" <+> pretty tVar <+> "in" <+> pretty t
    Mismatch reason tVar tVar' -> "Type mismatch:" <+> pretty tVar <+> "and" <+> pretty tVar' <+> parens (pretty reason)
    GotErrorType reason -> "Type error:" <+> pretty reason
    BadKind tVar tVar' -> "Kind mismatch:" <+> pretty tVar <+>"and" <+> pretty tVar'

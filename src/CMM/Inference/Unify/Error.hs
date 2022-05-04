{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Unify.Error where

import safe Data.Eq (Eq)
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)

-- | The errors that can happen during unification
data UnificationError
  = Occurs TypeVar Type -- ^ Occurs check failure (the given type variable occurs in the given type)
  | Mismatch Text Type Type -- ^ The two given types cannot be unified
  | GotErrorType Text -- ^ Encountered an error type during unification
  | BadKind Type Type -- ^ The type kinds of the given types do not match
  deriving (Show, Eq, IsError)

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Unify.Error where

import safe Data.Eq (Eq)
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)

data UnificationError
  = Occurs TypeVar Type
  | Mismatch Text Type Type
  | NoSubType Type Type -- supertype; subtype
  | NoConstness Constness Type
  | NoKind Text Type
  | NoRegister Text Type
  | TupleMismatch [Type] [Type]
  | GotErrorType Text
  | IllegalPolytype Type
  | BadKind Type Type
  | FalseKind
  | FalseConst
  deriving (Show, Eq, IsError)

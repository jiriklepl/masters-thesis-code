{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Variables.Error where

import safe Data.Eq (Eq)
import safe Data.Function ((.))
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.HasName (HasName(getName))
import safe CMM.Err.IsError (IsError)

data VariablesError
  = DuplicateVariable Text
  | DuplicateTypeVariable Text
  | DuplicateTypeConstant Text
  | DuplicateFunctionVariable Text
  deriving (Show, Eq, IsError)

duplicateVariable :: HasName n => n -> VariablesError
duplicateVariable = DuplicateVariable . getName

duplicateTypeVariable :: HasName n => n -> VariablesError
duplicateTypeVariable = DuplicateTypeVariable . getName

duplicateTypeConstant :: HasName n => n -> VariablesError
duplicateTypeConstant = DuplicateTypeConstant . getName

duplicateFunctionVariable :: HasName n => n -> VariablesError
duplicateFunctionVariable = DuplicateFunctionVariable . getName

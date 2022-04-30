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
  = DuplicateFunctionVariable Text
  | DuplicateTypeAlias Text
  | DuplicateTypeConstant Text
  | DuplicateTypeVariable Text
  | DuplicateVariable Text
  deriving (Show, Eq, IsError)

duplicateFunctionVariable :: HasName n => n -> VariablesError
duplicateFunctionVariable = DuplicateFunctionVariable . getName

duplicateTypeAlias :: HasName n => n -> VariablesError
duplicateTypeAlias = DuplicateTypeAlias . getName

duplicateTypeConstant :: HasName n => n -> VariablesError
duplicateTypeConstant = DuplicateTypeConstant . getName

duplicateTypeVariable :: HasName n => n -> VariablesError
duplicateTypeVariable = DuplicateTypeVariable . getName

duplicateVariable :: HasName n => n -> VariablesError
duplicateVariable = DuplicateVariable . getName

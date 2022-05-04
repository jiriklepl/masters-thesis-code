{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Variables.Error where

import safe Data.Eq (Eq)
import safe Data.Function ((.))
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Err.IsError (IsError)

-- | The errors that can happen during variable collection
data VariablesError
  = DuplicateFunctionVariable Text -- ^ The given function is already declared
  | DuplicateTypeAlias Text -- ^ The given type alias is already declared
  | DuplicateTypeConstant Text -- ^ The given type constant name is already declared
  | DuplicateTypeVariable Text -- ^ The given type variable is already declared
  | DuplicateVariable Text -- ^ The given variable is already declared
  deriving (Show, Eq, IsError)

-- | Helper function for using `DuplicateFunctionVariable` on variables that are in `GetName` type class
duplicateFunctionVariable :: GetName n => n -> VariablesError
duplicateFunctionVariable = DuplicateFunctionVariable . getName

-- | Helper function for using `DuplicateTypeAlias` on variables that are in `GetName` type class
duplicateTypeAlias :: GetName n => n -> VariablesError
duplicateTypeAlias = DuplicateTypeAlias . getName

-- | Helper function for using `DuplicateTypeConstant` on variables that are in `GetName` type class
duplicateTypeConstant :: GetName n => n -> VariablesError
duplicateTypeConstant = DuplicateTypeConstant . getName

-- | Helper function for using `DuplicateTypeVariable` on variables that are in `GetName` type class
duplicateTypeVariable :: GetName n => n -> VariablesError
duplicateTypeVariable = DuplicateTypeVariable . getName

-- | Helper function for using `DuplicateVariable` on variables that are in `GetName` type class
duplicateVariable :: GetName n => n -> VariablesError
duplicateVariable = DuplicateVariable . getName

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.AST.Variables.Error where

import safe Data.Text (Text)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty), (<+>) )

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Err.IsError (IsError)

-- | The errors that can happen during variable collection
data VariablesError
  = DuplicateFunctionVariable Text -- ^ The given function is already declared
  | DuplicateTypeAlias Text -- ^ The given type alias is already declared
  | DuplicateTypeConstant Text -- ^ The given type constant name is already declared
  | DuplicateTypeVariable Text -- ^ The given type variable is already declared
  | DuplicateVariable Text -- ^ The given variable is already declared
  deriving (Show, Eq, IsError, Data)

instance Pretty VariablesError where
  pretty = \case
    DuplicateFunctionVariable name -> "Duplicate function: " <+> pretty name
    DuplicateTypeAlias name -> "Duplicate type alias: " <+> pretty name
    DuplicateTypeConstant name -> "Duplicate class: " <+> pretty name
    DuplicateTypeVariable name -> "Duplicate type variable: " <+> pretty name
    DuplicateVariable name -> "Duplicate variable: " <+> pretty name

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

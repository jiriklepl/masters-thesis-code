{-# LANGUAGE Safe #-}

module CMM.AST.Variables.SymbolType where

import safe Data.Eq (Eq)
import safe Text.Show (Show)

data SymbolType
  = ConstDeclSymbol
  | ContSymbol
  | DatumLabelSymbol
  | ImportSymbol
  | LabelSymbol
  | RegisterSymbol
  deriving (Eq, Show)

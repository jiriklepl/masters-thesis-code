{-# LANGUAGE Safe #-}

module CMM.AST.Variables.SymbolType where

import safe Data.Eq (Eq)
import safe Text.Show (Show)

-- Symbol types used by `DuplicateSymbol` error in the `Blockifier`
data SymbolType
  = ConstDeclSymbol
  | ContSymbol
  | DatumLabelSymbol
  | ImportSymbol
  | LabelSymbol
  | RegisterSymbol
  deriving (Eq, Show)

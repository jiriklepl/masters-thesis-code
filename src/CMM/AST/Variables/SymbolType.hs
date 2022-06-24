{-# LANGUAGE Safe #-}

module CMM.AST.Variables.SymbolType where

import safe Data.Eq (Eq)
import safe Text.Show (Show)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty) )

-- Symbol types used by `DuplicateSymbol` error in the `Blockifier`
data SymbolType
  = ConstDeclSymbol
  | ContSymbol
  | DatumLabelSymbol
  | ImportSymbol
  | LabelSymbol
  | RegisterSymbol
  deriving (Eq, Show, Data)

instance Pretty SymbolType where
  pretty = \case
    ConstDeclSymbol -> "constant declaration"
    ContSymbol -> "continuation"
    DatumLabelSymbol -> "datum label"
    ImportSymbol -> "import"
    LabelSymbol -> "label"
    RegisterSymbol -> "register"

{-# LANGUAGE Safe #-}

module Language.AST.BlockAnnot where

import safe Data.Text (Text)
import safe Data.Map (Map)

data BlockAnnot
  = PartOf Int
  | Begins Int
  | Unreachable
  | NoBlock
  deriving (Show)

type BlockData = Map Int BlockVars
type BlockVars = Map Text (Bool, Bool, Bool)

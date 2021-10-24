{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.AST.BlockAnnot where

import safe Data.Data (Data)
import safe Data.Map (Map)
import safe Data.Text (Text)

data BlockAnnot
  = PartOf Int
  | Begins Int
  | Unreachable
  | NoBlock
  deriving (Eq, Show, Data)

type BlockData = Map Int BlockVars

type BlockVars = Map Text (Bool, Bool, Bool)

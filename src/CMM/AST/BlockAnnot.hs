{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.AST.BlockAnnot where

import safe Data.Data (Data)
import safe Data.Text (Text)
import safe Data.Map (Map)

data BlockAnnot
  = PartOf Int
  | Begins Int
  | Unreachable
  | NoBlock
  deriving (Eq, Show, Data)

type BlockData = Map Int BlockVars
type BlockVars = Map Text (Bool, Bool, Bool)

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.AST.BlockAnnot where

import safe Data.Data (Data)
import safe Data.Map (Map)
import safe Data.Text (Text)

-- | Contains information about basic-block structure
data BlockAnnot
  = PartOf Int -- | Belongs to a basic block with the specified index
  | Begins Int -- | Begins a basic block with the specified index
  | Unreachable -- | This node is (syntactically) unreachable
  | NoBlock -- | No block information about node annotated by this
  deriving (Eq, Show, Data)

type BlockData = Map Int BlockVars

type BlockVars = Map Text (Bool, Bool, Bool)

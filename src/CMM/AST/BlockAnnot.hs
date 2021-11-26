{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CMM.AST.BlockAnnot where

import safe Data.Data (Data)
import safe Data.Map (Map)
import safe Data.Text (Text)

import safe CMM.AST.Annot

-- | Contains information about basic-block structure
data BlockAnnot
  = PartOf Int -- | Belongs to a basic block with the specified index
  | Begins Int -- | Begins a basic block with the specified index
  | Unreachable -- | This node is (syntactically) unreachable
  | NoBlock -- | No block information about node annotated by this
  deriving (Eq, Show, Data)

class HasBlockAnnot a where
  getBlockAnnot :: a -> BlockAnnot

instance HasBlockAnnot a => HasBlockAnnot (Annot n a) where
  getBlockAnnot = getBlockAnnot . takeAnnot

class HasBlockAnnot b =>
      WithBlockAnnot a b
  | a -> b
  , b -> a
  where
  withBlockAnnot :: BlockAnnot -> a -> b

type BlockData = Map Int BlockVars

type BlockVars = Map Text (Bool, Bool, Bool)

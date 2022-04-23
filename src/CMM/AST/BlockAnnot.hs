{-# LANGUAGE Safe #-}

module CMM.AST.BlockAnnot where

import safe Data.Bool (Bool)
import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Function ((.), flip, id)
import safe Data.Int (Int)
import safe Data.Map (Map)
import safe Data.Text (Text)
import safe Text.Show (Show)

import safe CMM.AST.Annot (Annot, takeAnnot)
import safe Data.Tuple ( snd )

-- | Contains information about basic-block structure
data BlockAnnot
  = PartOf Int -- | Belongs to a basic block with the specified index
  | Begins Int -- | Begins a basic block with the specified index
  | Unreachable -- | This node is (syntactically) unreachable
  | NoBlock -- | No block information about node annotated by this
  deriving (Eq, Show, Data)

class HasBlockAnnot a where
  getBlockAnnot :: a -> BlockAnnot

instance HasBlockAnnot BlockAnnot where
  getBlockAnnot = id

instance HasBlockAnnot (a, BlockAnnot) where
  getBlockAnnot = snd

instance HasBlockAnnot a => HasBlockAnnot (Annot n a) where
  getBlockAnnot = getBlockAnnot . takeAnnot

class HasBlockAnnot b =>
      WithBlockAnnot a b
  | a -> b
  , b -> a
  where
  withBlockAnnot :: BlockAnnot -> a -> b

instance WithBlockAnnot a (a, BlockAnnot) where
  withBlockAnnot = flip (,)

type BlockData = Map Int BlockVars

type BlockVars = Map Text (Bool, Bool, Bool)

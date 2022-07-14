{-# LANGUAGE Safe #-}

module CMM.AST.BlockAnnot where

import safe Data.Data (Data)
import safe Data.Map (Map)
import safe Data.Text (Text)
import safe Data.Function ( (&) )
import safe Data.Bifunctor ( Bifunctor(second, first) )

import safe CMM.AST.Annot (Annot, takeAnnot, Annotation (Annot))

-- | Contains information about basic-block structure
data BlockAnnot
  = PartOf Int -- ^ Belongs to a basic block with the specified index
  | Begins Int -- ^ Begins a basic block with the specified index
  | Unreachable -- ^ This node is (syntactically) unreachable
  | NoBlock -- ^ No block information about node annotated by this
  deriving (Eq, Show, Data)

-- | A class of types that contain a `BlockAnnot`
class HasBlockAnnot a where
  getBlockAnnot :: a -> BlockAnnot -- ^ retrieves a `BlockAnnot`
  setBlockAnnot :: BlockAnnot -> a -> a

getBlockMembership :: HasBlockAnnot a => a -> Maybe Int
getBlockMembership a = getBlockAnnot a & \case
   PartOf idx -> Just idx
   Begins idx -> Just idx
   _ -> Nothing

instance HasBlockAnnot BlockAnnot where
  getBlockAnnot = id
  setBlockAnnot = const

instance HasBlockAnnot (a, BlockAnnot) where
  getBlockAnnot = snd
  setBlockAnnot = second . const

instance HasBlockAnnot ((a, BlockAnnot), b) where
  getBlockAnnot = snd . fst
  setBlockAnnot = first . second . const

instance (HasBlockAnnot a, Functor n) => HasBlockAnnot (Annot n a) where
  getBlockAnnot = getBlockAnnot . takeAnnot
  setBlockAnnot b = \case
    n `Annot` a -> (setBlockAnnot b <$> n) `Annot` setBlockAnnot b a

-- | Adds `BlockAnnot` to an annotation
class HasBlockAnnot b =>
      WithBlockAnnot a b
  | a -> b
  , b -> a
  where
  withBlockAnnot :: BlockAnnot -> a -> b

instance WithBlockAnnot a (a, BlockAnnot) where
  withBlockAnnot = flip (,)

-- | Maps a block to a `BlockVars` map, which then contains the R.W.L. tuple about each variable
type BlockData = Map Int BlockVars

-- | Maps a variable name to a tuple (Reads before any write, Writes, Requests alive)
type BlockVars = Map Text (Bool, Bool, Bool)

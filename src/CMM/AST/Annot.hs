{-# LANGUAGE Safe #-}

module CMM.AST.Annot where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable)
import safe Data.Function ((.), flip)
import safe Data.Functor (Functor(fmap), void)
import safe Data.Ord (Ord)
import safe Data.Traversable (Traversable)
import safe Text.Show (Show)

import safe Prettyprinter (Pretty(pretty))

import safe CMM.Inference.Preprocess.TypeHole (HasTypeHole(getTypeHole))
import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Parser.HasPos (HasPos(getPos))

-- TODO :
-- | Returns the unannotated version of the given annotated node
-- unAnnot :: Annot n a -> n a
-- -- | Returns the annotation of the given annotated node
-- takeAnnot :: Annot n a -> a

-- | Annotation used as a wrapper for nodes, can contain 'SourcePos', for example
data Annotation node annot =
  Annot { unAnnot :: node annot, takeAnnot :: annot }
  deriving (Show, Foldable, Traversable, Functor, Data)


deriving instance (Eq (n a), Eq a) => Eq (Annotation n a)

deriving instance (Ord (n a), Ord a) => Ord (Annotation n a)

instance HasTypeHole a => HasTypeHole (Annot n a) where
  getTypeHole = getTypeHole . takeAnnot

instance HasTypeHole a => ToType (Annot n a) where
  toType = toType . getTypeHole

instance HasPos a => HasPos (Annot n a) where
  getPos = getPos . takeAnnot

instance ToTypeVar a => ToTypeVar (Annot n a) where
  toTypeVar = toTypeVar . takeAnnot

instance (Pretty (n a)) => Pretty (Annot n a) where
  pretty =
    \case
      Annot n _ -> pretty n

-- | see 'Annotation'
type Annot = Annotation

-- | Annotates a node with the given annotation
withAnnot :: a -> n a -> Annot n a
withAnnot = flip Annot

toTuple :: Annot n a -> (a, n a)
toTuple = \case
  Annot {unAnnot, takeAnnot} -> (takeAnnot, unAnnot)

fromTuple :: (a, n a) -> Annot n a
fromTuple (takeAnnot, unAnnot) = Annot {unAnnot, takeAnnot}

mapAnnot :: (n a -> m a) -> Annot n a -> Annot m a
mapAnnot f = \case
  n `Annot` a -> f n `Annot` a

-- | Applies an update function to all annotations inside the given node
updateAnnots :: Functor n => (a -> b) -> n a -> n b
updateAnnots = fmap

-- | Replaces all annotations inside the given node with units
stripAnnots :: Functor n => n a -> n ()
stripAnnots = void

-- | Takes an the annotation of one node and applies it to another
copyAnnot :: Annot n a -> m a -> Annot m a
copyAnnot = withAnnot . takeAnnot

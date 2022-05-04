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

-- | Annotation used as a wrapper for nodes, can contain `SourcePos`, for example
data Annotation node annot =
  Annot (node annot) annot
  deriving (Show, Foldable, Traversable, Functor, Data)

deriving instance (Eq (n a), Eq a) => Eq (Annotation n a)

deriving instance (Ord (n a), Ord a) => Ord (Annotation n a)

-- | see `Annotation`
type Annot = Annotation

-- | Annotates a node with the given annotation
withAnnot :: a -> n a -> Annot n a
withAnnot = flip Annot

-- | Returns the annotation of the given annotated node
takeAnnot :: Annot n a -> a
takeAnnot (Annot _ annot) = annot

-- | Returns the unannotated version of the given annotated node
unAnnot :: Annot n a -> n a
unAnnot (Annot node _) = node

-- | Applies an update function to all annotations inside the given node
updateAnnots :: Functor n => (a -> b) -> n a -> n b
updateAnnots = fmap

-- | Replaces all annotations inside the given node with units
stripAnnots :: Functor n => n a -> n ()
stripAnnots = void

-- | Takes an the annotation of one node and applies it to another
copyAnnot :: Annot n a -> m a -> Annot m a
copyAnnot = withAnnot . takeAnnot

{-# LANGUAGE Safe #-}

module CMM.AST.Annot where

import safe Data.Data (Data)
import safe Data.Functor (void)

import safe Prettyprinter (Pretty(pretty))

import safe CMM.Inference.Preprocess.Elaboration (HasElaboration(getElaboration, setElaboration))
import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Parser.GetPos (GetPos(getPos))

-- | Annotation used to append extra information to nodes in AST
data Annotation node annot =
  Annot { unAnnot :: node annot, takeAnnot :: annot }
  deriving (Show, Foldable, Traversable, Functor, Data)


deriving instance (Eq (n a), Eq a) => Eq (Annotation n a)

deriving instance (Ord (n a), Ord a) => Ord (Annotation n a)

instance HasElaboration a => HasElaboration (Annot n a) where
  getElaboration = getElaboration . takeAnnot
  setElaboration s (n `Annot` a) =  n `Annot` setElaboration s a

instance HasElaboration a => ToType (Annot n a) where
  toType = toType . getElaboration

instance GetPos a => GetPos (Annot n a) where
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

-- | transforms an `Annot` object to an equivalent tuple
toTuple :: Annot n a -> (a, n a)
toTuple = \case
  Annot {unAnnot, takeAnnot} -> (takeAnnot, unAnnot)

-- | creates an `Annot` object from an equivalent tuple
fromTuple :: (a, n a) -> Annot n a
fromTuple (takeAnnot, unAnnot) = Annot {unAnnot, takeAnnot}

mapNode :: (n a -> m a) -> Annot n a -> Annot m a
mapNode f = \case
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

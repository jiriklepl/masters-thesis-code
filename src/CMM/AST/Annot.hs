{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.AST.Annot where

import safe Data.Data (Data)
import safe Data.Functor (void)

data Annotation node annot =
  Annot (node annot) annot
  deriving (Show, Functor, Data)

deriving instance (Eq (n a), Eq a) => Eq (Annotation n a)

deriving instance (Ord (n a), Ord a) => Ord (Annotation n a)

type Annot = Annotation

withAnnot :: a -> n a -> Annot n a
withAnnot = flip Annot

takeAnnot :: Annot n a -> a
takeAnnot (Annot _ annot) = annot

unAnnot :: Annot n a -> n a
unAnnot (Annot node _) = node

updateAnnots :: Functor n => (a -> b) -> n a -> n b
updateAnnots = fmap

stripAnnots :: Functor n => n a -> n ()
stripAnnots = void

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.WithElaboration where

import safe CMM.Inference.Preprocess.Elaboration
  ( Elaboration(EmptyElaboration)
  , HasElaboration
  )

-- | A class that appends a type hole to the given AST node annotation
class HasElaboration b =>
      WithElaboration a b
  | a -> b
  , b -> a
  where
  withElaboration :: Elaboration -> a -> b -- ^ Appends a type hole to the given AST node annotation

instance WithElaboration a (a, Elaboration) where
  withElaboration = flip (,)

-- | Appends empty type hole to the given AST node annotation
withEmptyElaboration :: WithElaboration a b => a -> b
withEmptyElaboration = withElaboration EmptyElaboration

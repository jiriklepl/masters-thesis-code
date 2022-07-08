{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.WithTypeHole where

import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole
  , TypeHole(EmptyTypeHole)
  )

-- | A class that appends a type hole to the given AST node annotation
class HasTypeHole b =>
      WithTypeHole a b
  | a -> b
  , b -> a
  where
  withTypeHole :: TypeHole -> a -> b -- ^ Appends a type hole to the given AST node annotation

instance WithTypeHole a (a, TypeHole) where
  withTypeHole = flip (,)

-- | Appends empty type hole to the given AST node annotation
withEmptyTypeHole :: WithTypeHole a b => a -> b
withEmptyTypeHole = withTypeHole EmptyTypeHole

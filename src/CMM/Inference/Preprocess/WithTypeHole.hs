{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.WithTypeHole where

import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole
  , TypeHole(EmptyTypeHole)
  )

class HasTypeHole b =>
      WithTypeHole a b
  | a -> b
  , b -> a
  where
  withTypeHole :: TypeHole -> a -> b

instance WithTypeHole a (a, TypeHole) where
  withTypeHole = flip (,)

withEmptyTypeHole :: WithTypeHole a b => a -> b
withEmptyTypeHole = withTypeHole EmptyTypeHole

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.WithTypeHole where

import safe Data.Function (flip)

import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole
  , TypeHole(EmptyTypeHole)
  )
import safe CMM.Parser.HasPos (SourcePos)

class HasTypeHole b =>
      WithTypeHole a b
  | a -> b
  , b -> a
  where
  withTypeHole :: TypeHole -> a -> b

instance WithTypeHole SourcePos (SourcePos, TypeHole) where
  withTypeHole = flip (,)

withEmptyTypeHole :: WithTypeHole a b => a -> b
withEmptyTypeHole = withTypeHole EmptyTypeHole

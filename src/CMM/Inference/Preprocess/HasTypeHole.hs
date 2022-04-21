{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.HasTypeHole where

import safe Control.Lens.Getter (view)
import safe Control.Lens.Tuple (_2)
import safe Data.Function ((.), flip, id)

import safe CMM.AST.Annot (Annot, takeAnnot)
import safe CMM.Inference.Preprocess.TypeHole (TypeHole, holeId)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Parser.HasPos (SourcePos)

class HasTypeHole a where
  getTypeHole :: a -> TypeHole

getTypeHoleId :: HasTypeHole a => a -> TypeVar
getTypeHoleId = holeId . getTypeHole

instance HasTypeHole TypeHole where
  getTypeHole = id

instance HasTypeHole (a, TypeHole) where
  getTypeHole = view _2

instance HasTypeHole (a, TypeHole, b) where
  getTypeHole = view _2

instance HasTypeHole a => HasTypeHole (Annot n a) where
  getTypeHole = getTypeHole . takeAnnot

class HasTypeHole b =>
      WithTypeHole a b
  | a -> b
  , b -> a
  where
  withTypeHole :: TypeHole -> a -> b

instance WithTypeHole SourcePos (SourcePos, TypeHole) where
  withTypeHole = flip (,)

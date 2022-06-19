{-# LANGUAGE Safe #-}

module CMM.Monomorphize.Schematized where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable)
import safe Data.Function (($))
import safe Data.Functor (Functor)
import safe Data.Traversable (Traversable)
import safe Text.Show (Show)

import safe CMM.AST (Procedure, Struct, TopLevel(TopProcedure, TopStruct))
import safe CMM.AST.Annot (Annot, copyAnnot)

data Schematized a
  = FuncScheme (Annot Procedure a)
  | StructScheme (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data)

schematized2topLevel :: Schematized a -> Annot TopLevel a
schematized2topLevel = \case
  FuncScheme procedure ->
    copyAnnot procedure $ TopProcedure procedure
  StructScheme struct ->
    copyAnnot struct $ TopStruct struct

deriving instance Eq (Schematized ())

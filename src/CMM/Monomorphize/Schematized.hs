{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Monomorphize.Schematized where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Foldable (Foldable)
import safe Data.Function (($))
import safe Data.Functor (Functor)
import safe Data.Traversable (Traversable)
import safe Text.Show (Show)

import safe CMM.AST (AST, Procedure, Struct, TopLevel(TopProcedure, TopStruct))
import safe CMM.AST.Annot (Annot, copyAnnot)

data Schematized a
  = FuncScheme (Annot Procedure a)
  | StructScheme (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data, AST)

schematized2topLevel :: Schematized a -> Annot TopLevel a
schematized2topLevel (FuncScheme procedure) =
  copyAnnot procedure $ TopProcedure procedure
schematized2topLevel (StructScheme struct) = copyAnnot struct $ TopStruct struct

deriving instance Eq (Schematized ())

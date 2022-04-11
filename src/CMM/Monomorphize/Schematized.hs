{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.Monomorphize.Schematized where

import safe CMM.AST ( Procedure, Struct, AST, TopLevel (TopProcedure, TopStruct) )
import safe CMM.AST.Annot (Annot, copyAnnot)
import safe Data.Data ( Data )

data Schematized a
  = FuncScheme (Annot Procedure a)
  | StructScheme (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data, AST)

schematized2topLevel :: Schematized a -> Annot TopLevel a
schematized2topLevel (FuncScheme procedure) = copyAnnot procedure $ TopProcedure procedure
schematized2topLevel (StructScheme struct) = copyAnnot struct $ TopStruct struct

deriving instance Eq (Schematized ())

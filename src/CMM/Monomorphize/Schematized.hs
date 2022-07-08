{-# LANGUAGE Safe #-}

module CMM.Monomorphize.Schematized where

import safe Data.Data (Data)

import safe CMM.AST (Procedure, Struct, TopLevel(TopProcedure, TopStruct))
import safe CMM.AST.Annot (Annot, copyAnnot)

-- | A wrapper for polytype objects that have schemes (currently just procedures and structs)
data Schematized a
  = ProcedureScheme (Annot Procedure a)
  | StructScheme (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Schematized ())

-- | Transforms a `Schematized` object into a `TopLevel` object
schematized2topLevel :: Schematized a -> Annot TopLevel a
schematized2topLevel =
  \case
    ProcedureScheme procedure -> copyAnnot procedure $ TopProcedure procedure
    StructScheme struct -> copyAnnot struct $ TopStruct struct

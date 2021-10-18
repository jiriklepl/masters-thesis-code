{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Utils (HasPos(..)) where

import safe Control.Lens.Tuple
import safe Control.Lens.Getter

import safe Language.AST ( Annot )
import safe Language.AST.Utils ( takeAnnot )
import safe Text.Megaparsec.Pos ( SourcePos )


class HasPos n where
  getPos :: n -> SourcePos

instance {-# OVERLAPPING #-} HasPos (Annot n SourcePos) where
  getPos = takeAnnot

instance Field1 s s SourcePos SourcePos =>  HasPos (Annot n s) where
  getPos = (^._1) . takeAnnot

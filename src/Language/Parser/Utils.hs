{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Parser.Utils (HasPos(..)) where

import safe Language.AST ( Annot )
import safe Language.AST.Utils ( takeAnnot )
import safe Text.Megaparsec.Pos ( SourcePos )

class HasPos n where
  getPos :: n -> SourcePos

instance HasPos SourcePos where
  getPos = id

instance HasPos a => HasPos (Annot n a) where
  getPos = getPos . takeAnnot

instance HasPos (SourcePos, b) where
  getPos (pos, _) = pos

instance HasPos (SourcePos, b, c) where
  getPos (pos, _, _) = pos

instance HasPos (SourcePos, b, c, d) where
  getPos (pos, _, _, _) = pos

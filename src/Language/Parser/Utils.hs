{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Utils (HasPos(..)) where

import safe Language.AST ( Annot )
import safe Language.AST.Utils ( takeAnnot )
import safe Text.Megaparsec.Pos ( SourcePos )


class HasPos n where
  getPos :: n -> SourcePos

instance HasPos (Annot n SourcePos) where
  getPos = takeAnnot

instance HasPos (Annot n (SourcePos, a)) where
  getPos = fst . takeAnnot

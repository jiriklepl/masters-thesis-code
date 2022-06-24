{-# LANGUAGE Safe #-}

-- TODO: rename to GetPos?
module CMM.Parser.HasPos
  ( HasPos(..)
  , SourcePos
  , sourcePosPretty
  ) where

import safe Data.Function (id)
import safe Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

class HasPos a where
  getPos :: a -> SourcePos
  -- ^ Retrieves a `SourcePos` annotation from the given variable

instance HasPos SourcePos where
  getPos = id

instance HasPos (SourcePos, b) where
  getPos (pos, _) = pos

instance HasPos (SourcePos, b, c) where
  getPos (pos, _, _) = pos

instance HasPos (SourcePos, b, c, d) where
  getPos (pos, _, _, _) = pos

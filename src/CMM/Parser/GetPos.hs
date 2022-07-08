{-# LANGUAGE Safe #-}

module CMM.Parser.GetPos
  ( GetPos(getPos)
  , SourcePos
  , sourcePosPretty
  ) where

import safe Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

--  | A class for objects we can get `SourcePos` from
class GetPos a where
  getPos :: a -> SourcePos
  -- ^ Retrieves a `SourcePos` annotation from the given variable

instance GetPos SourcePos where
  getPos = id

instance GetPos (SourcePos, b) where
  getPos (pos, _) = pos

instance GetPos ((SourcePos, b), c) where
  getPos ((pos, _), _) = pos

{-# LANGUAGE Safe #-}

module CMM.Inference.Utils where

import safe Data.Bool (otherwise)
import safe Data.Data (Data(gmapT))
import safe qualified Data.Set as Set
import safe Data.Set (Set)
import safe Data.String (String)
import safe Data.Text (Text)

import safe CMM.Data.Generics ((*|*))
import safe CMM.Data.Trilean (Trilean, trilean)
import safe CMM.Inference.TypeVar (TypeVar(tVarParent))
import safe CMM.Utils (addPrefix)

adopt :: Data d => TypeVar -> Set TypeVar -> d -> d
adopt parent children = go
  where
    go :: Data d => d -> d
    go = tVarCase *|* gmapT go
    tVarCase tVar
      | tVar `Set.member` children = tVar {tVarParent = parent}
      | otherwise = tVar

fieldClassHelper :: Text -> Text
fieldClassHelper = addPrefix fieldClassPrefix

fieldClassPrefix :: Text
fieldClassPrefix = "HasField"

funDepsClassPrefix :: Text
funDepsClassPrefix = "FunDeps"

trileanSeq :: [Trilean] -> String
trileanSeq =
  \case
    t:others -> trilean 'F' 'U' 'T' t : trileanSeq others
    [] -> []

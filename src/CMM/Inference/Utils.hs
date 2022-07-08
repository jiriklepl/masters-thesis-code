{-# LANGUAGE Safe #-}

module CMM.Inference.Utils where

import safe Data.Data (Data(gmapT))
import safe qualified Data.Set as Set
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Data.Generics ((*|*))
import safe CMM.Data.Trilean (Trilean, trilean)
import safe CMM.Inference.TypeVar (TypeVar(tVarParent))
import safe CMM.Utils (addPrefix)

-- | recursively changes the parents of the given children to the given parent
adopt :: Data d => TypeVar -> Set TypeVar -> d -> d
adopt parent children = go
  where
    go :: Data d => d -> d
    go = tVarCase *|* gmapT go
    tVarCase tVar
      | tVar `Set.member` children = tVar {tVarParent = parent}
      | otherwise = tVar

-- | adds prefix to the given name that represents it is a class that has
--   methods representing a field accessor
fieldClassHelper :: Text -> Text
fieldClassHelper = addPrefix fieldClassPrefix

-- | the prefix given to the classes with methods that represent field accessors
fieldClassPrefix :: Text
fieldClassPrefix = "HasField"

-- | the prefix given to the constraints generated according to the functional dependencies of certain classes
funDepsClassPrefix :: Text
funDepsClassPrefix = "FunDeps"

-- | turns a functional dependency to a text representation
trileanSeq :: [Trilean] -> String
trileanSeq =
  \case
    t:others -> trilean 'F' 'U' 'T' t : trileanSeq others
    [] -> []

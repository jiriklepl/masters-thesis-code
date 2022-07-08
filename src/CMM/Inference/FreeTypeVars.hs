{-# LANGUAGE Safe #-}

module CMM.Inference.FreeTypeVars where

import safe Data.Data (Data(gmapQ))
import safe Data.Generics.Aliases (extQ)
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe CMM.Inference.Fact
  ( Fact
  , FlatFact(InstType)
  , NestedFact(Fact, NestedFact)
  , Qual((:=>))
  , Scheme((:.))
  )
import safe CMM.Inference.TypeVar (TypeVar(TypeVar))

-- | Gets free type variables from the given object
freeTypeVars :: Data a => a -> Set TypeVar
freeTypeVars = go
  where
    go :: Data d => d -> Set TypeVar
    go = (Set.unions . gmapQ go) `extQ` factCase `extQ` tVarCase
    factCase =
      \case
        -- does not return the free type variables of the object to be instantiated
        Fact (InstType _ t') -> freeTypeVars t'
        -- does not propagate the quantified tVars
        NestedFact (tVars :. flatFacts :=> nestedFacts) ->
          (freeTypeVars flatFacts <> freeTypeVars nestedFacts) `Set.difference`
          tVars
        (fact :: Fact) -> Set.unions $ gmapQ go fact
    tVarCase tVar@TypeVar {} = Set.singleton tVar
    tVarCase _ = mempty

{-# LANGUAGE Safe #-}

module CMM.Inference.FreeTypeVars where

import safe Data.Data (Data(gmapQ))
import safe Data.Function (($), (.))
import safe Data.Generics.Aliases (extQ)
import safe Data.Monoid (Monoid(mempty), (<>))
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

freeTypeVars :: Data a => a -> Set TypeVar
freeTypeVars = go
  where
    go :: Data d => d -> Set TypeVar
    go = (Set.unions . gmapQ go) `extQ` factCase `extQ` leaf
    factCase =
      \case
        Fact (InstType _ t') -> freeTypeVars t'
        NestedFact (tVars :. flatFacts :=> nestedFacts) ->
          (freeTypeVars flatFacts <> freeTypeVars nestedFacts) `Set.difference`
          tVars
        (fact :: Fact) -> Set.unions $ gmapQ go fact
    leaf tVar@TypeVar {} = Set.singleton tVar
    leaf _ = mempty

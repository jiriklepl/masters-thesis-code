{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Inference.Refresh where

import safe Control.Monad (Monad(return))
import safe Data.Functor ((<$>))
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe CMM.Inference.Fact
  ( NestedFact(NestedFact)
  , Qual((:=>))
  , Scheme((:.))
  )
import safe CMM.Inference.Subst (Apply(apply), Subst)
import safe CMM.Inference.TypeVar (TypeVar)

class Monad s =>
      Refresher s
  where
  refresher :: Set TypeVar -> s (Subst TypeVar)

refreshScheme :: (Refresher s, Apply n TypeVar) => Scheme n -> s (Scheme n)
refreshScheme =
  \case
    tVars :. facts :=> nested -> do
      subst <- refresher tVars
      return
        (Set.fromAscList (Map.elems subst) :. (apply subst <$> facts) :=>
         apply subst nested)

refreshNestedFact ::
     (Refresher s, Apply n TypeVar) => NestedFact n -> s (NestedFact n)
refreshNestedFact =
  \case
    NestedFact scheme -> NestedFact <$> refreshScheme scheme
    fact -> return fact

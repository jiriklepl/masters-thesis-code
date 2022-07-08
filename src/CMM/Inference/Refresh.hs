{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Inference.Refresh where

import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Functor ((<&>))

import safe CMM.Inference.Fact
  ( NestedFact(NestedFact)
  , Qual((:=>))
  , Scheme((:.))
  )
import safe CMM.Inference.Subst (Apply(apply), Subst)
import safe CMM.Inference.TypeVar (TypeVar)

-- | Class for all monadic states that can refresh type variables,
--   providing a substitution from old names to fresh names
class Monad s =>
      Refresh s
  where
  refresh :: Set TypeVar -> s (Subst TypeVar)

-- | refreshes the given scheme, instantiation can be then
--   done by trivially throwing away the quantified type variables
refreshScheme :: (Refresh s, Apply n TypeVar) => Scheme n -> s (Scheme n)
refreshScheme =
  \case
    tVars :. facts :=> nested -> do
      refresh tVars <&> \subst ->
        Set.fromAscList (Map.elems subst) :. (apply subst <$> facts) :=>
         apply subst nested

refreshNestedFact ::
     (Refresh s, Apply n TypeVar) => NestedFact n -> s (NestedFact n)
refreshNestedFact =
  \case
    NestedFact scheme -> NestedFact <$> refreshScheme scheme
    fact -> return fact

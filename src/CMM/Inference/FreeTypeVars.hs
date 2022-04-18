{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.FreeTypeVars where

import safe Prelude

import safe Data.Data (Data(gmapQ))
import safe Data.Generics.Aliases (extQ)
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe CMM.Inference.Fact
  ( Fact
  , FlatFact(InstType)
  , NestedFact(..)
  , Qual(..)
  , Scheme(..)
  )
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeCompl (TypeCompl)
import safe CMM.Inference.TypeVar (TypeVar(TypeVar))

class Data a =>
      FreeTypeVars a
  where
  freeTypeVars :: a -> Set TypeVar
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

deriving instance FreeTypeVars a => FreeTypeVars [a]

deriving instance FreeTypeVars a => FreeTypeVars (NestedFact a)

deriving instance FreeTypeVars a => FreeTypeVars (FlatFact a)

deriving instance Data a => FreeTypeVars (Scheme a)

deriving instance Data a => FreeTypeVars (Qual a)

deriving instance Data a => FreeTypeVars (TypeCompl a)

deriving instance FreeTypeVars Type

deriving instance FreeTypeVars TypeVar

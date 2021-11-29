{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module CMM.Inference where

import safe Control.Monad.State
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe CMM.Inference.Type

type Subst = Map TypeVar Type

class Apply a where
  apply :: Subst -> a -> a

class Unify a where
  unify :: a -> a -> Subst
  unify a b = unifyImpl a b a
  unifyImpl :: a -> a -> (a -> Subst)
  unifyImpl a = const . unify a

instance Unify SimpleType where
  unify (VarType tVar) t'
    | tVar `Set.member` freeTypeVars t' = Map.singleton tVar (SimpleType t')
    | otherwise =
      Map.singleton tVar $
      ErrorType . T.pack $
      "Type variable " <> show tVar <> " occurs in the type " <> show t' -- TODO: pretty
-- factChecker :: MonadState Subst m => m [Facts]

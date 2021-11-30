{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Inference where

import safe Control.Monad.Writer.Lazy
import safe Data.Data
import safe Data.Functor
import safe Data.Generics.Aliases
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe CMM.Inference.Type

type Subst = Map TypeVar Type

class Data a =>
      Apply a
  where
  apply :: Subst -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCase
      typeCase =
        \case
          t@(SimpleType (VarType tVar)) -> fromMaybe t (tVar `Map.lookup` subst)
          t -> gmapT go t

instance Apply Type

instance Apply SimpleType

instance Apply Fact

instance Apply Subst where
  subst' `apply` subst = (apply subst' <$> subst) <> subst'

data InferenceError
  = Occurs TypeVar Type
  | Mismatch Type Type
  | TupleMismatch [Type] [Type]

class Unify a where
  unify ::
       MonadWriter ([InferenceError] -> [InferenceError]) m
    => a
    -> a
    -> m (Subst, a)
  unify a b = unifyImpl a b <*> return a
  unifyImpl ::
       MonadWriter ([InferenceError] -> [InferenceError]) m
    => a
    -> a
    -> m (a -> (Subst, a))
  unifyImpl a = (const <$>) . unify a

instance Unify Type -- TODO: continue from here

instance Unify SimpleType where
  unify t' t@VarType {} = unify t t'
  unify t@(VarType tVar) t'
    | tVar `Set.member` freeTypeVars t' =
      return (Map.singleton tVar $ SimpleType t', t')
    | otherwise =
      let err = Occurs tVar (SimpleType t')
       in tell (++ [err]) $> (mempty, t)
  unify (TupleType ts) (TupleType ts') = go ts ts' id mempty
    where
      go (h:t) (h':t') acc subst = do
        (subst', h'') <- (subst `apply` h) `unify` (subst `apply` h')
        go t t' (acc . (h'' :)) (subst' `apply` subst)
      go [] [] acc subst = return (subst, TupleType $ acc [])
      go _ _ acc subst =
        let err = TupleMismatch ts ts'
         in tell (++ [err]) $> (subst, TupleType $ acc [])
  unify (FunctionType args ret) (FunctionType args' ret') = do
    (subst, args'') <- unify args args'
    (subst', ret'') <- unify (subst `apply` ret) (subst `apply` ret')
    return (subst' `apply` subst, FunctionType (subst' `apply` args'') ret'')
  unify (AddrType t) (AddrType t') = fmap AddrType <$> unify t t'
  unify t t'
    | t == t' = return (mempty, t)
    | otherwise =
      let err = Mismatch (SimpleType t) (SimpleType t')
       in tell (++ [err]) $> (mempty, t)
-- factChecker :: MonadState Subst m => m [Facts]

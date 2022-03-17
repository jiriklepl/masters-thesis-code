{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Inference.Subst where

import safe Control.Lens.Setter ((%~))
import safe Data.Data (Data(gmapT), Typeable)
import Data.Generics (extT)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)

import safe CMM.Inference.Type
  ( Fact
  , FlatFact
  , PrimType
  , ToType(toType)
  , Type(VarType)
  , TypeVar(NoType, TypeVar, tVarParent)
  )
import safe CMM.Inference.TypeHandle (TypeHandle, consting, kinding, typing)

type Subst = Map TypeVar

class (ToType b, Typeable b, Data a, TypeCase b) =>
      Apply a b
  where
  apply :: Map TypeVar b -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCase subst go

class (ToType b, Typeable b, Data a, TypeCaseShallow b) =>
      ApplyShallow a b
  where
  applyShallow :: Map TypeVar b -> a -> a
  applyShallow subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCaseShallow subst go

class TypeCase b where
  typeCase ::
       Subst b
    -> (forall d. Data d =>
                    d -> d)
    -> b
    -> b

class TypeCaseShallow b where
  typeCaseShallow ::
       Subst b
    -> (forall d. Data d =>
                    d -> d)
    -> b
    -> b

instance TypeCase Type where
  typeCase subst go =
    \case
      t@(VarType tVar) -> maybe t toType $ tVar `Map.lookup` subst
      t -> gmapT go t

instance TypeCase TypeVar where
  typeCase subst _ = go
    where
      go tVar =
        case fromMaybe tVar $ tVar `Map.lookup` subst of
          tVar'@TypeVar {tVarParent = parent} -> tVar' {tVarParent = go parent}
          NoType -> NoType

instance TypeCaseShallow TypeVar where
  typeCaseShallow subst _ = go
    where
      go tVar =
        case fromMaybe tVar $ tVar `Map.lookup` subst of
          tVar'@TypeVar {} -> tVar'
          NoType -> NoType

instance (ToType b, Typeable b, TypeCase b) => Apply TypeVar b

instance (ToType b, Typeable b, TypeCaseShallow b) =>
         ApplyShallow TypeVar b

instance (ToType b, Typeable b, TypeCase b) => Apply Type b

instance (ToType b, Typeable b, TypeCase b) => Apply Fact b

instance (ToType b, Typeable b, TypeCase b) => Apply (FlatFact Type) b

instance (ToType b, Typeable b, TypeCase b) => Apply PrimType b

instance Apply b b => Apply (Map TypeVar b) b where
  subst' `apply` subst
    | null subst' = subst
    | otherwise = (apply subst' <$> subst) <> subst'

instance (Apply a t, Apply b t) => Apply (a, b) t where
  subst `apply` (a, b) = (subst `apply` a, subst `apply` b)

instance (Apply TypeVar t, ToType t, Typeable t, TypeCase t) =>
         Apply TypeHandle t where
  apply subst =
    (typing %~ apply subst) .
    (consting %~ apply subst) . (kinding %~ apply subst)

foldTVarSubsts :: [Map TypeVar TypeVar] -> Map TypeVar TypeVar
foldTVarSubsts = foldr apply mempty

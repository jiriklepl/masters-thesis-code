{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Inference.Subst where

import safe Control.Lens.Setter ((%~))
import safe Data.Data (Data(gmapT), Typeable)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)

import safe CMM.Data.Generics ((*|*))
import safe CMM.Inference.Fact (Fact, FlatFact, NestedFact, Qual, Scheme((:.)))
import safe CMM.Inference.Type (ToType(toType), Type(VarType))
import safe CMM.Inference.TypeCompl (PrimType)
import safe CMM.Inference.TypeHandle (TypeHandle, consting, kinding, typing)
import safe CMM.Inference.TypeVar (TypeVar(NoType, TypeVar, tVarParent))

type Subst = Map TypeVar

schemeNCase :: Apply (Qual n) b => Subst b -> Scheme n -> Scheme n
schemeNCase subst (tVars :. qN) =
  tVars :. (Map.withoutKeys subst tVars `apply` qN)

class (ToType b, Typeable b, Data a, TypeCase b) =>
      Apply a b
  where
  apply :: Map TypeVar b -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = typeCase subst go *|* schemeFactCase *|* schemeTypeCase *|* gmapT go
      schemeTypeCase :: Scheme Type -> Scheme Type
      schemeTypeCase = schemeNCase subst
      schemeFactCase :: Scheme Fact -> Scheme Fact
      schemeFactCase = schemeNCase subst

class (ToType b, Typeable b, Data a, TypeCaseShallow b) =>
      ApplyShallow a b
  where
  applyShallow :: Map TypeVar b -> a -> a
  applyShallow subst = go
    where
      go :: Data d => d -> d
      go = typeCaseShallow subst go *|* gmapT go

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

instance Apply n b => Apply [n] b

instance (ToType b, Typeable b, TypeCase b) => Apply PrimType b

instance (ToType b, Typeable b, TypeCase b) => Apply Type b

instance (ToType b, Typeable b, TypeCase b) => Apply (Qual Type) b

instance Apply n b => Apply (NestedFact n) b

instance (ToType b, Typeable b, TypeCase b) => Apply (Qual Fact) b

instance (ToType b, Typeable b, TypeCase b) => Apply (FlatFact Type) b

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

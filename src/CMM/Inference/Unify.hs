{-# LANGUAGE Safe #-}

module CMM.Inference.Unify where

import safe Control.Applicative (Applicative(pure), (<$>))
import safe Control.Lens ((%~))
import safe Control.Lens.Tuple (_1, _2)
import safe Control.Monad (Functor(fmap), Monad(return))
import safe Data.Bool (not, otherwise)
import safe Data.Either (Either(Left, Right))
import safe Data.Eq (Eq((/=), (==)))
import safe Data.Function ((.))
import safe qualified Data.Map as Map
import safe Data.Map (Map)
import safe Data.Maybe (Maybe(Just, Nothing))
import safe Data.Monoid (Monoid(mempty), (<>))
import safe Data.Ord (Ord((<), (>)))
import safe qualified Data.Set as Set
import safe GHC.Err (undefined)

import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.Subst (Apply(apply), Subst)
import safe CMM.Inference.Type
  ( ToType(toType)
  , Type(ComplType, ErrorType, VarType)
  )
import safe CMM.Inference.TypeCompl
  ( PrimType
  , TypeCompl(AddrType, AppType, FunctionType, TupleType)
  )
import safe CMM.Inference.TypeKind
  ( HasTypeKind(setTypeKind)
  , combineTypeKind
  , matchKind
  )
import safe CMM.Inference.TypeVar
  ( TypeVar(NoType, TypeVar, tVarKind)
  , familyDepth
  )
import safe CMM.Inference.Unify.Error
  ( UnificationError(BadKind, GotErrorType, Mismatch, Occurs)
  )

class Unify a b | a -> b where
  unify :: a -> a -> Either [UnificationError] (Subst b, a)

bind :: TypeVar -> Type -> Either [UnificationError] (Subst Type, Type)
bind tVar@TypeVar {} (VarType tVar') =
  (_1 %~ fmap toType) . (_2 %~ toType) <$> unify tVar tVar'
bind tVar@TypeVar {} t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']
bind tVar t = Left [toType tVar `unifyMismatch` t]

unifyMismatch :: Type -> Type -> UnificationError
unifyMismatch = Mismatch "Types are not unifiable"

instance Unify TypeVar TypeVar where
  unify tVar@TypeVar {tVarKind = kind} tVar'@TypeVar {tVarKind = kind'}
    | not (matchKind tVar tVar') = Left [BadKind (VarType tVar) (VarType tVar')]
    | otherwise = improve <$> tVar `unifyLax` tVar'
    where
      kind'' = kind `combineTypeKind` kind'
      improve pair@(subst, tVar''@TypeVar {tVarKind = kind'''})
        | kind''' /= kind'' =
          let tVar''' = tVar'' {tVarKind = kind''}
           in ( (setTypeKind kind'' <$> subst) <> Map.singleton tVar'' tVar'''
              , tVar''')
        | otherwise = pair
      improve (_, NoType) = undefined -- TODO: logic error
  unify tVar tVar' = Left [toType tVar `unifyMismatch` toType tVar']

unifyLax ::
     TypeVar
  -> TypeVar
  -> Either [UnificationError] (Map TypeVar TypeVar, TypeVar)
unifyLax tVar@TypeVar {} tVar'@TypeVar {}
  | tVar == tVar' = Right (mempty, tVar')
  | familyDepth tVar > familyDepth tVar' =
    Right (Map.singleton tVar tVar', tVar')
  | familyDepth tVar < familyDepth tVar' =
    Right (Map.singleton tVar' tVar, tVar)
  | tVar > tVar' = Right (Map.singleton tVar tVar', tVar')
  | otherwise = Right (Map.singleton tVar' tVar, tVar)
unifyLax tVar tVar' = Left [toType tVar `unifyMismatch` toType tVar']

unifyMany ::
     (Unify a b1, Apply a b2, Apply (Map TypeVar b2) b1, Apply b2 b2)
  => [UnificationError]
  -> [a]
  -> [a]
  -> Either [UnificationError] (Map TypeVar b2, [a])
unifyMany msg = go mempty
  where
    go subst (x:xs) (y:ys) = do
      (subst', z) <- apply subst x `unify` apply subst y
      (subst'', zs) <- go (subst' `apply` subst) xs ys
      return (subst'' `apply` subst, apply subst'' z : zs)
    go subst [] [] = pure (subst, [])
    go _ _ _ = Left msg

unifyFold ::
     [TypeVar] -> Either [UnificationError] (Map TypeVar TypeVar, Maybe TypeVar)
unifyFold = go mempty
  where
    go subst [] = return (subst, Nothing)
    go subst [tVar] = return (subst, Just tVar)
    go subst (tVar:tVar':tVars) = do
      (subst', tVar'') <- apply subst tVar `unify` apply subst tVar'
      go (subst' `apply` subst) (tVar'' : tVars)

unifyCompl ::
     (Apply b b, Apply a b, Unify a b, Eq a, ToType a)
  => TypeCompl a
  -> TypeCompl a
  -> Either [UnificationError] (Map TypeVar b, TypeCompl a)
unifyCompl t t' =
  case (t, t') of
    (TupleType ts, TupleType ts') -> (_2 %~ TupleType) <$> unifyMany msg ts ts'
    (FunctionType args ret, FunctionType args' ret') -> do
      (subst, args'') <- unifyMany msg args args'
      (subst', ret'') <- apply subst ret `unify` apply subst ret'
      return (subst' `apply` subst, FunctionType (apply subst' <$> args'') ret'')
    (AppType app arg, AppType app' arg') -> do
      (subst, app'') <- app `unify` app'
      (subst', arg'') <- apply subst arg `unify` apply subst arg'
      return (subst' `apply` subst, AppType (subst' `apply` app'') arg'')
    (AddrType addr, AddrType addr') -> (_2 %~ AddrType) <$> unify addr addr'
    _
      | t == t' -> return (mempty, t)
      | otherwise -> Left msg
  where
    msg = [toType t `unifyMismatch` toType t']

instance Unify PrimType TypeVar where
  unify = unifyCompl

instance Unify Type Type where
  ErrorType text `unify` ErrorType text' =
    Left [GotErrorType text, GotErrorType text']
  ErrorType text `unify` _ = Left [GotErrorType text]
  _ `unify` ErrorType text = Left [GotErrorType text]
  t `unify` t'
    | t == t' = Right (mempty, t)
  VarType tVar `unify` t' = bind tVar t'
  t `unify` VarType tVar' = bind tVar' t
  ComplType t `unify` ComplType t' = (_2 %~ ComplType) <$> unifyCompl t t'

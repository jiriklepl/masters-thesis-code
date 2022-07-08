{-# LANGUAGE Safe #-}

module CMM.Inference.Unify where

import safe Control.Lens ( (%~), _1, _2 )
import safe Data.Either (isRight)
import safe qualified Data.Map as Map
import safe Data.Map (Map)
import safe qualified Data.Set as Set

import safe CMM.Data.Way (Way(Backward, Both, Forward), otherWay)
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.Subst (Apply(apply), Subst)
import safe CMM.Inference.Type
  ( ToType(toType)
  , Type(ComplType, VarType)
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
  ( UnificationError(BadKind, Mismatch, Occurs)
  )
import safe CMM.Utils ( HasCallStack )

unify :: (HasCallStack, Unify a b) => a -> a -> Either [UnificationError] (Subst b, a)
unify = unifyDirected Both

unifiable :: (HasCallStack, Unify a b) => a -> a -> Bool
unifiable = (isRight .) . unify

instanceOf :: (HasCallStack, Unify a b) => a -> a -> Bool
inst `instanceOf` scheme = isRight $ unifyDirected Backward inst scheme

instantiateFrom :: (HasCallStack, Unify a b) => a -> a -> Either [UnificationError] (Subst b, a)
instantiateFrom = unifyDirected Backward

schemeOf :: Unify a b => a -> a -> Bool
schemeOf = flip instanceOf

-- | unifies two type variables ignoring their type kinds
unifyLax ::
     TypeVar
  -> TypeVar
  -> Either [UnificationError] (Map TypeVar TypeVar, TypeVar)
unifyLax = unifyLaxDirected Both

class Unify a b | a -> b where
  unifyDirected :: HasCallStack => Way -> a -> a -> Either [UnificationError] (Subst b, a)

bind :: Way -> TypeVar -> Type -> Either [UnificationError] (Subst Type, Type)
bind Backward tVar t' = Left [toType tVar `unifyMismatch` t']
bind way tVar@TypeVar {} (VarType tVar') =
  (_1 %~ fmap toType) . (_2 %~ toType) <$> unifyDirected way tVar tVar'
bind _ tVar@TypeVar {} t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']
bind _ tVar t' = Left [toType tVar `unifyMismatch` t']

unifyMismatch :: Type -> Type -> UnificationError
unifyMismatch = Mismatch "Types are not unifiable"

instance Unify TypeVar TypeVar where
  unifyDirected way tVar@TypeVar {tVarKind = kind} tVar'@TypeVar {tVarKind = kind'}
    | not (matchKind tVar tVar') = Left [BadKind (VarType tVar) (VarType tVar')]
    | otherwise = improve <$> tVar `unifyLax'` tVar'
    where
      unifyLax' = unifyLaxDirected way
      kind'' = kind `combineTypeKind` kind'
      improve pair@(subst, tVar''@TypeVar {tVarKind = kind'''})
        | kind''' /= kind'' =
          let tVar''' = tVar'' {tVarKind = kind''}
           in ( (setTypeKind kind'' <$> subst) <> Map.singleton tVar'' tVar'''
              , tVar''')
        | otherwise = pair
      improve (_, NoType) = undefined -- TODO: logic error
  unifyDirected _ tVar tVar' = Left [toType tVar `unifyMismatch` toType tVar']

unifyLaxDirected ::
     Way
  -> TypeVar
  -> TypeVar
  -> Either [UnificationError] (Map TypeVar TypeVar, TypeVar)
unifyLaxDirected way tVar@TypeVar {} tVar'@TypeVar {}
  | way == Forward = forward
  | way == Backward = backward
  | tVar == tVar' = Right (mempty, tVar')
  | familyDepth tVar > familyDepth tVar' = forward
  | familyDepth tVar < familyDepth tVar' = backward
  | tVar > tVar' = forward
  | otherwise = backward
  where
    forward = Right (Map.singleton tVar tVar', tVar')
    backward = Right (Map.singleton tVar' tVar, tVar)
unifyLaxDirected _ tVar tVar' = Left [toType tVar `unifyMismatch` toType tVar']

unifyMany ::
     (HasCallStack, Unify a b1, Apply a b2, Apply (Map TypeVar b2) b1, Apply b2 b2)
  => Way
  -> [UnificationError]
  -> [a]
  -> [a]
  -> Either [UnificationError] (Map TypeVar b2, [a])
unifyMany way msg = go mempty
  where
    unify' = unifyDirected way
    go subst (x:xs) (y:ys) = do
      (subst', z) <- apply subst x `unify'` apply subst y
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
     (HasCallStack, Apply b b, Apply a b, Unify a b, Eq a, ToType a)
  => Way
  -> TypeCompl a
  -> TypeCompl a
  -> Either [UnificationError] (Map TypeVar b, TypeCompl a)
unifyCompl way t t'
  | TupleType ts <- t
  , TupleType ts' <- t' = (_2 %~ TupleType) <$> ts `unifyMany'` ts'
  | FunctionType args ret <- t
  , FunctionType args' ret' <- t' = do
    (subst, args'') <- args `unifyMany'` args'
    (subst', ret'') <- apply subst ret `unify'` apply subst ret'
    return (subst' `apply` subst, FunctionType (apply subst' <$> args'') ret'')
  | AppType app arg <- t
  , AppType app' arg' <- t' = do
    (subst, app'') <- app `unify'` app'
    (subst', arg'') <- apply subst arg `unify'` apply subst arg'
    return (subst' `apply` subst, AppType (subst' `apply` app'') arg'')
  | AddrType addr <- t
  , AddrType addr' <- t' = (_2 %~ AddrType) <$> unify' addr addr'
  | t == t' = return (mempty, t)
  | otherwise = Left msg
  where
    unifyMany' = unifyMany way msg
    unify' = unifyDirected way
    msg = [toType t `unifyMismatch` toType t']

instance Unify PrimType TypeVar where
  unifyDirected = unifyCompl

instance Unify Type Type where
  unifyDirected way t t'
    | t == t' = Right (mempty, t)
    | way /= Backward
    , VarType tVar <- t = bind way tVar t'
    | way /= Forward
    , VarType tVar' <- t' = bind (otherWay way) tVar' t
    | ComplType tCompl <- t
    , ComplType tCompl' <- t' =
      (_2 %~ ComplType) <$> unifyCompl way tCompl tCompl'
    | otherwise = Left [t `unifyMismatch` t']

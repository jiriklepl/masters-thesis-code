{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CMM.Inference where

import safe Control.Monad.Writer.Lazy
import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Control.Lens.Tuple
import safe Data.Data
import safe Data.Functor

import safe Data.Generics.Aliases
import safe Data.Text (Text)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe qualified Data.Set as Set
import Prelude hiding (const)

import safe CMM.Inference.Type
import safe CMM.Inference.State


class Unify a where
  unify :: a -> a -> Either [UnificationError] (Subst, a)

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
          t@(VarType tVar) -> fromMaybe t $ tVar `Map.lookup` subst
          t -> gmapT go t

instance Apply Type

instance Apply Fact

instance Apply Subst where
  subst' `apply` subst = (apply subst' <$> subst) <> subst'

class UnionPropagate a where
  unionPropagate :: MonadInferencer m => TypeVar -> a -> m ()

-- equivalent :: Facts -> Facts -> Bool
-- equivalent = undefined

freshInst :: (MonadInferencer m, MonadIO m, Data a) => Scheme a -> m (a, Facts)
freshInst (given :. facts :=> t) = do
  tVars <- traverse ((VarType <$>) . freshTypeHandle) given
  let varMap = Map.fromList $ zip [0..] tVars -- TODO: int map?
      leaf (VarType (TypeLam i _)) = varMap Map.! i
      leaf t' = gmapT transform t'
      transform :: Data d => d -> d
      transform = gmapT transform `extT` leaf
  return (transform t, transform facts)

instance UnionPropagate Type where
  unionPropagate NoType  _ = return () -- error "Implementation error"
  unionPropagate _ (ErrorType text) = errors <>= [GotErrorType text]
  unionPropagate tVar (VarType tVar') = facts <>= [SubType tVar tVar', SubType tVar' tVar]
  unionPropagate tVar (TupleType ts) = do
    tTypes <- traverse (\_ -> freshTypeHandle Star) ts
    facts <>= zipWith TypeUnion tTypes ts <> fmap (SubConst tVar) tTypes
  unionPropagate tVar (FunctionType args ret) = do
    argsType <- freshTypeHandle Star
    retType <- freshTypeHandle Star
    facts <>= [TypeUnion argsType args, TypeUnion retType ret, SubConst retType argsType, SubConst argsType tVar]
  unionPropagate _ _ = return () -- TODO: check this

matchKind :: (HasKind a, HasKind b) => a -> b -> Bool
matchKind a b = getKind a == getKind b

bind :: TypeVar -> Type -> Either [UnificationError] (Subst, Type)
bind tVar t'@(VarType tVar')
  | tVar == tVar' = Right (mempty, t')
bind tVar t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar  t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']

subUnify :: MonadInferencer m => TypeVar -> TypeVar -> m ()
subUnify tVar tVar' = do
  facts <>= [SubKind tVar tVar', SubConst tVar tVar']
  unifyVars tVar tVar'

unifyVars :: MonadInferencer m => TypeVar -> TypeVar -> m ()
unifyVars tVar tVar' = use typing >>= \typings ->
    addTyping $ go (tVar `Map.lookup` typings) (tVar' `Map.lookup` typings)
    where
      go Nothing t' = bind tVar $ fromMaybe (VarType tVar') t'
      go t Nothing = bind tVar' $ fromMaybe (VarType tVar) t
      go (Just t) (Just t') = unify t t'

addTyping :: MonadInferencer m => Either [UnificationError] (Subst, Type) -> m ()
addTyping (Right (subst, _)) = do
  typings <- apply subst <$> use typing
  case Map.foldrWithKey go (Right typings) subst of
    Right typings' -> typing .= typings'
    Left errs -> errors <>= errs
  where
    go key new (Right typings) = case Map.insertLookupWithKey (\_ _ _ -> new) key new typings of
      (Nothing, typings') -> Right typings'
      (Just old, _)
        | old == new -> Right typings
        | otherwise -> (`apply` typings) . (^. _1) <$> unify old new
    go _ _ errs = errs
addTyping (Left errs) = errors <>= errs

instance Unify Type where
  unify (ErrorType text) (ErrorType text') = Left [GotErrorType text, GotErrorType text']
  unify (ErrorType text) _ = Left [GotErrorType text]
  unify _ (ErrorType text) = Left [GotErrorType text]
  unify scheme@Forall{} scheme'@Forall{} = Left [IllegalPolytype scheme, IllegalPolytype scheme']
  unify t t'
    | t == t' = Right (mempty, t)
  unify (VarType tVar) t' = bind tVar t'
  unify t (VarType tVar') = bind tVar' t
  unify (FunctionType args ret) (FunctionType args' ret') = do
    (subst, args'') <- unify args args'
    unify (subst `apply` ret) (subst `apply` ret') <&> (_1 %~ (`apply` subst)) . (_2 %~ FunctionType args'')
  unify (TupleType ts) (TupleType ts') = (_2 %~ TupleType) <$> go ts ts' mempty
    where
      go (t:rest) (t':rest') subst = do
        (subst', t'') <- unify (subst `apply` t) (subst `apply` t')
        (_2 %~ (t'':)) <$> go rest rest' (subst' `apply` subst)
      go [] [] subst = return (subst, [])
      go _ _ _ = Left [TupleMismatch ts ts']
  unify (AddrType t) (AddrType t') = (_2 %~ AddrType) <$> unify t t'
  unify t t' = Left [Mismatch t t']

-- hasKind :: Text -> Type -> Maybe [UnificationError]
-- hasKind _ (SimpleType VarType{}) = Nothing
-- hasKind _ (ErrorType text) = Just [GotErrorType text]
-- hasKind _ NoType = Just []
-- hasKind kind t@Forall{} = Just [NoKind kind t]
-- hasKind kind (AnnotType (Just kind', _, _) _)
--   | kind == kind' = Just []
-- hasKind kind t = Just [NoKind kind t]

-- onRegister :: Text -> Type -> Maybe [UnificationError]
-- onRegister _ (SimpleType VarType{}) = Nothing
-- onRegister _ (ErrorType text) = Just [GotErrorType text]
-- onRegister _ NoType = Just []
-- onRegister reg t@Forall{} = Just [NoRegister reg t]
-- onRegister reg (AnnotType (_, _, Just reg') _)
--   | reg == reg' = Just []
-- onRegister reg t = Just [NoRegister reg t]

infer :: MonadInferencer m => Fact -> m ()
infer fact = case fact of
  SubType t t' -> subUnify t t'
  SubKind t t' -> subKinding %= Map.insertWith Set.union t' (Set.singleton t)
  SubConst t t' -> subConsting %= Map.insertWith Set.union t (Set.singleton t')
  Typing t t' -> addTyping $ bind t t'
  TypeUnion t t' -> unionPropagate t t' $> bind t t' >>= addTyping
  InstType tVar inst -> go tVar
    where
      go var = uses typing (var `Map.lookup`) >>= \case
        Just (Forall scheme) -> do
          (inst', facts') <- freshInst scheme
          facts <>= facts'
          unionPropagate inst inst' $> bind inst inst' >>= addTyping
        Just (VarType var') -> go var'
        Just t -> errors <>= [IllegalPolytype t]
        Nothing -> facts <>= [fact]
  ConstnessLimit const t ->
    consting %= Map.insertWith max t (const, ConstExpr)
  KindLimit kind t -> do
    kinding <~ (use kinding >>= Map.alterF go t)
    where
      go Nothing = return (Just kind)
      go (Just kind') = kind `intersect` kind'
  -- OnRegister reg t ->
  --   case onRegister reg t of
  --     Nothing -> return [fact]
  --     Just errs -> return [] -- TODO: do something about `errs`
  Constraint classHandle ts -> return () -- undefined

-- returns Nothing on failure (if the intersection is empty) and also writes errors
intersect :: MonadInferencer m => Text -> Text -> m (Maybe Text)
intersect = undefined

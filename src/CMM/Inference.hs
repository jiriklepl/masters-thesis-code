{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module CMM.Inference where

import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Control.Lens.Traversal
import safe Control.Lens.Tuple

-- TODO: add the overlap check for instances
-- TODO: add the overload resolution for instances to monomorphization
import safe Control.Monad.Writer.Lazy
import safe Data.Data
import Data.Foldable
import safe Data.Function
import safe Data.Functor
import safe Data.Generics.Aliases
import safe qualified Data.Graph as Graph
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Prelude hiding (const)

import safe CMM.Inference.State
import safe CMM.Inference.Type
import safe CMM.Data.Bounds
import safe CMM.Data.Orderable

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

addUnifying :: MonadInferencer m => TypeVar -> Type -> m ()
addUnifying tVar t = do
  unifs <- use unifying
  traverse_ (traverse_ (unionPropagate t)) unifs
  unifying .= Map.insertWith Set.union tVar (Set.singleton t) unifs

class UnionPropagate a b where
  unionPropagate :: MonadInferencer m => a -> b -> m ()

fresherVarType :: MonadInferencer m => TypeVar -> m TypeVar
fresherVarType tVar = do
  handleCounter += 1
  uses handleCounter $ \i -> TypeVar i (getTypeKind tVar) (TVarInst tVar)

freshInst :: (MonadInferencer m, MonadIO m, Data a) => Scheme a -> m (Facts, a)
freshInst (given :. facts' :=> t) = do
  varMap <- sequence $ Map.fromSet fresherVarType given
  let transform :: Data d => d -> d
      transform = gmapT transform `extT` leaf
      leaf tVar = fromMaybe tVar (tVar `Map.lookup` varMap)
  return (transform facts', transform t)

instance UnionPropagate Type Type where
  unionPropagate (ErrorType text) (ErrorType text') =
    errors <>= [GotErrorType text, GotErrorType text']
  unionPropagate (ErrorType text) _ = errors <>= [GotErrorType text]
  unionPropagate _ (ErrorType text) = errors <>= [GotErrorType text]
  unionPropagate (VarType tVar) t = unionPropagate tVar t
  unionPropagate t (VarType tVar) = unionPropagate tVar t
  unionPropagate (TupleType ts) (TupleType ts') =
    zipWithM_ unionPropagate ts ts'
  unionPropagate (FunctionType arg ret) (FunctionType arg' ret') =
    unionPropagate arg arg' >> unionPropagate ret ret'
  unionPropagate (AddrType t) (AddrType t') = unionPropagate t t'
  unionPropagate _ _ = return () -- TODO: check this

-- TODO: maybe add simplifications for types that already depend just on tVars
instance UnionPropagate TypeVar Type where
  unionPropagate NoType _ = return () -- TODO: error "Implementation error"
  unionPropagate _ (ErrorType text) = errors <>= [GotErrorType text]
  unionPropagate tVar (VarType tVar') = do
    facts <>= [SubType tVar tVar', SubType tVar' tVar]
  unionPropagate tVar (TupleType ts) = do
    tTypes <- traverse (\_ -> freshTypeHelper Star) ts
    addUnifying tVar $ makeTuple (VarType <$> tTypes)
    facts <>= zipWith TypeUnion tTypes ts <> fmap (SubConst tVar) tTypes
  unionPropagate tVar (FunctionType args ret) = do
    argsType <- freshTypeHelper Star
    retType <- freshTypeHelper Star
    addUnifying tVar $ VarType argsType `makeFunction` VarType retType
    facts <>=
      [ TypeUnion argsType args
      , TypeUnion retType ret
      , SubConst retType argsType
      , SubConst argsType tVar
      ]
  unionPropagate _ _ = return () -- TODO: check this

matchKind :: (HasTypeKind a, HasTypeKind b) => a -> b -> Bool
matchKind a b = getTypeKind a == getTypeKind b

bind :: TypeVar -> Type -> Either [UnificationError] (Subst, Type)
bind tVar t'@(VarType tVar')
  | tVar == tVar' = Right (mempty, t')
bind tVar t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']

subUnify :: MonadInferencer m => TypeVar -> TypeVar -> m ()
subUnify tVar tVar' = do
  facts <>= [SubKind tVar tVar', SubConst tVar tVar']
  unifyVars tVar tVar'

unifyVars :: MonadInferencer m => TypeVar -> TypeVar -> m ()
unifyVars tVar tVar' =
  use typing >>= \typings ->
    addTyping $ go (tVar `Map.lookup` typings) (tVar' `Map.lookup` typings)
  where
    go Nothing t' = bind tVar $ fromMaybe (VarType tVar') t'
    go t Nothing = bind tVar' $ fromMaybe (VarType tVar) t
    go (Just t) (Just t') = unify t t'

addTyping ::
     MonadInferencer m => Either [UnificationError] (Subst, Type) -> m ()
addTyping (Right (subst, _)) = do
  typings <- apply subst <$> use typing
  case Map.foldrWithKey go (Right typings) subst of
    Right typings' -> typing .= typings'
    Left errs -> errors <>= errs
  where
    go key new (Right typings) =
      case Map.insertLookupWithKey (\_ _ _ -> new) key new typings of
        (Nothing, typings') -> Right typings'
        (Just old, _)
          | old == new -> Right typings
          | otherwise -> (`apply` typings) . (^. _1) <$> unify old new
    go _ _ errs = errs
addTyping (Left errs) = errors <>= errs

instance Unify Type where
  unify (ErrorType text) (ErrorType text') =
    Left [GotErrorType text, GotErrorType text']
  unify (ErrorType text) _ = Left [GotErrorType text]
  unify _ (ErrorType text) = Left [GotErrorType text]
  unify t t'
    | t == t' = Right (mempty, t)
  unify (VarType tVar) t' = bind tVar t'
  unify t (VarType tVar') = bind tVar' t
  unify (FunctionType args ret) (FunctionType args' ret') = do
    (subst, args'') <- unify args args'
    unify (subst `apply` ret) (subst `apply` ret') <&> (_1 %~ (`apply` subst)) .
      (_2 %~ FunctionType args'')
  unify (TupleType ts) (TupleType ts') = (_2 %~ TupleType) <$> go ts ts' mempty
    where
      go (t:rest) (t':rest') subst = do
        (subst', t'') <- unify (subst `apply` t) (subst `apply` t')
        (_2 %~ (t'' :)) <$> go rest rest' (subst' `apply` subst)
      go [] [] subst = return (subst, [])
      go _ _ _ = Left [TupleMismatch ts ts']
  unify (AddrType t) (AddrType t') = (_2 %~ AddrType) <$> unify t t'
  unify t t' = Left [Mismatch t t']

reduce :: MonadInferencer m => Fact -> m ()
reduce fact =
  case fact of
    SubType t t' -> subUnify t t'
    SubKind t t' -> subKinding %= Map.insertWith Set.union t' (Set.singleton t)
    SubConst t t' ->
      subConsting %= Map.insertWith Set.union t (Set.singleton t')
    Typing t t' -> addTyping $ bind t t'
    TypeUnion t t' -> unionPropagate t t' $> bind t t' >>= addTyping
    ConstnessLimit bounds t -> consting %= Map.insertWith (<>) t bounds
    KindLimit (Bounds minKind maxKind) t -> do
      kinding <~ (use kinding >>= Map.alterF go t)
      where go Nothing = return (Just kindBounds)
            go (Just kind) = return . Just $ kindBounds <> kind
            kindBounds = unmakeOrdered minKind `Bounds` unmakeOrdered maxKind
    InstType gen inst ->
      uses schemes (gen `Map.lookup`) >>= \case
        Just scheme -> do
          (fs', t) <- freshInst scheme
          facts <>= inst `TypeUnion` t : fs'
        Nothing -> facts <>= [fact]
    OnRegister reg t -> registerKind reg >>= reduce . (`KindLimit` t) . (`Bounds` maxBound) . makeOrdered
    Constraint {} -> facts <>= [fact]
    NestedFacts (kinds :. fs :=> [tVar `TypeUnion` t]) -> do
      let scheme = kinds :. fs :=> t
      (fs', t') <- freshInst scheme
      tVar' <- fresherVarType tVar
      facts <>= tVar' `TypeUnion` t' : fs'
      schemes %= Map.insert tVar scheme -- TODO: simplify the scheme

collectVertices ::
     MonadInferencer m
  => Getting (Map.Map TypeVar (Set.Set TypeVar)) Inferencer (Map.Map TypeVar (Set.Set TypeVar))
  -> m [(TypeVar, TypeVar)]
collectVertices from = do
  from' <- Map.toList <$> use from
  handles <- use handleCounter
  let edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> from')
      varMap =
        Map.fromList $ (\tVar@(TypeVar i _ _) -> (i, tVar)) <$>
        uncurry (<>) (unzip edges)
      graph =
        Graph.buildG (1, handles) $ (both %~ \(TypeVar i _ _) -> i) <$> edges
      vs = Graph.vertices graph
      pairs = [(v, v') | v <- vs, v' <- vs, v /= v', Graph.path graph v v']
  return $ pairs <&> both %~ \i -> varMap Map.! i

deduceKinds :: MonadInferencer m => m ()
deduceKinds = do
  pairs <- collectVertices subKinding
  for_ pairs $ \(v, v') -> do
    uses kinding (v' `Map.lookup`) >>=
      traverse_ ((kinding %=) . Map.insertWith (<>) v . (upperBound .~ maxBound))
    uses kinding (v `Map.lookup`) >>=
      traverse_ ((kinding %=) . Map.insertWith (<>) v' . (lowerBound .~ minBound))

deduceConsts :: MonadInferencer m => m ()
deduceConsts = do
  pairs <- collectVertices subConsting
  for_ pairs $ \(v, v') -> do
    uses consting (v' `Map.lookup`) >>=
      traverse_
        ((consting %=) . Map.insertWith (<>) v . (lowerBound .~ minBound))
    uses consting (v `Map.lookup`) >>=
      traverse_ ((consting %=) . Map.insertWith (<>) v' . (upperBound .~ maxBound))

registerKind :: MonadInferencer m => Text -> m DataKind
registerKind = undefined

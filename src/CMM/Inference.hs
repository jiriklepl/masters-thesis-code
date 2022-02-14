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

import safe Control.Applicative

import Control.Monad.State.Lazy hiding (state)
-- TODO: add the overlap check for instances
-- TODO: add the overload resolution for instances to monomorphization
import safe Control.Monad.Writer.Lazy
import safe Data.Data
import Data.Either
import Data.Foldable
import safe Data.Function
import safe Data.Functor
import safe Data.Generics.Aliases
import safe qualified Data.Graph as Graph
import Data.List
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import Data.Traversable
import safe Prelude hiding (const)

import safe CMM.Data.Bounds
import safe CMM.Data.Orderable
import safe CMM.Inference.State
import safe CMM.Inference.Type

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

addUnifying :: MonadInferencer m => TypeVar -> Type -> m Facts
addUnifying tVar t = do
  (found, unifs') <- uses unifying $ Map.insertLookupWithKey (const Set.union) tVar (Set.singleton t)
  unifying .= unifs'
  case found of
    Nothing -> return []
    Just old -> if t `Set.member` old
      then return []
      else concat <$> unionPropagate t `traverse` Set.toList old

class UnionPropagate a b where
  unionPropagate :: MonadInferencer m => a -> b -> m Facts

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
  unionPropagate (ErrorType text) (ErrorType text') = do
    errors <>= [GotErrorType text, GotErrorType text']
    return []
  unionPropagate (ErrorType text) _ = do
    errors <>= [GotErrorType text]
    return []
  unionPropagate _ (ErrorType text) = do
    errors <>= [GotErrorType text]
    return []
  unionPropagate (VarType tVar) t = unionPropagate tVar t
  unionPropagate t (VarType tVar) = unionPropagate tVar t
  unionPropagate (TupleType ts) (TupleType ts') =
    concat <$> zipWithM unionPropagate ts ts'
  unionPropagate (FunctionType arg ret) (FunctionType arg' ret') =
    liftA2 (++) (unionPropagate arg arg') (unionPropagate ret ret')
  unionPropagate (AddrType t) (AddrType t') = unionPropagate t t'
  unionPropagate _ _ = return [] -- TODO: check this

-- TODO: maybe add simplifications for types that already depend just on tVars
instance UnionPropagate TypeVar Type where
  unionPropagate NoType _ = return [] -- TODO: error "Implementation error"
  unionPropagate _ (ErrorType text) = do
    errors <>= [GotErrorType text]
    return []
  unionPropagate tVar (VarType tVar') = do
    return [SubType tVar tVar', SubType tVar' tVar]
  unionPropagate tVar (TupleType ts) = do
    tTypes <- ts `for` \_ -> freshTypeHelper Star
    (zipWith TypeUnion tTypes ts <> fmap (SubConst tVar) tTypes ++) <$>
      addUnifying tVar (makeTuple $ VarType <$> tTypes)
  unionPropagate tVar (FunctionType args ret) = do
    (fs, argsType) <- case args of
      VarType argsType -> return ([], argsType)
      _ -> do
        argsType <- freshTypeHelper Star
        return ([TypeUnion argsType args], argsType)
    (fs', retType) <- case ret of
      VarType retType -> return ([], retType)
      _ -> do
        retType <- freshTypeHelper Star
        return ([TypeUnion retType ret], retType)
    ((fs ++ fs' ++
      [ SubConst retType argsType
      , SubConst argsType tVar
      ]) ++) <$>
      addUnifying tVar (VarType argsType `makeFunction` VarType retType)
  unionPropagate _ _ = return [] -- TODO: check this

matchKind :: (HasTypeKind a, HasTypeKind b) => a -> b -> Bool
matchKind a b = getTypeKind a `go` getTypeKind b
  where
    ErrorKind _ `go` _ = False
    _ `go` ErrorKind _ = False

    GenericType `go` _ = True
    _ `go` GenericType = True

    Constraint `go` Constraint = True

    Star `go` Star = True

    (l :-> r) `go` (l' :-> r') = go l l' && go r r'
    _ `go` _ = False

bind :: TypeVar -> Type -> Either [UnificationError] (Subst, Type)
bind tVar t'@(VarType tVar')
  | tVar == tVar' = Right (mempty, t')
bind tVar t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']

subUnify :: MonadInferencer m => TypeVar -> TypeVar -> m Facts
subUnify tVar tVar' =
  unifyVars tVar tVar' $> [SubKind tVar tVar', SubConst tVar tVar']

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

reduceOne :: MonadInferencer m => Fact -> m (Bool, Facts)
reduceOne fact =
  case fact of
    SubType t t' -> (True, ) <$> subUnify t t'
    SubKind t t' -> do
      subKinding %= Map.insertWith Set.union t' (Set.singleton t)
      return (True, [])
    SubConst t t' -> do
      subConsting %= Map.insertWith Set.union t (Set.singleton t')
      return (True, [])
    Typing t t' -> addTyping (bind t t') $> (True, [])
    TypeUnion t t' -> (True, ) <$> unionPropagate t t' <* addTyping (bind t t')
    ConstnessLimit bounds t -> do
      consting %= Map.insertWith (<>) t bounds
      return (True, [])
    KindLimit (Bounds minKind maxKind) t -> do
      kinding <~ (use kinding >>= Map.alterF go t)
      return (True, [])
      where go Nothing = return (Just kindBounds)
            go (Just kind) = return . Just $ kindBounds <> kind
            kindBounds = unmakeOrdered minKind `Bounds` unmakeOrdered maxKind
    InstType {} -> return (False, [fact])
    OnRegister reg t ->
      registerKind reg >>= reduceOne . (`KindLimit` t) . (`Bounds` maxBound) .
      makeOrdered
    ClassConstraint {} -> return (False, [fact]) -- undefined
    NestedFact (tVars :. fs :=> tVar `TypeUnion` t) -> do
      let scheme = tVars :. fs :=> t
      (fs', t') <- freshInst scheme
      tVar' <- fresherVarType tVar
      schemes %= Map.insertWith (<>) tVar (Set.singleton scheme) -- TODO: simplify the scheme
      return . (True, ) $ tVar' `TypeUnion` t' : fs'
    NestedFact _ -> undefined -- error

reduceMany :: MonadInferencer m => Facts -> m (Facts, Facts)
reduceMany facts' = do
  let (instFacts, facts'') =
        partition
          (\case
             InstType {} -> True
             _ -> False)
          facts'
  (changes, newFacts) <- (_2 %~ concat) . unzip <$> traverse reduceOne facts''
  if or changes
    then (_1 %~ (instFacts ++)) <$> reduceMany newFacts
    else return (instFacts, newFacts)

reduce :: MonadInferencer m => Facts -> m (Facts, Facts)
reduce facts' = do
  result@(instFacts, facts'') <- reduceMany facts'
  (instFacts', facts''') <-
    (_2 %~ concat) . partitionEithers <$> traverse go instFacts
  if null facts'''
    then return result
    else (_1 %~ (instFacts' ++)) <$> reduce (facts''' ++ facts'')
  where
    go fact@(InstType gen inst) =
      uses schemes (gen `Map.lookup`) >>= \case
        Just schemes'
          | null schemes' -> undefined -- error
          | Set.size schemes' == 1 -> do
            (fs', t) <- freshInst $ Set.findMin schemes'
            return . Right $ inst `TypeUnion` t : fs'
          | otherwise -> return $ Left fact
        Nothing -> undefined -- error
    go _ = undefined -- impl error

solve :: MonadInferencer m => Facts -> m ()
solve facts' = do
  (instFacts, facts'') <- reduce facts'
  use errors >>= \errs ->
    if null errs
      then if null instFacts
             then unless (null facts'') undefined -- error
             else solve'
                    ((\(InstType gen inst) -> (gen, inst)) <$> instFacts)
                    facts''
      else undefined -- error

solve' :: MonadInferencer m => [(TypeVar, TypeVar)] -> Facts -> m ()
solve' instFacts facts' = do
  schemes' <-
    (Set.toList <$>) <$> uses schemes (Map.!) <&> (<$> ((^. _1) <$> instFacts))
  let tVars = (^. _2) <$> instFacts
  go schemes' tVars >>= \case
    Just facts'' -> solve (facts'' ++ facts')
    Nothing -> return ()
  where
    go ((scheme:choices):schemes'') (tVar:tVars') = do
      state <- get
      (fs', t) <- freshInst scheme
      result <-
        go schemes'' tVars' >>= \case
          Just fs'' -> do
            noErrors <- uses errors null
            if noErrors
              then return . Just $ tVar `TypeUnion` t : fs' ++ fs''
              else return Nothing
          _ -> return Nothing
      case result of
        Just {} -> return result -- no errors encountered
        _ ->
          if null choices
            then return Nothing -- all branches failed
                      -- try another branch
            else do
              put state -- rollback
              go (choices : schemes'') (tVar : tVars')
    go [] [] = return $ Just []
    go _ [] = error "implementation error" -- TODO: make this nicer
    go [] _ = error "implementation error" -- TODO: make this nicer
    go ([]:_) _ = error "implementation error"

collectVertices ::
     MonadInferencer m
  => Getting (Map.Map TypeVar (Set.Set TypeVar)) Inferencer (Map.Map TypeVar (Set.Set TypeVar))
  -> m [(TypeVar, TypeVar)]
collectVertices from = do
  from' <- Map.toList <$> use from
  handles <- use handleCounter
  let edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> from')
      varMap =
        Map.fromList $ (\tVar@(~(TypeVar i _ _)) -> (i, tVar)) <$>
        uncurry (<>) (unzip edges)
      graph =
        Graph.buildG (1, handles) $ (both %~ \(~(TypeVar i _ _)) -> i) <$> edges
      vs = Graph.vertices graph
      pairs = [(v, v') | v <- vs, v' <- vs, v /= v', Graph.path graph v v']
  return $ pairs <&> both %~ \i -> varMap Map.! i

deduceKinds :: MonadInferencer m => m ()
deduceKinds = do
  pairs <- collectVertices subKinding
  for_ pairs $ \(v, v') -> do
    uses kinding (v' `Map.lookup`) >>=
      traverse_
        ((kinding %=) . Map.insertWith (<>) v . (upperBound .~ maxBound))
    uses kinding (v `Map.lookup`) >>=
      traverse_
        ((kinding %=) . Map.insertWith (<>) v' . (lowerBound .~ minBound))

deduceConsts :: MonadInferencer m => m ()
deduceConsts = do
  pairs <- collectVertices subConsting
  for_ pairs $ \(v, v') -> do
    uses consting (v' `Map.lookup`) >>=
      traverse_
        ((consting %=) . Map.insertWith (<>) v . (lowerBound .~ minBound))
    uses consting (v `Map.lookup`) >>=
      traverse_
        ((consting %=) . Map.insertWith (<>) v' . (upperBound .~ maxBound))

registerKind :: MonadInferencer m => Text -> m DataKind
registerKind = undefined

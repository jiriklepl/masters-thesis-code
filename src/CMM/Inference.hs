{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module CMM.Inference where

import Debug.Trace

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
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import Data.Traversable
import safe Prelude hiding (const)

import safe CMM.Data.Bounds
import safe CMM.Data.Orderable
import safe CMM.Inference.State
import safe CMM.Inference.Type
import qualified Data.Bimap as Bimap
import CMM.Inference.TypeHandle (initTypeHandle, TypeHandle, kinding, consting, typing)
import Data.Tuple (swap)
import Control.Lens (Lens')
import CMM.Data.Lattice

class Unify a b | a -> b where
  unify :: a -> a -> Either [UnificationError] (Subst b, a)

class (ToType b, Data a) =>
      Apply a b
  where
  apply :: Map TypeVar b -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCase
      typeCase =
        \case
          t@(VarType tVar) -> maybe t toType $ tVar `Map.lookup` subst
          t -> gmapT go t

instance ToType b => Apply TypeVar b

instance ToType b => Apply Type b

instance ToType b => Apply Fact b

instance ToType b => Apply PrimType b

instance Apply b b => Apply (Map TypeVar b) b where
  subst' `apply` subst
    | null subst' = subst
    | otherwise = (apply subst' <$> subst) <> subst'

instance (Apply a t, Apply b t) => Apply (a, b) t where
  subst `apply` (a, b) = (subst `apply` a, subst `apply` b)

instance (Apply TypeVar t, ToType t) => Apply TypeHandle t where
  apply subst = (typing %~ apply subst) . (consting %~ apply subst) . (kinding %~ apply subst)

-- addUnifying :: MonadInferencer m => TypeVar -> Type -> m Facts
-- addUnifying tVar t = do
--   (found, unifs') <- uses unifying $ Map.insertLookupWithKey (const Set.union) tVar (Set.singleton t)
--   unifying .= unifs'
--   case found of
--     Nothing -> return []
--     Just old -> if t `Set.member` old
--       then return []
--       else concat <$> unionPropagate t `traverse` Set.toList old

-- checkGeneral :: MonadInferencer m => Way -> TypeVar -> Type -> m Facts
-- checkGeneral way tVar t = do
--   uses unifying (tVar `Map.lookup`) >>= \case
--     Nothing -> case way of
--       Fwd -> do
--         errors <>= [Mismatch "Type does not generalize" (VarType tVar) t]
--         return []
--       Bck -> return [] -- TODO: check this
--     Just t' -> if t `Set.member` t'
--       then return []
--       else concat <$> generalPropagate (otherWay way) t `traverse` Set.toList t'

-- class UnionPropagate a b where
--   unionPropagate :: MonadInferencer m => a -> b -> m Facts

-- class GeneralPropagate a b where
--   generalPropagate :: MonadInferencer m => Way -> a -> b -> m Facts

-- data Way = Fwd | Bck

-- otherWay :: Way -> Way
-- otherWay Fwd = Bck
-- otherWay Bck = Fwd

-- fresherVarType :: MonadInferencer m => TypeVar -> m TypeVar
-- fresherVarType tVar = do
--   handleCounter += 1
--   uses handleCounter $ \i -> TypeVar i (getTypeKind tVar) (TVarInst tVar)

-- fresherConstType :: MonadInferencer m => TypeVar -> m TypeVar
-- fresherConstType tVar = do
--   handleCounter += 1
--   uses handleCounter $ \i -> TypeConst i (getTypeKind tVar) (TVarInst tVar)

-- freshInst :: (MonadInferencer m, MonadIO m, Data a) => Scheme a -> m (Facts, a)
-- freshInst (given :. facts' :=> t) = do
--   varMap <- sequence $ Map.fromSet fresherVarType given
--   let transform :: Data d => d -> d
--       transform = gmapT transform `extT` leaf
--       leaf tVar = fromMaybe tVar (tVar `Map.lookup` varMap)
--   return (transform facts', transform t)

-- freshConstInst :: (MonadInferencer m, MonadIO m, Data a) => Scheme a -> m (Facts, a)
-- freshConstInst (given :. facts' :=> t) = do
--   varMap <- sequence $ Map.fromSet fresherConstType given
--   let transform :: Data d => d -> d
--       transform = gmapT transform `extT` leaf
--       leaf tVar = fromMaybe tVar (tVar `Map.lookup` varMap)
--   return (transform facts', transform t)

-- instance UnionPropagate Type Type where
--   unionPropagate (ErrorType text) (ErrorType text') = do
--     errors <>= [GotErrorType text, GotErrorType text']
--     return []
--   unionPropagate (ErrorType text) _ = do
--     errors <>= [GotErrorType text]
--     return []
--   unionPropagate _ (ErrorType text) = do
--     errors <>= [GotErrorType text]
--     return []
--   unionPropagate (VarType tVar) t = unionPropagate tVar t
--   unionPropagate t (VarType tVar) = unionPropagate tVar t
--   unionPropagate (TupleType ts) (TupleType ts') =
--     concat <$> zipWithM unionPropagate ts ts'
--   unionPropagate (FunctionType arg ret) (FunctionType arg' ret') =
--     liftA2 (++) (unionPropagate arg arg') (unionPropagate ret ret')
--   unionPropagate (AddrType t) (AddrType t') = unionPropagate t t'
--   unionPropagate _ _ = return [] -- TODO: check this

-- -- TODO: maybe add simplifications for types that already depend just on tVars
-- instance UnionPropagate TypeVar Type where
--   unionPropagate NoType _ = error "NoType encountered during unification" -- TODO
--   unionPropagate _ (ErrorType text) = do
--     errors <>= [GotErrorType text]
--     return []
--   unionPropagate tVar (VarType tVar') = do
--     return [SubType tVar tVar', SubType tVar' tVar]
--   unionPropagate tVar (TupleType ts) = do
--     tTypes <- ts `for` \_ -> freshTypeHelper Star
--     (zipWith TypeUnion tTypes ts <> fmap (SubConst tVar) tTypes ++) <$>
--       addUnifying tVar (makeTuple $ VarType <$> tTypes)
--   unionPropagate tVar (FunctionType args ret) = do
--     (fs, argsType) <- case args of
--       VarType argsType -> return ([], argsType)
--       _ -> do
--         argsType <- freshTypeHelper Star
--         return ([TypeUnion argsType args], argsType)
--     (fs', retType) <- case ret of
--       VarType retType -> return ([], retType)
--       _ -> do
--         retType <- freshTypeHelper Star
--         return ([TypeUnion retType ret], retType)
--     ((fs ++ fs' ++
--       [ SubConst retType argsType
--       , SubConst argsType tVar
--       ]) ++) <$>
--       addUnifying tVar (VarType argsType `makeFunction` VarType retType)
--   unionPropagate _ _ = return [] -- TODO: check this

-- instance GeneralPropagate Type Type where
--   generalPropagate _ (ErrorType text) (ErrorType text') = do
--     errors <>= [GotErrorType text, GotErrorType text']
--     return []
--   generalPropagate _ (ErrorType text) _ = do
--     errors <>= [GotErrorType text]
--     return []
--   generalPropagate _ _ (ErrorType text) = do
--     errors <>= [GotErrorType text]
--     return []
--   generalPropagate way (VarType tVar) t = generalPropagate way tVar t
--   generalPropagate way t (VarType tVar) = generalPropagate (otherWay way) tVar t
--   generalPropagate way (TupleType ts) (TupleType ts') =
--     concat <$> zipWithM (generalPropagate way) ts ts'
--   generalPropagate way (FunctionType arg ret) (FunctionType arg' ret') =
--     liftA2 (++) (generalPropagate way arg arg') (generalPropagate way ret ret')
--   generalPropagate way (AddrType t) (AddrType t') = generalPropagate way t t'
--   generalPropagate _ _ _ = return [] -- TODO: check this

-- instance GeneralPropagate TypeVar Type where
--   generalPropagate _ NoType _ = error "NoType encountered during unification" -- TODO
--   generalPropagate _ _ (ErrorType text) = do
--     errors <>= [GotErrorType text]
--     return []
--   generalPropagate way tVar t@(VarType _) =
--       checkGeneral way tVar t
--   generalPropagate way tVar (TupleType ts) = do
--     tTypes <- ts `for` \_ -> freshTypeHelper Star
--     (zipWith TypeUnion tTypes ts <> fmap (SubConst tVar) tTypes ++) <$>
--       checkGeneral way tVar (makeTuple $ VarType <$> tTypes)
--   generalPropagate way tVar (FunctionType args ret) = do
--     (fs, argsType) <- case args of
--       VarType argsType -> return ([], argsType)
--       _ -> do
--         argsType <- freshTypeHelper Star
--         return ([TypeUnion argsType args], argsType)
--     (fs', retType) <- case ret of
--       VarType retType -> return ([], retType)
--       _ -> do
--         retType <- freshTypeHelper Star
--         return ([TypeUnion retType ret], retType)
--     ((fs ++ fs' ++
--       [ SubConst retType argsType
--       , SubConst argsType tVar
--       ]) ++) <$>
--       checkGeneral way tVar (VarType argsType `makeFunction` VarType retType)
--   generalPropagate _ _ _ = return [] -- TODO: check this

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

bind :: TypeVar -> Type -> Either [UnificationError] (Subst Type, Type)
bind tVar t'@(VarType tVar')
  | tVar == tVar' = Right (mempty, t')
bind tVar@TypeVar{} t'
  | not (tVar `Set.member` freeTypeVars t') =
    if matchKind tVar t'
      then Right (Map.singleton tVar t', t')
      else Left [BadKind (VarType tVar) t']
  | otherwise = Left [Occurs tVar t']
bind tVar t = Left [toType tVar `unifyMismatch` t]

-- subUnify :: MonadInferencer m => TypeVar -> TypeVar -> m Facts
-- subUnify tVar tVar' =
--   unifyVars tVar tVar' $> [SubKind tVar tVar', SubConst tVar tVar']

-- unifyVars :: MonadInferencer m => TypeVar -> TypeVar -> m ()
-- unifyVars tVar tVar' =
--   use typing >>= \typings ->
--     addTyping $ go (tVar `Map.lookup` typings) (tVar' `Map.lookup` typings)
--   where
--     go Nothing t' = bind tVar $ fromMaybe (VarType tVar') t'
--     go t Nothing = bind tVar' $ fromMaybe (VarType tVar) t
--     go (Just t) (Just t') = unify t t'

-- addTyping ::
--      MonadInferencer m => Either [UnificationError] (Subst, Type) -> m ()
-- addTyping (Right (subst, _)) = do
--   typings <- apply subst <$> use typing
--   case Map.foldrWithKey go (Right typings) subst of
--     Right typings' -> typing .= typings'
--     Left errs -> errors <>= errs
--   where
--     go key new (Right typings) =
--       case Map.insertLookupWithKey (\_ _ _ -> new) key new typings of
--         (Nothing, typings') -> Right typings'
--         (Just old, _)
--           | old == new -> Right typings
--           | otherwise -> (`apply` typings) . (^. _1) <$> unify old new
--     go _ _ errs = errs
-- addTyping (Left errs) = errors <>= errs

unifyMismatch :: Type -> Type -> UnificationError
unifyMismatch = Mismatch "Types are not unifiable"

instance Unify TypeVar TypeVar where
  unify tVar@TypeVar{} tVar'
    | tVar == tVar' = Right (mempty, tVar')
    | otherwise = Right (Map.singleton tVar tVar', tVar')
  unify tVar' tVar@TypeVar{} = Right (Map.singleton tVar tVar', tVar')
  unify tVar tVar' =
    Left [toType tVar `unifyMismatch` toType tVar']

instance Unify PrimType TypeVar where
  unify (TupleType ts) (TupleType ts') = undefined

instance Unify Type Type where
  unify (ErrorType text) (ErrorType text') =
    Left [GotErrorType text, GotErrorType text']
  unify (ErrorType text) _ = Left [GotErrorType text]
  unify _ (ErrorType text) = Left [GotErrorType text]
  unify t t'
    | t == t' = Right (mempty, t)
  unify (VarType tVar) t' = bind tVar t'
  unify t (VarType tVar') = bind tVar' t
  unify funcT@(ComplType (FunctionType args ret)) funcT'@(ComplType (FunctionType args' ret')) = do
    (subst, ~(ret'':args'')) <- go (ret:args) (ret':args') mempty
    return (subst, ComplType $ FunctionType args'' ret'')
    where
      go (t:rest) (t':rest') subst = do
        (subst', t'') <- unify (subst `apply` t) (subst `apply` t')
        (_2 %~ (t'' :)) <$> go rest rest' (subst' `apply` subst)
      go [] [] subst = return (subst, [])
      go _ _ _ = Left [funcT `unifyMismatch` funcT']
  unify (ComplType (AppType app arg)) (ComplType (AppType app' arg')) = do
    (subst, app'') <- unify app app'
    unify (subst `apply` arg) (subst `apply` arg') <&> (_1 %~ (`apply` subst)) .
      (_2 %~ ComplType . AppType app'')
  unify (ComplType (TupleType ts)) (ComplType (TupleType ts')) = (_2 %~ ComplType . TupleType) <$> go ts ts' mempty
    where
      go (t:rest) (t':rest') subst = do
        (subst', t'') <- unify (subst `apply` t) (subst `apply` t')
        (_2 %~ (t'' :)) <$> go rest rest' (subst' `apply` subst)
      go [] [] subst = return (subst, [])
      go _ _ _ = Left [TupleMismatch ts ts']
  unify (ComplType (AddrType t)) (ComplType (AddrType t')) = (_2 %~ ComplType . AddrType) <$> unify t t'
  unify t t' = Left [t `unifyMismatch` t']

class FactCheck a where
  factCheck :: MonadInferencer m => a -> m ()

instance (Foldable t, FactCheck a) => FactCheck (t a) where
  factCheck = traverse_ factCheck

instance {-# OVERLAPPING #-} FactCheck (TypeCompl Type) where
  factCheck (TupleType ts) = factCheck ts
  factCheck (FunctionType ts t) = factCheck ts *> factCheck t
  factCheck (AppType t t') = factCheck t *> factCheck t'
  factCheck (AddrType t) = factCheck t
  factCheck _ = return ()

instance FactCheck Type where
  factCheck (VarType t) = factCheck t
  factCheck (ComplType t) = factCheck t
  factCheck _ = return ()

instance FactCheck Fact where
  factCheck fact =
    case fact of
      SubType t t' -> factCheck t *> factCheck t'
      SubKind t t' -> factCheck t *> factCheck t'
      SubConst t t' -> factCheck t *> factCheck t'
      Typing t t' -> factCheck t *> factCheck t'
      TypeUnion t t' -> factCheck t *> factCheck t'
      ClassUnion t t' -> factCheck t *> factCheck t'
      InstanceUnion t t' -> factCheck t *> factCheck t'
      ConstnessLimit _ t -> factCheck t
      KindLimit _ t -> factCheck t
      InstType t t' -> factCheck t *> factCheck t'
      OnRegister _ t -> factCheck t
      ClassConstraint _ t -> factCheck t
      ClassFact _ t -> factCheck t
      NestedFact (tVars :. facts' :=> fact') -> factCheck tVars *> factCheck facts' *> factCheck fact'

instance FactCheck TypeVar where
  factCheck tVar = do
    unifs %= Map.insertWith const tVar tVar
    handlize %= Bimap.tryInsert tVar (initTypeHandle NoTypeAnnot tVar)

freshTypeHelperWithHandle :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHelperWithHandle kind = do
  tVar <- freshTypeHelper kind
  handlize %= Bimap.insert tVar (initTypeHandle NoTypeAnnot tVar)
  return tVar

simplify :: MonadInferencer m => Type -> m TypeVar
simplify = \case
  ErrorType _ -> undefined
  VarType tVar -> return tVar
  ComplType (TupleType ts) ->
    traverse simplify ts >>= go . TupleType
  ComplType (FunctionType ts t) ->
    liftA2 FunctionType (traverse simplify ts) (simplify t) >>= go
  ComplType (AppType t t') ->
    liftA2 AppType (simplify t) (simplify t') >>= go
  ComplType (AddrType t) ->
    simplify t >>= go . AddrType
  ComplType StringType -> go StringType
  ComplType String16Type -> go String16Type
  ComplType LabelType -> go LabelType
  ComplType (TBitsType int) -> go (TBitsType int)
  ComplType BoolType -> go BoolType
  ComplType VoidType -> go VoidType
  where
    go primType = uses typize (primType `Bimap.lookupR`) >>= \case
      Nothing -> do
        tVar <- freshTypeHelperWithHandle Star
        typize %= Bimap.insert tVar primType
        return tVar
      Just tVar -> return tVar

getHandle :: MonadInferencer m => TypeVar -> m TypeHandle
getHandle tVar = uses handlize (Bimap.! tVar)


infix 6 `insertEdge`
insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

pushSubKind :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushSubKind handle handle' = subKinding %= handle ^. kinding `insertEdge` handle' ^. kinding

pushConst :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushConst handle handle' = subConsting %= handle ^. consting `insertEdge` handle' ^. consting

doFor :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
doFor a b = a >>= traverse b

doFor_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
doFor_ a b = a >>= traverse_ b

pushTyping :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushTyping handle handle' = do
  let t  = handle  ^. typing
      t' = handle' ^. typing
  case unify t t' of
    Left errs -> errors <>= errs
    Right (subst, _) -> do
      let
        fixIt = do
          fixTypize >>= \case
            True -> fixHandlize >>= \case
              True -> fixIt
              False -> fixSubs
            False -> fixSubs
      safeHandlizeUpdate (_2 . typing %~ apply subst) >>= flip when (void fixIt)

fixAll :: MonadInferencer m => m ()
fixAll = do
  results <- sequence [fixTypize, fixHandlize, fixSubs]
  when (or results) fixAll

fixSubs :: MonadInferencer m => m Bool
fixSubs = do
  unifs' <- use unifs
  let
    mapFold :: (Ord a, Semigroup b) => [(a,b)] -> Map a b
    mapFold = foldl (flip . uncurry $ Map.insertWith (<>)) Map.empty
    fixOne :: MonadInferencer m => Lens' Inferencer (Map TypeVar (Set TypeVar)) -> m ()
    fixOne which = do
      let applyUnifs = (_1 %~ apply unifs') . (_2 %~ Set.fromList . (apply unifs' <$>) . Set.toList)
      whichList <- uses which $ (applyUnifs <$>) . Map.toList
      let which' = mapFold whichList
      which .= Map.mapWithKey Set.delete which'
    fixBounds :: (MonadInferencer m, Lattice a) => Lens' Inferencer (Map TypeVar (Bounds a)) -> m ()
    fixBounds which = do
      whichList <- uses which $ ((_1 %~ apply unifs') <$>) . Map.toList
      which .= mapFold whichList
  fixOne subKinding
  fixOne subConsting
  fixBounds kindingBounds
  fixBounds constingBounds
  undefined

-- TODO: reduce duplication
safeHandlizeUpdate :: MonadInferencer m =>
  ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle)) -> m Bool
safeHandlizeUpdate change = do
  handles' <- uses handlize ((change <$>) . Bimap.toList)
  let
    handlize' = Bimap.fromList handles'
    forgottenKeys = Map.fromList handles' `Map.withoutKeys` Set.fromList (Bimap.keys handlize')
    forgottenValues = Map.fromList (swap <$> handles') `Map.withoutKeys` Set.fromList (Bimap.elems handlize')

    goValues [] subst = return subst
    goValues _ _ = undefined

    goKeys [] subst = return subst
    goKeys ((tVar, handle''):others) subst = do
      let tVar' = handlize' Bimap.!> handle''
      case apply subst tVar `unify` apply subst tVar' of
        Left errs -> do
          errors <>= errs
          return subst
        Right (subst', _) -> goKeys others $ subst' `apply` subst

  handlize .= handlize'
  if null forgottenKeys && null forgottenValues
    then return False
    else do
      subst <- goValues (Map.toList forgottenValues) (mempty :: Subst TypeVar) >>= goKeys (Map.toList forgottenKeys)
      unifs %= apply subst
      True <$ fixHandlize

fixHandlize :: MonadInferencer m => m Bool
fixHandlize = uses unifs apply >>= safeHandlizeUpdate

fixTypize :: MonadInferencer m => m Bool
fixTypize = do
  types' <- uses unifs (fmap . apply) <*> uses typize Bimap.toList
  let
    typize' = Bimap.fromList types'

    forgottenKeys = Map.fromList types' `Map.withoutKeys` Set.fromList (Bimap.keys typize')
    forgottenValues = Map.fromList (swap <$> types') `Map.withoutKeys` Set.fromList (Bimap.elems typize')

    -- TODO: reduce duplication
    goValues [] subst = return subst
    goValues ((primType, tVar):others) subst = do
      let primType' = typize' Bimap.! tVar
      case apply subst primType `unify` apply subst primType' of
        Left errs -> do
          errors <>= errs
          return subst
        Right (subst', _) -> goValues others $ subst' `apply` subst

    goKeys [] subst = return subst
    goKeys ((tVar, handle''):others) subst = do
      let tVar' = typize' Bimap.!> handle''
      case apply subst tVar `unify` apply subst tVar' of
        Left errs -> do
          errors <>= errs
          return subst
        Right (subst', _) -> goKeys others $ subst' `apply` subst

  typize .= typize'
  if null forgottenKeys && null forgottenValues
    then return False
    else do
      subst <- goValues (Map.toList forgottenValues) (mempty :: Subst TypeVar) >>= goKeys (Map.toList forgottenKeys)
      unifs %= apply subst
      True <$ fixTypize

fixFacts :: MonadState Inferencer m => Facts -> m Facts
fixFacts facts' = uses unifs $ flip fmap facts' . apply

reduceOne :: MonadInferencer m => Facts -> m (Bool, Facts)
reduceOne [ ] = return (False, [])
reduceOne (fact:facts) =
  case fact of
    SubType t t' -> do
      tVar <- simplify t
      tVar' <- simplify t'
      let facts' = tVar `subKind` tVar' : tVar `typeConstraint` tVar' : tVar `subConst` tVar' : facts
      (_1 .~ True) <$> reduceOne facts'
    SubKind t t' -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushSubKind handle handle'
      (_1 .~ True) <$> reduceOne facts
    SubConst t t' -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushConst handle handle'
      (_1 .~ True) <$> reduceOne facts
    Typing t t' -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushTyping handle handle'
      facts' <- uses unifs (flip fmap facts . apply)
      (_1 .~ True) <$> reduceOne facts'
--     TypeUnion t t' -> (True, ) <$> unionPropagate t t' <* addTyping (bind t t')
--     ConstnessLimit bounds t -> do
--       consting %= Map.insertWith (<>) t bounds
--       return (True, [])
--     KindLimit (Bounds minKind maxKind) t -> do
--       kinding <~ (use kinding >>= Map.alterF go t)
--       return (True, [])
--       where go Nothing = return (Just kindBounds)
--             go (Just kind) = return . Just $ kindBounds <> kind
--             kindBounds = unmakeOrdered minKind `Bounds` unmakeOrdered maxKind
--     InstType {} -> return (False, [fact])
--     OnRegister reg t ->
--       registerKind reg >>= reduceOne . (`KindLimit` t) . (`Bounds` maxBound) .
--       makeOrdered
--     ClassConstraint _ _ -> do
--       return (False, [fact])
--     ClassFact name handle -> do
--       instanceSchemes %= Map.insertWith (<>) name (Set.singleton $ Set.fromList [] :. [] :=> handle)
--       return (True, [])
--     NestedFact (tVars :. fs :=> tVar `TypeUnion` t) -> do
--       let
--         tVars' = freeTypeVars fs <> freeTypeVars t
--         scheme = tVars' :. fs :=> t
--       (fs', t') <- freshConstInst (tVars :. fs :=> t)
--       (fs'', t'') <- freshInst (tVars' :. fs' :=> t')
--       tVar' <- fresherVarType tVar
--       schemes %= Map.insertWith (<>) tVar (Set.singleton scheme) -- TODO: simplify the scheme
--       return . (True, ) $ tVar' `TypeUnion` t'' : fs''
--     NestedFact (tVars :. fs :=> name `ClassFact` handle) -> do
--       let scheme = tVars :. fs :=> handle
--       classSchemes %= Map.insertWith undefined name scheme
--       return (True, [])
--     NestedFact (tVars :. fs :=> name `ClassConstraint` handle) -> do
--       uses classSchemes (name `Map.lookup`) >>= \case
--         Nothing -> return (False, [fact])
--         Just scheme' -> do
--           let scheme = tVars :. fs :=> handle
--           (fs', handle') <- freshConstInst scheme
--           (fs'', handle'') <- freshInst scheme'
--           instanceSchemes %= Map.insertWith (<>) name (Set.singleton scheme)
--           return (True, handle'' `typeUnion` VarType handle' : fs' <> fs'')
--     NestedFact (_ :. fs :=> tVar `ClassUnion` t) -> do
--       let
--         tVars' = freeTypeVars fs <> freeTypeVars t
--         scheme = tVars' :. fs :=> t
--       schemes %= Map.insertWith (<>) tVar (Set.singleton scheme)
--       return (True, [])
--     NestedFact (tVars :. fs :=> tVar `InstanceUnion` t) -> do
--       let
--         tVars' = freeTypeVars fs <> freeTypeVars t
--       (fs', t') <- freshConstInst (tVars :. fs :=> t)
--       (fs'', t'') <- freshInst (tVars' :. fs' :=> t')
--       tVar' <- fresherVarType tVar
--       return . (True, ) $ tVar' `TypeUnion` t'' : fs''
--     _ ->  undefined -- error

-- reduceMany :: MonadInferencer m => Facts -> m (Facts, Facts, Facts)
-- reduceMany facts' = do
--   let (instFacts, facts'') =
--         partition
--           (\case
--              InstType {} -> True
--              _ -> False)
--           facts'
--   let (classConstraints, facts''') =
--         partition
--           (\case
--              ClassConstraint {} -> True
--              _ -> False)
--           facts''
--   (changes, newFacts) <- (_2 %~ concat) . unzip <$> traverse reduceOne facts'''
--   if or changes
--     then (_1 %~ (instFacts ++)) . (_2 %~ (classConstraints ++)) <$> reduceMany newFacts
--     else return (instFacts, classConstraints, newFacts)

-- reduce :: MonadInferencer m => Facts -> m (Facts, Facts, Facts)
-- reduce facts' = do
--   result@(instFacts, classConstraints, facts'') <- reduceMany facts'
--   (instFacts', facts''') <-
--     (_2 %~ concat) . partitionEithers <$> traverse goInst instFacts
--   (classConstraints', facts'''') <-
--     (_2 %~ concat) . partitionEithers <$> traverse goClass classConstraints
--   if null facts''' && null facts''''
--     then return result
--     else (_1 %~ (instFacts' ++)) . (_2 %~ (classConstraints' ++)) <$> reduce (facts'''' ++ facts''' ++ facts'')
--   where
--     goInst fact@(InstType gen inst) =
--       uses schemes (gen `Map.lookup`) >>= \case
--         Just schemes'
--           | null schemes' -> undefined -- error
--           | Set.size schemes' == 1 -> do
--             (fs, t) <- freshInst $ Set.findMin schemes'
--             return . Right $ inst `TypeUnion` t : fs
--           | otherwise -> return $ Left fact
--         Nothing -> undefined -- error
--     goInst _ = undefined -- impl error
--     goClass fact@(ClassConstraint name handle) =
--       uses instanceSchemes (name `Map.lookup`) >>= \case
--         Just schemes'
--           | null schemes' -> undefined -- error
--           | Set.size schemes' == 1 -> do
--             (fs', t) <- freshInst $ Set.findMin schemes'
--             return . Right $ handle `typeUnion` VarType t : fs'
--           | otherwise -> return $ Left fact
--         Nothing -> undefined -- error
--     goClass _ = undefined -- impl error

-- solve :: MonadInferencer m => Facts -> m ()
-- solve facts' = do
--   (instFacts, classConstraints, facts'') <- reduce facts'
--   use errors >>= \errs ->
--     if null errs
--       then if null instFacts
--         then if null classConstraints
--           then unless (null facts'') undefined
--           else solve'' ((\(ClassConstraint name handle) -> (name, handle)) <$> classConstraints) facts''
--         else solve'
--               ((\(InstType gen inst) -> (gen, inst)) <$> instFacts)
--               (classConstraints <> facts'')
--       else undefined -- error

-- solve' :: MonadInferencer m => [(TypeVar, TypeVar)] -> Facts -> m ()
-- solve' instFacts facts' = do
--   schemes' <-
--     (Set.toList <$>) <$> uses schemes (Map.!) <&> (<$> ((^. _1) <$> instFacts))
--   let tVars = (^. _2) <$> instFacts
--   go schemes' tVars >>= \case
--     Just facts'' -> solve (facts'' ++ facts')
--     Nothing -> return ()
--   where
--     go ((scheme:choices):schemes'') (tVar:tVars') = do
--       state <- get
--       (fs', t) <- freshInst scheme
--       result <-
--         go schemes'' tVars' >>= \case
--           Just fs'' -> do
--             noErrors <- uses errors null
--             if noErrors
--               then return . Just $ tVar `TypeUnion` t : fs' ++ fs''
--               else return Nothing
--           _ -> return Nothing
--       case result of
--         Just {} -> return result -- no errors encountered
--         _ ->
--           if null choices
--             then return Nothing -- all branches failed
--                       -- try another branch
--             else do
--               put state -- rollback
--               go (choices : schemes'') (tVar : tVars')
--     go [] [] = return $ Just []
--     go _ [] = error "implementation error" -- TODO: make this nicer
--     go [] _ = error "implementation error" -- TODO: make this nicer
--     go ([]:_) _ = error "implementation error"

-- solve'' :: MonadInferencer m => [(Text, TypeVar)] -> Facts -> m ()
-- solve'' instFacts facts' = do
--   schemes' <-
--     (Set.toList <$>) <$> uses instanceSchemes (Map.!) <&> (<$> ((^. _1) <$> instFacts))
--   let tVars = (^. _2) <$> instFacts
--   go schemes' tVars >>= \case
--     Just facts'' -> solve (facts'' ++ facts')
--     Nothing -> return ()
--   where
--     go ((scheme:choices):schemes'') (tVar:tVars') = do
--       state <- get
--       (fs', t) <- freshInst scheme
--       result <-
--         go schemes'' tVars' >>= \case
--           Just fs'' -> do
--             noErrors <- uses errors null
--             if noErrors
--               then return . Just $ tVar `TypeUnion` VarType t : fs' ++ fs''
--               else return Nothing
--           _ -> return Nothing
--       case result of
--         Just {} -> return result -- no errors encountered
--         _ ->
--           if null choices
--             then return Nothing -- all branches failed
--                       -- try another branch
--             else do
--               put state -- rollback
--               go (choices : schemes'') (tVar : tVars')
--     go [] [] = return $ Just []
--     go _ [] = error "implementation error" -- TODO: make this nicer
--     go [] _ = error "implementation error" -- TODO: make this nicer
--     go ([]:_) _ = error "implementation error"

data Way
  = Forward
  | Both

collectPairs ::
     MonadInferencer m
  => Getting (Map TypeVar (Set TypeVar)) Inferencer (Map TypeVar (Set TypeVar))
  -> Way
  -> m [(TypeVar, TypeVar)]
collectPairs from way = do
  from' <- Map.toList <$> use from
  handles <- use handleCounter
  let edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> from')
      varMap =
        Map.fromList $ (\tVar -> (varId tVar, tVar)) <$>
        uncurry (<>) (unzip edges)
      graph =
        Graph.buildG (1, handles) $ (both %~ varId) <$> edges
      vs = Map.keys varMap
      pairs = case way of
        Forward -> [(v, v') | v <- vs, v' <- vs, v /= v', Graph.path graph v v']
        Both -> [(v, v') | v <- vs, v' <- vs, v < v', Graph.path graph v v', Graph.path graph v' v]
  return $ pairs <&> both %~ \i -> varMap Map.! i

deduceUnifs :: MonadInferencer m =>
  Getting
    (Map TypeVar (Set TypeVar)) Inferencer (Map TypeVar (Set TypeVar))
  -> m (Map TypeVar TypeVar)
deduceUnifs which = do
  pairs <- collectPairs which Both
  let
    go [] subst = return subst
    go ((tVar, tVar'):others) subst = do
      case apply subst tVar `unify` apply subst tVar' of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go others $ subst' `apply` subst
  go pairs mempty

deduceKindUnifs :: MonadInferencer m => m (Subst TypeVar)
deduceKindUnifs = deduceUnifs subKinding

deduceConstUnifs :: MonadInferencer m => m (Subst TypeVar)
deduceConstUnifs = deduceUnifs subConsting

deduceKinds :: MonadInferencer m => m ()
deduceKinds = do
  pairs <- collectPairs subKinding Forward
  for_ pairs $ \(v, v') -> do
    uses kindingBounds (v' `Map.lookup`) >>=
      traverse_
        ((kindingBounds %=) . Map.insertWith (<>) v . (upperBound .~ maxBound))
    uses kindingBounds (v `Map.lookup`) >>=
      traverse_
        ((kindingBounds %=) . Map.insertWith (<>) v' . (lowerBound .~ minBound))

deduceConsts :: MonadInferencer m => m ()
deduceConsts = do
  pairs <- collectPairs subConsting Forward
  for_ pairs $ \(v, v') -> do
    uses constingBounds (v' `Map.lookup`) >>=
      traverse_
        ((constingBounds %=) . Map.insertWith (<>) v . (lowerBound .~ minBound))
    uses constingBounds (v `Map.lookup`) >>=
      traverse_
        ((constingBounds %=) . Map.insertWith (<>) v' . (upperBound .~ maxBound))

registerKind :: MonadInferencer m => Text -> m DataKind
registerKind = undefined

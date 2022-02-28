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
import safe Prelude hiding (const)

import safe CMM.Data.Bounds
import safe CMM.Data.Ordered
import safe CMM.Data.OrderedBounds()
import safe CMM.Inference.State
import safe CMM.Inference.Type
import qualified Data.Bimap as Bimap
import CMM.Inference.TypeHandle (initTypeHandle, TypeHandle, kinding, consting, typing, handleId)
import Data.Tuple (swap)
import Control.Lens (Lens')
import CMM.Data.Lattice
import Data.PartialOrd (PartialOrd)
import CMM.Inference.Preprocess.State (HasTypeHandle (getTypeHandle))

class Unify a b | a -> b where
  unify :: a -> a -> Either [UnificationError] (Subst b, a)

class (ToType b, Typeable b, Data a, TypeCase b) =>
      Apply a b
  where
  apply :: Map TypeVar b -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCase subst go

class TypeCase b where
  typeCase :: Subst b -> (forall d . Data d => d -> d) -> b -> b

instance TypeCase Type where
  typeCase subst go =
    \case
      t@(VarType tVar) -> maybe t toType $ tVar `Map.lookup` subst
      t -> gmapT go t

instance TypeCase TypeVar where
  typeCase subst _ =
    \case
      tVar -> fromMaybe tVar $ tVar `Map.lookup` subst

instance (ToType b, Typeable b, TypeCase b) => Apply TypeVar b

instance (ToType b, Typeable b, TypeCase b) => Apply Type b

instance (ToType b, Typeable b, TypeCase b) => Apply Fact b

instance (ToType b, Typeable b, TypeCase b) => Apply PrimType b

instance Apply b b => Apply (Map TypeVar b) b where
  subst' `apply` subst
    | null subst' = subst
    | otherwise = (apply subst' <$> subst) <> subst'

instance (Apply a t, Apply b t) => Apply (a, b) t where
  subst `apply` (a, b) = (subst `apply` a, subst `apply` b)

instance (Apply TypeVar t, ToType t, Typeable t, TypeCase t) => Apply TypeHandle t where
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

unifyMany :: (Unify a b1, Apply a b2, Apply (Map TypeVar b2) b1) =>
  [UnificationError]
  -> [a] -> [a] -> Either [UnificationError] (Map TypeVar b2, [a])
unifyMany msg  = go mempty
  where
    go subst (x:xs) (y:ys) = do
      (subst', z) <- apply subst x `unify` apply subst y
      (_2 %~ (z:)) <$> go (subst' `apply` subst) xs ys
    go subst [] [] = pure (subst, [])
    go _ _ _ = Left msg

unifyCompl :: (Apply b b, Apply a b, Unify a b, Eq a, ToType a) =>
  TypeCompl a
  -> TypeCompl a
  -> Either [UnificationError] (Map TypeVar b, TypeCompl a)
unifyCompl t t' = case (t, t') of
  (TupleType ts, TupleType ts') ->
    (_2 %~ TupleType) <$> unifyMany msg ts ts'
  (FunctionType args ret, FunctionType args' ret') -> do
    (subst, args'') <- unifyMany msg args args'
    (subst', ret'') <- apply subst ret `unify` apply subst ret'
    return (subst' `apply` subst, FunctionType args'' ret'')
  (AppType app arg, AppType app' arg') -> do
    (subst, app'') <- app `unify` app'
    (subst', arg'') <- apply subst arg `unify` apply subst arg'
    return (subst' `apply` subst, AppType app'' arg'')
  (AddrType addr, AddrType addr') -> (_2 %~ AddrType) <$> unify addr addr'
  _
    | t == t' -> return (mempty, t)
    | otherwise -> Left msg
  where msg = [toType t `unifyMismatch` toType t']


instance Unify PrimType TypeVar where
  unify = unifyCompl

instance Unify Type Type where
  unify (ErrorType text) (ErrorType text') =
    Left [GotErrorType text, GotErrorType text']
  unify (ErrorType text) _ = Left [GotErrorType text]
  unify _ (ErrorType text) = Left [GotErrorType text]
  unify t t'
    | t == t' = Right (mempty, t)
  unify (VarType tVar) t' = bind tVar t'
  unify t (VarType tVar') = bind tVar' t
  unify (ComplType t) (ComplType t') = (_2 %~ ComplType) <$> unifyCompl t t'

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

instance {-# OVERLAPPING #-} FactCheck Fact where
  factCheck fact =
    case fact of
      Fact f -> factCheck f
      NestedFact (tVars :. facts :=> fact') -> factCheck tVars *> factCheck facts *> factCheck fact'

instance FactCheck TypeVar where
  factCheck tVar = do
    handlize %= Bimap.tryInsert tVar (initTypeHandle NoTypeAnnot tVar)

handlizeTVar :: MonadInferencer m => TypeVar -> m TypeVar
handlizeTVar tVar = do
  handlize %= Bimap.insert tVar (initTypeHandle NoTypeAnnot tVar)
  return tVar


freshTypeHelperWithHandle :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHelperWithHandle kind = freshTypeHelper kind >>= handlizeTVar

elaborate :: MonadInferencer m => PrimType -> m (TypeCompl Type)
elaborate primType = do
  handles <- use handlize
  let getTyping t = maybe (VarType t) (^. typing) (t `Bimap.lookup` handles)
  return $ getTyping <$> primType

simplify :: MonadInferencer m => Type -> m TypeVar
simplify = \case
  ErrorType _ -> undefined
  VarType tVar -> do
    uses handlize (tVar `Bimap.lookup`) >>= \case
      Just _ -> return tVar
      Nothing -> handlizeTVar tVar
  ComplType complType -> traverse simplify complType >>= \primType ->
    uses typize (primType `Bimap.lookupR`) >>= \case
      Nothing -> do
        tVar <- freshTypeHelperWithHandle Star
        typize %= Bimap.insert tVar primType
        t <- elaborate primType
        handlize %= Bimap.adjust (typing .~ ComplType t)  tVar
        return tVar
      Just tVar -> return tVar

getHandle :: MonadInferencer m => TypeVar -> m TypeHandle
getHandle tVar = uses handlize (fromJust . Bimap.lookup tVar)

infix 6 `insertEdge`
insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

pushSubKind :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushSubKind handle handle' = subKinding %= view kinding handle' `insertEdge` view kinding handle

pushSubConst :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushSubConst handle handle' = subConsting %= view consting handle' `insertEdge` view consting handle

pushKindBounds :: MonadInferencer m => TypeHandle -> Bounds DataKind -> m ()
pushKindBounds handle bounds = kindingBounds %= Map.insertWith (<>) (handle ^. kinding) bounds

pushConstBounds :: MonadInferencer m => TypeHandle -> Bounds Constness -> m ()
pushConstBounds handle bounds = constingBounds %= Map.insertWith (<>) (handle ^. consting) bounds

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

mineAST :: (MonadInferencer m, HasTypeHandle a, Foldable n) => n a -> m ()
mineAST = traverse_ (addHandle . getTypeHandle)
  where
    addHandle handle = handlize %= Bimap.insert (handleId handle) handle

normalizeFacts :: Facts -> Facts
normalizeFacts [] = []
normalizeFacts (fact@Fact{} : others) = fact : normalizeFacts others
normalizeFacts (NestedFact (tVars :. from :=> facts) : others) =
  makeFacts (Set.toList tVars) ++ NestedFact (Set.fromList [] :. from :=> normalizeFacts facts) : normalizeFacts others
  where
    makeFacts [] = []
    makeFacts (tVar':rest) = Fact (tVar' `typeUnion` ComplType (toLam tVar')) : makeFacts rest

fixAll :: MonadInferencer m => m ()
fixAll = do
  results <- sequence [fixTypize, fixHandlize, fixSubs]
  when (or results) fixAll

mapFold :: (Ord a, Semigroup b) => [(a,b)] -> Map a b
mapFold = foldl (flip . uncurry $ Map.insertWith (<>)) Map.empty

fixSubGraph :: MonadInferencer m => Lens' Inferencer (Map TypeVar (Set TypeVar)) -> Subst TypeVar -> m ()
fixSubGraph which subst = do
  let applyUnifs = (_1 %~ apply subst) . (_2 %~ Set.fromList . (apply subst <$>) . Set.toList)
  whichList <- uses which $ (applyUnifs <$>) . Map.toList
  let which' = mapFold whichList
  which .= Map.mapWithKey Set.delete which'

fixBounds :: (MonadInferencer m, Lattice a) => Lens' Inferencer (Map TypeVar (Bounds a)) -> Subst TypeVar -> m ()
fixBounds which subst = do
  whichList <- uses which $ ((_1 %~ apply subst) <$>) . Map.toList
  which .= mapFold whichList

fixSubs :: MonadInferencer m => m Bool
fixSubs = do
  unifs' <- use unifs
  let
    fixBoth :: (MonadInferencer m, Bounded a, Eq a, Ord (Ordered a), Lattice a) => Lens' Inferencer (Map TypeVar (Set TypeVar)) -> Lens' Inferencer (Map TypeVar (Bounds a)) -> m (Subst TypeVar)
    fixBoth sub bounds = do
      fixSubGraph sub unifs'
      subst <- subUnifs
      fixSubGraph sub subst
      fixBounds bounds $ subst `apply` unifs'
      propagateBounds bounds sub
      boundsUnifs bounds >>= go subst
      where
        subUnifs = liftA2 deduceUnifs (use handleCounter) (use sub)
        go accum subst
          | null subst = return accum
          | otherwise = do
            fixSubGraph sub subst
            subst' <- subUnifs
            fixSubGraph sub subst'
            let subst'' = subst' `apply` subst
            fixBounds bounds subst''
            propagateBounds bounds sub
            boundsUnifs bounds >>= go (subst'' `apply` accum)

  subst <- fixBoth subKinding kindingBounds
  subst' <- fixBoth subConsting constingBounds

  if null subst && null subst'
    then return False
    else safeHandlizeUpdate $ (_2 . kinding %~ apply subst) . (_2 . consting %~ apply subst')

-- TODO: reduce duplication
safeHandlizeUpdate :: MonadInferencer m =>
  ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle)) -> m Bool
safeHandlizeUpdate change = do
  handles' <- uses handlize ((change <$>) . Bimap.toList)
  let
    handlize' = Bimap.fromList handles'
    forgottenKeys = Map.fromList handles' `Map.withoutKeys` Set.fromList (Bimap.keys handlize')
    forgottenValues = Map.fromList (swap <$> handles') `Map.withoutKeys` Set.fromList (Bimap.elems handlize')

    goValues [] tSubst cSubst kSubst = return (tSubst, cSubst, kSubst)
    goValues ((handle, tVar):others) tSubst cSubst kSubst = do
      let handle' = handlize' Bimap.! tVar
      (tSubst', _) <- apply tSubst (handle ^. typing) `unify` apply tSubst (handle' ^. typing)
      (cSubst', _) <- apply cSubst (handle ^. consting) `unify` apply cSubst (handle' ^. consting)
      (kSubst', _) <- apply kSubst (handle ^. kinding) `unify` apply kSubst (handle' ^. kinding)
      goValues others (tSubst' `apply` tSubst) (cSubst' `apply` cSubst) (kSubst' `apply` kSubst)

    goKeys [] subst = return subst
    goKeys ((tVar, handle):others) subst = do
      let tVar' = handlize' Bimap.!> handle
      (subst', _) <- apply subst tVar `unify` apply subst tVar'
      goKeys others $ subst' `apply` subst

  handlize .= handlize'
  if null forgottenKeys && null forgottenValues
    then return False
    else do
      let
        performForgotten = do
          (tSubst, cSubst, kSubst) <- goValues (Map.toList forgottenValues) (mempty :: Map TypeVar Type) (mempty :: Map TypeVar TypeVar) (mempty :: Map TypeVar TypeVar)
          subst <- goKeys (Map.toList forgottenKeys) (mempty :: Map TypeVar TypeVar)
          return (tSubst, cSubst, kSubst, subst)
      case performForgotten of
        Left errs -> do
          errors <>= errs
          return False -- TODO: error
        Right (tSubst, cSubst, kSubst, subst) -> do
          unifs %= apply subst
          fixSubGraph subConsting cSubst
          fixSubGraph subKinding kSubst
          fixBounds constingBounds cSubst
          fixBounds kindingBounds kSubst
          (True <$) . safeHandlizeUpdate $ apply subst . (_2 . typing %~ apply tSubst) . (_2 . consting %~ apply cSubst) . (_2 . kinding %~ apply kSubst)

fixHandlize :: MonadInferencer m => m Bool
fixHandlize = uses unifs apply >>= safeHandlizeUpdate

fixTypize :: MonadInferencer m => m Bool
fixTypize = do
  types' <- uses unifs (fmap . apply) <*> uses typize Bimap.toList
  let
    typize' = Bimap.fromList types'

    forgottenKeys = Map.fromList types' `Map.withoutKeys` Set.fromList (Bimap.keys typize')
    forgottenValues = Map.fromList (swap <$> types') `Map.withoutKeys` Set.fromList (Bimap.elems typize')

    -- TODO: reduce duplication; consider continuing despite errors
    goValues [] subst = return subst
    goValues ((primType, tVar):others) subst = do
      let primType' = fromJust $ tVar `Bimap.lookup` typize'
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
fixFacts facts = uses unifs $ flip fmap facts . apply

reduceOne :: MonadInferencer m => Facts -> m (Bool, Facts)
reduceOne [ ] = fixAll $> (False, [])
reduceOne (fact:facts) =
  case fact of
    Fact (SubType t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      let facts' = Fact (tVar `subKind` tVar') : Fact (tVar `typeConstraint` tVar') : Fact (tVar `subConst` tVar') : facts
      (_1 .~ True) <$> reduceOne facts'
    Fact (SubKind t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushSubKind handle handle'
      (_1 .~ True) <$> reduceOne facts
    Fact (SubConst t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushSubConst handle handle'
      (_1 .~ True) <$> reduceOne facts
    Fact (Typing t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushTyping handle handle'
      (_1 .~ True) <$> (fixFacts facts >>= reduceOne)
    Fact (Union t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      case tVar `unify` tVar' of
        Left errs -> errors <>= errs
        Right (subst, _) -> unifs %= (subst `apply`)
      fixAll
      (_1 .~ True) <$> (fixFacts facts >>= reduceOne)
    Fact (ConstnessBounds bounds t) -> do
      handle <- simplify t >>= getHandle
      pushConstBounds handle bounds
      (_1 .~ True) <$> reduceOne facts
    Fact (KindBounds (Bounds (Ordered minKind) (Ordered maxKind)) t) -> do
      handle <- simplify t >>= getHandle
      pushKindBounds handle $ minKind `Bounds` maxKind
      (_1 .~ True) <$> reduceOne facts
    Fact (OnRegister reg t) -> do
      handle <- simplify t >>= getHandle
      kind <- registerKind reg
      pushKindBounds handle $ kind `Bounds` kind
      (_1 .~ True) <$> reduceOne facts
    NestedFact (tVars :. facts' :=> nesteds) -> do
      (changed, nesteds') <- reduceOne nesteds
      (_1 %~ (|| changed)) . (_2 %~ (NestedFact (tVars :. facts' :=> nesteds'):)) <$> reduceOne facts
    Fact _ -> (_2 %~ (fact:)) <$> reduceOne facts
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

reduceMany :: MonadInferencer m => Facts -> m (Bool, Facts)
reduceMany facts = do
  (change, facts') <- reduceOne facts
  if change
    then (_1 .~ True) <$> reduceMany facts'
    else return (False, facts)

reduce :: MonadInferencer m => Facts -> m (Bool, Facts)
reduce facts = do
  (change, facts') <- reduceMany facts
  let (change', facts'') = floatFacts facts' $ collectCounts facts' mempty
  if change || change'
    then (_1 .~ True) <$> reduce facts''
    else return (False, facts'')

floatFacts :: Facts -> Subst Int -> (Bool, Facts)
floatFacts [] _ = (False, [])
floatFacts (fact:facts) counts = go $ floatFacts facts counts
  where
    go = case fact of
      NestedFact (_ :. [Union (VarType tVar) t] :=> fs)
        | counts Map.! tVar == 1 -> (_1 .~ True) . (_2 %~ (newFacts <>))
        where
          newFacts = Fact (Union (VarType tVar) t) : fs
      _ -> _2 %~ (fact:)

collectCounts :: Facts -> Subst Int -> Subst Int
collectCounts [] counts = counts
collectCounts (fact:facts) counts = do
  case fact of
    NestedFact (_ :. [Union (VarType tVar) _] :=> _) -> do
      collectCounts facts (Map.insertWith (+) tVar 1 counts)
    _ -> collectCounts facts counts

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

collectPairs :: Way
  -> Int
  -> Map TypeVar (Set TypeVar)
  -> [(TypeVar, TypeVar)]
collectPairs way handles from =
  pairs <&> both %~ \i -> varMap Map.! i
  where
    edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> Map.toList from)
    varMap =
      Map.fromList $ (\tVar -> (varId tVar, tVar)) <$>
      uncurry (<>) (unzip edges)
    graph =
      Graph.buildG (1, handles) $ (both %~ varId) <$> edges
    vs = Map.keys varMap
    pairs = case way of
      Forward -> [(v, v') | v <- vs, v' <- vs, v /= v', Graph.path graph v v']
      Both -> [(v, v') | v <- vs, v' <- vs, v < v', Graph.path graph v v', Graph.path graph v' v]

deduceUnifs :: Int
  -> Map TypeVar (Set TypeVar)
  -> Map TypeVar TypeVar
deduceUnifs handles which = go pairs mempty
  where
    pairs = collectPairs Both handles which
    go [] subst = subst
    go ((tVar, tVar'):others) subst =
      case apply subst tVar `unify` apply subst tVar' of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go others $ subst' `apply` subst

propagateBounds :: (MonadInferencer m, Lattice a, Bounded a) => Lens' Inferencer (Map TypeVar (Bounds a)) -> Getter Inferencer (Map TypeVar (Set TypeVar)) -> m ()
propagateBounds which by = do
  pairs <- liftA2 (collectPairs Forward) (use handleCounter) (use by)
  for_ pairs $ \(v, v') -> do
    uses which (v' `Map.lookup`) >>=
      traverse_
        ((which %=) . Map.insertWith (<>) v . (lowerBound .~ minBound))
    uses which (v `Map.lookup`) >>=
      traverse_
        ((which %=) . Map.insertWith (<>) v' . (upperBound .~ maxBound))

boundsUnifs :: (MonadInferencer m, PartialOrd a, Eq a, Ord (Ordered a), Bounded a) => Getter Inferencer (Map TypeVar (Bounds a)) -> m (Subst TypeVar)
boundsUnifs which = do
  whichList <- filter (isTrivial . view _2) <$> uses which Map.toList
  let trivialGroups = mapFold $ (_2 %~ Set.singleton) . (_1 %~ Ordered . normalizeAbsurd) . swap <$> whichList
  let nontrivialTrivialGroups = (Set.toList <$>) . Map.elems $ Map.filter ((> 1) . Set.size) trivialGroups
  return $ go nontrivialTrivialGroups mempty
  where
    go ((first:second:others):rest) subst =
      case apply subst first `unify` apply subst second of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go ((first:others):rest) $ subst' `apply` subst
    go (_:rest) subst = go rest subst
    go [] subst = subst

registerKind :: MonadInferencer m => Text -> m DataKind
registerKind = undefined

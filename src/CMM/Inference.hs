{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

-- TODO: add the overlap check for instances
-- TODO: add the overload resolution for instances to monomorphization
module CMM.Inference where

import safe Control.Applicative (Applicative(liftA2))
import safe Control.Lens (Lens')
import safe Control.Lens.Getter (Getter, (^.), use, uses, view)
import safe Control.Lens.Setter ((%=), (%~), (.=), (.~), (<>=))
import safe Control.Lens.Traversal (both)
import safe Control.Lens.Tuple (Field1(_1), Field2(_2))
import safe Control.Monad.State.Lazy (MonadState, void, when)
import qualified Data.Bimap as Bimap
import safe Data.Data (Data(gmapT), Typeable)
import safe Data.Foldable (Foldable(fold), for_, traverse_)
import safe Data.Functor (($>), (<&>))
import safe Data.Generics.Aliases (extT)
import safe Data.Graph (SCC(AcyclicSCC, CyclicSCC), stronglyConnCompR)
import safe qualified Data.Graph as Graph
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromJust, fromMaybe)
import safe Data.PartialOrd (PartialOrd)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Tuple (swap)
import safe Prelude hiding (const)

import safe CMM.Data.Bounds
  ( Bounds(Bounds)
  , isTrivial
  , lowerBound
  , normalizeAbsurd
  , upperBound
  )
import safe CMM.Data.Lattice (Lattice)
import CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Data.Ordered (Ordered(Ordered))
import safe CMM.Data.OrderedBounds ()
import safe CMM.Inference.Preprocess.State (HasTypeHandle(getTypeHandle))
import safe CMM.Inference.State
  ( Inferencer
  , MonadInferencer
  , Subst
  , UnificationError(BadKind, GotErrorType, Mismatch, Occurs)
  , constingBounds
  , errors
  , freshTypeHelper
  , getParent
  , handleCounter
  , handlize
  , kindingBounds
  , nextHandleCounter
  , popParent
  , pushParent
  , schemes
  , subConsting
  , subKinding
  , typize
  , unifs, classSchemes, classFacts
  )
import safe CMM.Inference.Type
  ( Constness
  , DataKind
  , Fact
  , Facts
  , FlatFact(ConstnessBounds, InstType, KindBounds, OnRegister,
         SubConst, SubKind, SubType, Typing, Union, ClassConstraint, ClassFact, ClassDetermine)
  , FlatFacts
  , HasTypeKind(..)
  , IsTyped(freeTypeVars)
  , NestedFact(Fact, NestedFact)
  , PrimType
  , Qual((:=>))
  , Scheme((:.))
  , ToType(..)
  , Type(..)
  , TypeAnnot(NoTypeAnnot)
  , TypeCompl(AddrType, AppType, FunctionType, TupleType)
  , TypeKind(..)
  , TypeVar(NoType, TypeVar, tVarParent)
  , familyDepth
  , predecessor
  , subConst
  , subKind
  , tVarId
  , typeConstraint
  , typeUnion
  )
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , consting
  , handleId
  , initTypeHandle
  , kinding
  , typing
  )

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
  typeCase ::
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

instance (ToType b, Typeable b, TypeCase b) => Apply TypeVar b

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
    (typing %~ apply subst) . (consting %~ apply subst) .
    (kinding %~ apply subst)

foldTVarSubsts :: [Map TypeVar TypeVar] -> Map TypeVar TypeVar
foldTVarSubsts = foldr apply mempty

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
  unify tVar@TypeVar {} tVar'@TypeVar {}
    | tVar == tVar' = Right (mempty, tVar')
    | not (matchKind tVar tVar') = Left [BadKind (VarType tVar) (VarType tVar')]
    | familyDepth tVar > familyDepth tVar' =
      Right (Map.singleton tVar tVar', tVar')
    | familyDepth tVar < familyDepth tVar' =
      Right (Map.singleton tVar' tVar, tVar)
    | tVar > tVar' = Right (Map.singleton tVar tVar', tVar')
    | otherwise = Right (Map.singleton tVar' tVar, tVar)
  unify tVar tVar' = Left [toType tVar `unifyMismatch` toType tVar']

unifyMany ::
     (Unify a b1, Apply a b2, Apply (Map TypeVar b2) b1)
  => [UnificationError]
  -> [a]
  -> [a]
  -> Either [UnificationError] (Map TypeVar b2, [a])
unifyMany msg = go mempty
  where
    go subst (x:xs) (y:ys) = do
      (subst', z) <- apply subst x `unify` apply subst y
      (_2 %~ (z :)) <$> go (subst' `apply` subst) xs ys
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
      return (subst' `apply` subst, FunctionType args'' ret'')
    (AppType app arg, AppType app' arg') -> do
      (subst, app'') <- app `unify` app'
      (subst', arg'') <- apply subst arg `unify` apply subst arg'
      return (subst' `apply` subst, AppType app'' arg'')
    (AddrType addr, AddrType addr') -> (_2 %~ AddrType) <$> unify addr addr'
    _
      | t == t' -> return (mempty, t)
      | otherwise -> Left msg
  where
    msg = [toType t `unifyMismatch` toType t']

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
      NestedFact (tVars :. facts :=> fact') ->
        factCheck tVars *> factCheck facts *> factCheck fact'

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
  let oneLevels t = maybe (VarType t) (view typing) (t `Bimap.lookup` handles)
  return $ oneLevels <$> primType

simplify :: MonadInferencer m => Type -> m TypeVar
simplify =
  \case
    ErrorType _ -> undefined
    VarType tVar -> do
      uses handlize (tVar `Bimap.lookup`) >>= \case
        Just _ -> return tVar
        Nothing -> handlizeTVar tVar
    ComplType complType ->
      traverse simplify complType >>= \primType ->
        uses typize (primType `Bimap.lookupR`) >>= \case
          Nothing -> do
            tVar <- freshTypeHelperWithHandle $ getTypeKind primType
            typize %= Bimap.insert tVar primType
            t <- elaborate primType
            handlize %= Bimap.adjust (typing .~ ComplType t) tVar
            return tVar
          Just tVar -> return tVar

getHandle :: MonadInferencer m => TypeVar -> m TypeHandle
getHandle tVar = uses handlize (fromJust . Bimap.lookup tVar)

infix 6 `insertEdge`

insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

pushSubKind :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushSubKind handle handle' =
  subKinding %= view kinding handle `insertEdge` view kinding handle'

pushSubConst :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushSubConst handle handle' =
  subConsting %= view consting handle `insertEdge` view consting handle'

pushKindBounds :: MonadInferencer m => TypeHandle -> Bounds DataKind -> m ()
pushKindBounds handle bounds =
  kindingBounds %= Map.insertWith (<>) (handle ^. kinding) bounds

pushConstBounds :: MonadInferencer m => TypeHandle -> Bounds Constness -> m ()
pushConstBounds handle bounds =
  constingBounds %= Map.insertWith (<>) (handle ^. consting) bounds

doFor :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
doFor a b = a >>= traverse b

doFor_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
doFor_ a b = a >>= traverse_ b

pushTyping :: MonadInferencer m => TypeHandle -> TypeHandle -> m ()
pushTyping handle handle' = do
  let t = handle ^. typing
      t' = handle' ^. typing
  case unify t t' of
    Left errs -> errors <>= errs
    Right (subst, _) -> do
      let fixIt = do
            fixTypize >>= \case
              True ->
                fixHandlize >>= \case
                  True -> fixIt
                  False -> fixSubs
              False -> fixSubs
      safeHandlizeUpdate (_2 . typing %~ apply subst) >>= flip when (void fixIt)

mineAST :: (MonadInferencer m, HasTypeHandle a, Foldable n) => n a -> m ()
mineAST = traverse_ (addHandle . getTypeHandle)
  where
    addHandle handle = handlize %= Bimap.insert (handleId handle) handle

fixAll :: MonadInferencer m => m Bool
fixAll = do
  results <- sequence [fixTypize, fixHandlize, fixSubs]
  if or results
    then True <$ fixAll
    else return False

mapFold :: (Ord a, Semigroup b) => [(a, b)] -> Map a b
mapFold = foldl (flip . uncurry $ Map.insertWith (<>)) Map.empty

fixSubGraph ::
     MonadInferencer m
  => Lens' Inferencer (Map TypeVar (Set TypeVar))
  -> Subst TypeVar
  -> m ()
fixSubGraph which subst = do
  let applyUnifs =
        (_1 %~ apply subst) .
        (_2 %~ Set.fromList . (apply subst <$>) . Set.toList)
  whichList <- uses which $ (applyUnifs <$>) . Map.toList
  let which' = mapFold whichList
  which .= Map.filter (not . null) (Map.mapWithKey Set.delete which')

fixBounds ::
     (MonadInferencer m, Lattice a)
  => Lens' Inferencer (Map TypeVar (Bounds a))
  -> Subst TypeVar
  -> m ()
fixBounds which subst = do
  whichList <- uses which $ ((_1 %~ apply subst) <$>) . Map.toList
  which .= mapFold whichList

fixSubs :: MonadInferencer m => m Bool
fixSubs = do
  unifs' <- use unifs
  let fixBoth ::
           (MonadInferencer m, Bounded a, Eq a, Ord (Ordered a), Lattice a)
        => Lens' Inferencer (Map TypeVar (Set TypeVar))
        -> Lens' Inferencer (Map TypeVar (Bounds a))
        -> m (Subst TypeVar)
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
    else safeHandlizeUpdate $ (_2 . kinding %~ apply subst) .
         (_2 . consting %~ apply subst')

unsafeTypizeUpdate ::
     MonadInferencer m => ((TypeVar, PrimType) -> (TypeVar, PrimType)) -> m ()
unsafeTypizeUpdate change =
  typize %= Bimap.fromList . (change <$>) . Bimap.toList

unsafeHandlizeUpdate ::
     MonadInferencer m
  => ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle))
  -> m ()
unsafeHandlizeUpdate change =
  handlize %= Bimap.fromList . (change <$>) . Bimap.toList

-- TODO: reduce duplication
safeHandlizeUpdate ::
     MonadInferencer m
  => ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle))
  -> m Bool
safeHandlizeUpdate change = do
  handles' <- uses handlize ((change <$>) . Bimap.toList)
  let handlize' = Bimap.fromList handles'
      forgottenKeys =
        Map.fromList handles' `Map.withoutKeys`
        Set.fromList (Bimap.keys handlize')
      forgottenValues =
        Map.fromList (swap <$> handles') `Map.withoutKeys`
        Set.fromList (Bimap.elems handlize')
      goValues [] tSubst cSubst kSubst = return (tSubst, cSubst, kSubst)
      goValues ((handle, tVar):others) tSubst cSubst kSubst = do
        let handle' = handlize' Bimap.! tVar
        (tSubst', _) <-
          apply tSubst (handle ^. typing) `unify`
          apply tSubst (handle' ^. typing)
        (cSubst', _) <-
          apply cSubst (handle ^. consting) `unify`
          apply cSubst (handle' ^. consting)
        (kSubst', _) <-
          apply kSubst (handle ^. kinding) `unify`
          apply kSubst (handle' ^. kinding)
        goValues
          others
          (tSubst' `apply` tSubst)
          (cSubst' `apply` cSubst)
          (kSubst' `apply` kSubst)
      goKeys [] subst = return subst
      goKeys ((tVar, handle):others) subst = do
        let tVar' = handlize' Bimap.!> handle
        (subst', _) <- apply subst tVar `unify` apply subst tVar'
        goKeys others $ subst' `apply` subst
  handlize .= handlize'
  if null forgottenKeys && null forgottenValues
    then return False
    else do
      let performForgotten = do
            (tSubst, cSubst, kSubst) <-
              goValues
                (Map.toList forgottenValues)
                (mempty :: Map TypeVar Type)
                (mempty :: Map TypeVar TypeVar)
                (mempty :: Map TypeVar TypeVar)
            subst <-
              goKeys (Map.toList forgottenKeys) (mempty :: Map TypeVar TypeVar)
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
          (True <$) . safeHandlizeUpdate $ apply subst .
            (_2 . typing %~ apply tSubst) .
            (_2 . consting %~ apply cSubst) .
            (_2 . kinding %~ apply kSubst)

fixHandlize :: MonadInferencer m => m Bool
fixHandlize = uses unifs apply >>= safeHandlizeUpdate

fixTypize :: MonadInferencer m => m Bool
fixTypize = do
  types' <- uses unifs (fmap . apply) <*> uses typize Bimap.toList
  let typize' = Bimap.fromList types'
      forgottenKeys =
        Map.fromList types' `Map.withoutKeys` Set.fromList (Bimap.keys typize')
      forgottenValues =
        Map.fromList (swap <$> types') `Map.withoutKeys`
        Set.fromList (Bimap.elems typize')
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
      subst <-
        goValues (Map.toList forgottenValues) (mempty :: Subst TypeVar) >>=
        goKeys (Map.toList forgottenKeys)
      unifs %= apply subst
      True <$ fixTypize

fixFacts :: MonadInferencer m => Facts -> m Facts
fixFacts facts = uses unifs $ flip fmap facts . apply

wrapParent :: MonadInferencer m => FlatFacts -> m a -> m a
wrapParent flatFacts wrapped =
  case determineParent flatFacts of
    Just parent -> pushParent parent *> wrapped <* popParent
    Nothing -> wrapped
  where
    determineParent [VarType tVar `Union` _] = Just tVar
    determineParent _ = undefined

flattenFacts :: [NestedFact Type] -> FlatFacts
flattenFacts = fmap go
  where
    go (Fact fact) = fact
    go _ = undefined -- TODO: error

reverseFacts :: FlatFacts -> FlatFacts
reverseFacts = fmap go
  where
    go (ClassFact name t) = ClassConstraint name t
    go _ = undefined -- TODO: error

reduceOne :: MonadInferencer m => Facts -> m (Bool, Facts)
reduceOne [] = fixAll $> (False, [])
reduceOne (fact:facts) =
  case fact of
    Fact (SubType t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $
            Fact (tVar `subKind` tVar') : Fact (tVar `typeConstraint` tVar') :
            Fact (tVar `subConst` tVar') :
            facts
    Fact (SubKind t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushSubKind handle handle'
      continue
    Fact (SubConst t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushSubConst handle handle'
      continue
    Fact (Typing t t') -> do
      handle <- simplify t >>= getHandle
      handle' <- simplify t' >>= getHandle
      pushTyping handle handle'
      fixFacts facts >>= continueWith
    Fact (Union t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      case tVar `unify` tVar' of
        Left errs -> errors <>= errs
        Right (subst, _) -> unifs %= (subst `apply`)
      _ <- fixAll
      fixFacts facts >>= continueWith
    Fact (ClassConstraint name t) ->
      uses classSchemes (name `Map.lookup`) >>= \case
        Nothing -> skip
        Just (tVars :. facts' :=> tVar', _) -> do
          tVar <- simplify t
          subst <- refresher tVars
          continueWith $ (Fact <$> ClassDetermine name (toType tVar) : typeUnion tVar (subst `apply` tVar') : (apply subst <$> facts')) <> facts
    Fact (ClassFact name t) ->
      uses classSchemes (name `Map.lookup`) >>= \case
        Nothing -> skip
        Just (tVars :. facts' :=> tVar', _) -> do
          tVar <- simplify t
          subst <- refresher tVars
          classFacts %= Map.insertWith mappend name (Set.singleton tVar)
          continueWith $ (Fact <$> typeUnion tVar (subst `apply` tVar') : (apply subst <$> facts')) <> facts
    Fact (ClassDetermine name t) ->
      uses classFacts (name `Map.lookup`) >>= \case
        Nothing -> skip
        Just tVars -> do
          tVar <- simplify t
          if tVar `Set.member` tVars
            then continue
            else skipWith . Fact $ ClassDetermine name (VarType tVar)
    Fact (ConstnessBounds bounds t) -> do
      handle <- simplify t >>= getHandle
      pushConstBounds handle bounds
      continue
    Fact (KindBounds (Bounds (Ordered minKind) (Ordered maxKind)) t) -> do
      handle <- simplify t >>= getHandle
      pushKindBounds handle $ minKind `Bounds` maxKind
      continue
    Fact (OnRegister reg t) -> do
      handle <- simplify t >>= getHandle
      kind <- registerKind reg
      pushKindBounds handle $ kind `Bounds` kind
      continue
    NestedFact (tVars :. [ClassConstraint name (VarType tVar)] :=> nesteds) -> do
      classSchemes %= Map.insertWith undefined name (tVars :. flattenFacts nesteds :=> tVar, mempty) -- TODO: error
      continue
    NestedFact (tVars :. [ClassFact name (VarType tVar)] :=> nesteds) -> do
      uses classSchemes (name `Map.lookup`) >>= \case
        Nothing -> skip
        Just (scheme@(tVars' :. facts' :=> t'), consts) -> do
          subst <- refresher tVars'
          classSchemes %= Map.insert name (scheme, (tVars :. flattenFacts nesteds :=> tVar) : consts)
          continueWith $ (Fact <$> typeUnion tVar (subst `apply` t') : flattenFacts nesteds <> reverseFacts (apply subst <$> facts')) <> facts
    NestedFact (tVars :. facts' :=> nesteds) -> do
      (changed, nesteds') <- wrapParent facts' $ reduceOne nesteds
      (_1 %~ (|| changed)) <$>
        skipWith (NestedFact (tVars :. facts' :=> nesteds'))
    Fact _ -> skip
  where
    skip = skipWith fact
    skipWith fact' = (_2 %~ (fact' :)) <$> reduceOne facts
    continue = continueWith facts
    continueWith = ((_1 .~ True) <$>) . reduceOne

reduceMany :: MonadInferencer m => Facts -> m (Bool, Facts)
reduceMany facts = do
  (change, facts') <- reduceOne facts
  if change
    then (_1 .~ True) <$> reduceMany facts'
    else return (False, facts)

reduce :: MonadInferencer m => Facts -> m (Bool, Facts)
reduce facts = do
  (change, facts') <- reduceMany facts
  let (facts'', sccs) = makeCallGraph facts'
  (change', facts''') <- closeSCCs facts'' sccs
  return (change || change', facts''')
  -- if change
  --   then (_1 .~ True) <$> reduce facts''
  --   else return (False, facts'')

-- floatFacts :: (Facts, [SCC (Fact, TypeVar, [TypeVar])]) -> Facts
-- floatFacts [] _ = (False, [])
-- floatFacts (fact:facts) counts = go $ floatFacts facts counts
--   where
--     go =
--       case fact of
--         NestedFact (_ :. [Union (VarType tVar) t] :=> fs)
--           | counts Map.! tVar == 1 -> (_1 .~ True) . (_2 %~ (newFacts <>))
--           where newFacts = Fact (Union (VarType tVar) t) : fs
--         _ -> _2 %~ (fact :)
readBounds :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> Bounds a
readBounds = (fromMaybe (minBound `Bounds` maxBound) .) . Map.lookup

readLowerBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readLowerBound = (view lowerBound .) . readBounds

readUpperBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readUpperBound = (view upperBound .) . readBounds

getConsting :: MonadInferencer m => TypeVar -> m TypeVar
getConsting tVar = view consting <$> getHandle tVar

getKinding :: MonadInferencer m => TypeVar -> m TypeVar
getKinding tVar = view kinding <$> getHandle tVar

getTyping :: MonadInferencer m => TypeVar -> m Type
getTyping tVar = view typing <$> getHandle tVar

collectPrimeTVars :: MonadState Inferencer m => TypeVar -> m (Set TypeVar)
collectPrimeTVars tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> fold <$> traverse collectPrimeTVars primType
    Nothing -> return $ Set.singleton tVar

reconstruct :: MonadState Inferencer m => TypeVar -> m Type
reconstruct tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> ComplType <$> traverse reconstruct primType
    Nothing -> return $ VarType tVar

minimizeSubs :: MonadInferencer m => TypeVar -> m ()
minimizeSubs parent = do
  constings <- uses subConsting transformMap
  constBounds <- use constingBounds
  kindings <- uses subKinding transformMap
  kindBounds <- use kindingBounds
  let (cSubsts, cRest) = laundry constBounds constings
      cSubst = foldTVarSubsts cSubsts
      (kSubsts, kRest) = laundry kindBounds kindings
      kSubst = foldTVarSubsts kSubsts
  fixSubGraph subConsting cSubst
  fixBounds constingBounds cSubst
  fixSubGraph subKinding kSubst
  fixBounds kindingBounds kSubst
  cRest `for_` \case
    Just ((_, limits), tVar)
      | null limits -> minimizeBounds constingBounds tVar
      | otherwise -> subConsting %= Map.insert tVar limits
    Nothing -> return ()
  kRest `for_` \case
    Just ((_, limits), tVar)
      | null limits -> minimizeBounds kindingBounds tVar
      | otherwise -> subKinding %= Map.insert tVar limits
    Nothing -> return ()
  void $
    safeHandlizeUpdate
      ((_2 . consting %~ apply cSubst) . (_2 . kinding %~ apply kSubst))
  where
    laundry boundsMap subGraph =
      let scc =
            Graph.stronglyConnCompR $
            (\(from, to) -> (from `readBounds` boundsMap, from, Set.toList to)) <$>
            subGraph
          depends = getDepends boundsMap scc mempty
          clusters =
            Map.toList . Map.fromListWith mappend .
            fmap (\(a, (b, c)) -> ((Ordered <$> b, c), Set.singleton a)) $
            Map.toList depends
       in unzip $ fromClusters boundsMap <$> clusters
    fromClusters boundsMap ((Ordered lower `Bounds` _, limits), tVars)
      | null limits = def
      | Set.size limits == 1 =
        let tVar' = Set.findMin limits
         in if readLowerBound tVar' boundsMap == lower
              then let Right (subst', _) =
                         apply subst tVar `unify` apply subst tVar'
                    in (subst' `apply` subst, nullVal)
              else def
      | otherwise = def
      where
        def = (subst, pure ((Ordered lower, limits), tVar))
        Right (subst, Just tVar) = unifyFold $ Set.toList tVars
    getDepends _ [] acc = acc
    getDepends boundsMap (Graph.AcyclicSCC (bounds, tVar, tVars):others) acc
      | tVarParent tVar == parent =
        getDepends boundsMap others $
        Map.insert tVar (bounds, mconcat (getLimits <$> tVars)) acc
      | otherwise = getDepends boundsMap others acc
      where
        getLimits tVar'
          | tVarParent tVar' == parent =
            Set.filter relevant . view _2 . fromMaybe (mempty, mempty) $ tVar' `Map.lookup`
            acc
          | parent `predecessor` tVarParent tVar' =
            if relevant tVar'
              then Set.singleton tVar'
              else mempty
          | otherwise = undefined -- TODO: error
        relevant tVar' = not . isTrivial $ readBounds tVar' boundsMap <> bounds
    getDepends _ _ _ = undefined -- TODO: error
    transformMap =
      Map.toList . fmap (Set.filter setFilter) . Map.filterWithKey mapFilter
    setFilter = predecessor parent . tVarParent
    mapFilter from _ = tVarParent from == parent

minimizeBounds ::
     (MonadInferencer m, Bounded a)
  => Lens' Inferencer (Map TypeVar (Bounds a))
  -> TypeVar
  -> m ()
minimizeBounds what tVar = do
  low <- uses what (tVar `readLowerBound`)
  what %= Map.insert tVar (low `Bounds` low)

-- TODO: remove parent (implicit)
freeParented :: MonadState Inferencer m => TypeVar -> m ([TypeVar], [TypeVar])
freeParented parent = do
  (consts, kinds) <- uses handlize $ unzip . fmap mineSubs . Bimap.elems
  subConsts <- use subConsting
  subKinds <- use subKinding
  return
    ( filter (isFreeParented subConsts) consts
    , filter (isFreeParented subKinds) kinds)
  where
    isFreeParented with tVar@TypeVar {} =
      not (tVar `Map.member` with) && tVarParent tVar == parent
    isFreeParented _ NoType = False
    mineSubs handle = (handle ^. consting, handle ^. kinding)

minimizeFree :: MonadInferencer m => TypeVar -> m ()
minimizeFree parent = do
  (consts, kinds) <- freeParented parent
  minimizeBounds constingBounds `traverse_` consts
  minimizeBounds kindingBounds `traverse_` kinds

-- TODO: remove parent (implicit)
floatSubs :: MonadInferencer m => TypeVar -> m ()
floatSubs parent = do
  (consts, kinds) <- (both %~ Set.fromList) <$> freeParented parent
  constBounds <- use constingBounds
  kindBounds <- use kindingBounds
  Set.intersection consts kinds `for_` \tVar -> do
    tVar' <- makeFloated tVar
    constingBounds %= Map.insert tVar' (tVar `readBounds` constBounds)
    kindingBounds %= Map.insert tVar' (tVar `readBounds` kindBounds)
  Set.difference consts kinds `for_` \tVar -> do
    tVar' <- makeFloated tVar
    constingBounds %= Map.insert tVar' (tVar `readBounds` constBounds)
  Set.difference kinds consts `for_` \tVar -> do
    tVar' <- makeFloated tVar
    kindingBounds %= Map.insert tVar' (tVar `readBounds` kindBounds)
  where
    makeFloated tVar = do
      int <- nextHandleCounter
      return tVar {tVarId = int, tVarParent = NoType}

-- TODO: remove newParent (implicit)
reParent :: Data d => TypeVar -> Set TypeVar -> d -> d
reParent newParent oldParents
  | oldParents == Set.singleton newParent = id
reParent newParent oldParents = go
  where
    go :: Data d => d -> d
    go = gmapT go `extT` tVarCase
    tVarCase tVar@TypeVar {tVarParent = parent}
      | parent `Set.member` oldParents = tVar {tVarParent = newParent}
      | otherwise = tVar
    tVarCase NoType = NoType

refresher :: (MonadInferencer m, HasTypeKind k) =>
  Set k -> m (Map k TypeVar)
refresher tVars =
  sequence $ Map.fromSet (freshTypeHelper . getTypeKind) tVars

unSchematize :: MonadInferencer m => Facts -> m Facts
unSchematize [] = return []
unSchematize (Fact (InstType (VarType scheme) inst):others) = do
  uses schemes (scheme `Map.lookup`) >>= \case
    Just scheme'
      | Set.size scheme' == 1 -> do
        let tVars :. facts :=> t = Set.findMin scheme'
        instSubst <- refresher tVars
        let facts' = Fact . apply instSubst <$> facts
            t' = instSubst `apply` t
        ((Fact (inst `Union` t') : facts') <>) <$> unSchematize others
      | otherwise -> undefined -- TODO: classes
    -- | Nothing: the typeVar `scheme` is in the same same scc
    Nothing -> (Fact (VarType scheme `Union` inst) :) <$> unSchematize others
unSchematize (fact:others) = (fact :) <$> unSchematize others

fOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f g x = f x || g x

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = f x && g x

registerScheme :: MonadInferencer m => TypeVar -> Scheme Type -> m ()
registerScheme tVar scheme = schemes %= Map.insert tVar (Set.singleton scheme)

-- TODO: check whether all typings are set (otherwise error)
schematize :: MonadInferencer m => Set TypeVar -> m ()
schematize tVars = do
  parent <- getParent
  x <- Set.toList . Set.unions <$> traverse collectPrimeTVars (Set.toList tVars)
  constings <- Set.fromList <$> traverse getConsting x
  kindings <- Set.fromList <$> traverse getKinding x
  -- TODO: leave out trivial
  typings <- zip x <$> traverse getTyping x
  subConsts <-
    uses subConsting $ filter (parentedBy parent `fOr` presentIn constings) .
    Map.toList
  subKinds <-
    uses subKinding $ filter (parentedBy parent `fOr` presentIn kindings) .
    Map.toList
  constFacts <- translateSubs constingBounds SubConst ConstnessBounds subConsts
  kindFacts <-
    translateSubs kindingBounds SubKind (KindBounds . fmap Ordered) subKinds
  let typingFacts = uncurry typeConstraint <$> typings
      facts = typingFacts <> constFacts <> kindFacts
  tVars `for_` \tVar -> do
    t <- reconstruct tVar
    let scheme = freeTypeVars facts :. facts :=> t
    registerScheme tVar scheme
  where
    translateSubs _ _ _ [] = return []
    translateSubs bounds subConstr boundsConstr ((tVar, limits):others) = do
      let facts =
            (\tVar' -> toType tVar `subConstr` toType tVar') <$>
            Set.toList limits
          translateOthers =
            (facts <>) <$> translateSubs bounds subConstr boundsConstr others
      uses bounds (tVar `Map.lookup`) >>= \case
        Nothing -> translateOthers
        Just bounds' ->
          (boundsConstr bounds' (toType tVar) :) <$> translateOthers
    parentedBy parent (key, _) = tVarParent key == parent
    presentIn where' (key, _) = key `Set.member` where'

closeSCCs ::
     MonadInferencer m
  => Facts
  -> [SCC (Fact, TypeVar, [TypeVar])]
  -> m (Bool, Facts)
closeSCCs facts [] = return (False, facts)
closeSCCs facts (scc:others) =
  case scc of
    AcyclicSCC trio -> followUp $ pure trio
    CyclicSCC trios -> followUp trios
  where
    followUp trios = do
      let parents = getParents trios
          parent = head parents
          rePar :: Data d => d -> d
          rePar = reParent parent (Set.fromList parents)
      typize %= Bimap.fromList . rePar . Bimap.toList
      handlize %= Bimap.fromList . rePar . Bimap.toList
      subConsting %= rePar
      constingBounds %= rePar
      subKinding %= rePar
      kindingBounds %= rePar
      unifs %= rePar
      pushParent parent
      (_, facts') <- traverse transformFact (fmap rePar trios) >>= reduceMany .
        (<> fmap rePar facts) . concat
      minimizeSubs parent
      minimizeFree parent
      _ <- fixAll
      parent' <- uses unifs (`apply` parent)
      popParent *> pushParent parent'
      floatSubs parent'
      _ <- fixAll
      parent'' <- uses unifs (`apply` parent)
      popParent *> pushParent parent''
      (_, facts'') <- fixFacts facts' >>= reduceMany
      parents' <- uses unifs ((<$> parents) . apply)
      schematize $ Set.fromList parents'
      (_1 .~ True) <$> (popParent *> closeSCCs facts'' others)
    transformFact (NestedFact (_ :. [fact] :=> fs), _, _) = do
      fact' <- uses unifs (`apply` fact)
      (Fact fact' :) <$> unSchematize fs
    transformFact _ = undefined
    getParents [] = []
    getParents ((_, tVar, _):rest) = tVar : getParents rest

makeCallGraph :: Facts -> (Facts, [SCC (Fact, TypeVar, [TypeVar])])
makeCallGraph = (_2 %~ stronglyConnCompR) . foldr transform ([], [])
  where
    transform fact =
      case fact of
        NestedFact (_ :. [Union (VarType tVar) _] :=> fs) ->
          _2 %~ ((fact, tVar, foldr out [] fs) :)
        _ -> _1 %~ (fact :)
    out fact =
      case fact of
        Fact (InstType (VarType scheme) _) -> (scheme :)
        Fact InstType {} -> undefined
        _ -> id

collectCounts :: Facts -> Subst Int
collectCounts = foldr countIn mempty
  where
    countIn (NestedFact (_ :. [Union (VarType tVar) _] :=> _)) =
      Map.insertWith (+) tVar 1
    countIn _ = id

data Way
  = Forward
  | Both

collectPairs :: Way -> Int -> Map TypeVar (Set TypeVar) -> [(TypeVar, TypeVar)]
collectPairs way handles from = pairs <&> both %~ \i -> varMap Map.! i
  where
    edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> Map.toList from)
    varMap =
      Map.fromList $ (\tVar -> (tVarId tVar, tVar)) <$>
      uncurry (<>) (unzip edges)
    graph = Graph.buildG (1, handles) $ (both %~ tVarId) <$> edges
    vs = Map.keys varMap
    pairs =
      case way of
        Forward -> [(v, v') | v <- vs, v' <- vs, v /= v', Graph.path graph v v']
        Both ->
          [ (v, v')
          | v <- vs
          , v' <- vs
          , v < v'
          , Graph.path graph v v'
          , Graph.path graph v' v
          ]

deduceUnifs :: Int -> Map TypeVar (Set TypeVar) -> Map TypeVar TypeVar
deduceUnifs handles which = go pairs mempty
  where
    pairs = collectPairs Both handles which
    go [] subst = subst
    go ((tVar, tVar'):others) subst =
      case apply subst tVar `unify` apply subst tVar' of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go others $ subst' `apply` subst

propagateBounds ::
     (MonadInferencer m, Lattice a, Bounded a)
  => Lens' Inferencer (Map TypeVar (Bounds a))
  -> Getter Inferencer (Map TypeVar (Set TypeVar))
  -> m ()
propagateBounds which by = do
  pairs <- liftA2 (collectPairs Forward) (use handleCounter) (use by)
  for_ pairs $ \(v, v') -> do
    uses which (v' `Map.lookup`) >>=
      traverse_ ((which %=) . Map.insertWith (<>) v . (upperBound .~ maxBound))
    uses which (v `Map.lookup`) >>=
      traverse_ ((which %=) . Map.insertWith (<>) v' . (lowerBound .~ minBound))

boundsUnifs ::
     (MonadInferencer m, PartialOrd a, Eq a, Ord (Ordered a), Bounded a)
  => Getter Inferencer (Map TypeVar (Bounds a))
  -> m (Subst TypeVar)
boundsUnifs which = do
  whichList <- filter (isTrivial . view _2) <$> uses which Map.toList
  let trivialGroups =
        mapFold $ (_2 %~ Set.singleton) . (_1 %~ Ordered . normalizeAbsurd) .
        swap <$>
        whichList
  let nontrivialTrivialGroups =
        (Set.toList <$>) . Map.elems $
        Map.filter ((> 1) . Set.size) trivialGroups
  return $ go nontrivialTrivialGroups mempty
  where
    go ((first:second:others):rest) subst =
      case apply subst first `unify` apply subst second of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go ((first : others) : rest) $ subst' `apply` subst
    go (_:rest) subst = go rest subst
    go [] subst = subst

registerKind :: MonadInferencer m => Text -> m DataKind
registerKind = undefined

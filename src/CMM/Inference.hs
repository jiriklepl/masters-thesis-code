{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

{-|
Module      : CMM.Inference
Description : Inference layer
Maintainer  : jiriklepl@seznam.cz

This module defines the inference layer of the compiler.

The most important function is `reduce` - it runs the whole type inference.
Then, `reduceTrivial` contains most cases for constraint solving.
-}

module CMM.Inference where

import safe Control.Applicative (Applicative(liftA2))
import safe Control.Lens
    ( Lens',
      (+=),
      Getter,
      (^.),
      use,
      uses,
      view,
      (%=),
      (%~),
      (.=),
      (.~),
      both,
      _1,
      _2 )
import safe Control.Monad ((>=>), when)
import safe Data.Bifunctor (second, Bifunctor (first))
import safe Data.Data (Data(gmapT))
import safe Data.Foldable (Foldable(foldl'), for_, traverse_)
import safe Data.Functor (($>), (<&>), void)
import safe Data.Graph (SCC(AcyclicSCC, CyclicSCC), stronglyConnCompR)
import safe qualified Data.Graph as Graph
import safe Data.List (partition)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe, fromJust)
import safe Data.PartialOrd (PartialOrd)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.String (fromString)
import safe Data.Text (Text)
import safe Data.Tuple (swap)
import safe Data.Tuple.Extra (dupe)


import safe qualified CMM.Data.Bimap as Bimap
import safe CMM.Data.Bounds
  ( Bounds(Bounds)
  , isTrivialOrAbsurd
  , lowerBound
  , normalizeAbsurd
  , upperBound
  )
import safe CMM.Data.Function (fOr)
import safe CMM.Data.Generics ((*|*))
import safe CMM.Data.Lattice (Lattice)
import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Data.Ordered (Ordered(Ordered))
import safe CMM.Data.OrderedBounds ()
import safe CMM.Data.Trilean (trilean)
import safe CMM.Data.Way (Way(Backward, Both, Forward))
import safe CMM.Inference.BuiltIn ()
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact
  ( Fact
  , Facts
  , FlatFact(ClassConstraint, ClassFact, ClassFunDeps, ConstEquality,
         ConstnessBounds, FactComment, InstType, KindBounds, KindEquality,
         Lock, OnRegister, SubConst, SubKind, SubType, TypingEquality, Equality)
  , FlatFacts
  , NestedFact(Fact, NestedFact)
  , Qual((:=>))
  , Scheme((:.))
  , classConstraint
  , constEquality
  , kindEquality
  , subConst
  , subKind
  , typingEquality
  , typeEquality
  )
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter (nextHandleCounter, handleCounter)
import safe CMM.Inference.Refresh (Refresh(refresh))
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.State
    ( InferencerState, Inferencer )
import safe CMM.Inference.Subst
  ( Apply(apply)
  , ApplyShallow(applyShallow)
  , Subst
  , foldTVarSubsts
  )
import safe CMM.Inference.Type
  ( ToType(toType)
  , Type(ComplType, VarType)
  , foldApp
  , unfoldApp
  )
import safe CMM.Inference.TypeCompl
  ( PrimType
  , TypeCompl
  )
import safe CMM.Inference.Properties
  ( Properties
  , consting


  , kinding
  , typing
  )
import safe CMM.Inference.TypeKind (TypeKind(GenericType), getTypeKind)
import safe CMM.Inference.TypeVar
  ( TypeVar(NoType, TypeVar, tVarId, tVarParent)
  , predecessor
  )
import safe CMM.Inference.Unify (instanceOf, unify, unifyFold, unifyLax)
import safe CMM.Inference.Utils (trileanSeq)
import safe CMM.Utils (HasCallStack, addPrefix, logicError, notYetImplemented)
import safe qualified CMM.Inference.State.Impl as State
import safe CMM.Err.State ( nullErrorState )

-- | creates typing for the given primitive pattern,
--   derived from typings of the immediate type subterms
createTyping :: PrimType -> Inferencer (TypeCompl Type)
createTyping primType = do
  props <- use State.typeProps
  let oneLevel t = maybe (VarType t) (view typing) (t `Bimap.lookup` props)
  return $ oneLevel <$> primType

-- | simplifies a given type into a representant type variable
simplify :: HasCallStack => Type -> Inferencer TypeVar
simplify =
  \case
    VarType tVar -> do
      uses State.typeProps (tVar `Bimap.lookup`) >>= \case
        Just _ -> return tVar
        Nothing -> State.typePropsTVar tVar
    ComplType complType ->
      traverse simplify complType >>= \primType ->
        uses State.typings (primType `Bimap.lookupR`) >>= \case
          Nothing -> do
            tVar <- State.freshTypeHelperWithHandle $ getTypeKind primType
            State.typings %= Bimap.insert tVar primType
            t <- createTyping primType
            State.typeProps %= Bimap.adjust (typing .~ ComplType t) tVar
            return tVar
          Just tVar -> return tVar

-- | unifies the given typings (constitutes an occurs check)
pushTyping :: Properties -> Properties -> Inferencer ()
pushTyping props props' = do
  let t = props ^. typing
      t' = props' ^. typing
  case unify t t' of
    Left errs -> State.addUnificationErrors errs
    Right (subst, _) -> do
      let fixIt = do
            fixTypings >>= \case
              True ->
                fixProperties >>= \case
                  True -> fixIt
                  False -> fixSubs
              False -> fixSubs
      safePropertiesUpdate (_2 . typing %~ apply subst) >>= flip when (void fixIt)

fixAll :: Inferencer Bool
fixAll = do
  results <- sequence [fixTypings, fixProperties, fixSubs]
  if or results
    then True <$ fixAll
    else return False

mapFold :: (Ord a, Semigroup b) => [(a, b)] -> Map a b
mapFold = foldl' (flip . uncurry $ Map.insertWith (<>)) Map.empty

fixSubGraph ::
     Bool
  -> Lens' InferencerState (Map TypeVar (Set TypeVar))
  -> Subst TypeVar
  -> Inferencer ()
fixSubGraph isShallow which subst = do
  let applyUnifs =
        (_1 %~
         (if isShallow
            then applyShallow
            else apply)
           subst) .
        (_2 %~ Set.fromList .
         ((if isShallow
             then applyShallow
             else apply)
            subst <$>) .
         Set.toList)
  whichList <- uses which $ (applyUnifs <$>) . Map.toList
  let which' = mapFold whichList
  which .= Map.filter (not . null) (Map.mapWithKey Set.delete which')

fixBounds ::
     Lattice a
  => Bool
  -> Lens' InferencerState (Map TypeVar (Bounds a))
  -> Subst TypeVar
  -> Inferencer ()
fixBounds isShallow which subst = do
  whichList <-
    uses which $
    ((_1 %~
      (if isShallow
         then applyShallow
         else apply)
        subst) <$>) .
    Map.toList
  which .= mapFold whichList

fixSubs :: Inferencer Bool
fixSubs = do
  renaming' <- use State.renaming
  let fixBoth ::
           (Bounded a, Eq a, Ord (Ordered a), Lattice a)
        => Lens' InferencerState (Map TypeVar (Set TypeVar))
        -> Lens' InferencerState (Map TypeVar (Bounds a))
        -> Inferencer (Subst TypeVar)
      fixBoth sub bounds = do
        fixSubGraph False sub renaming'
        fixBounds False bounds renaming'
        subst <- subUnifs
        fixSubGraph True sub subst
        fixBounds True bounds $ subst `apply` renaming'
        propagateBounds bounds sub
        boundsUnifs bounds >>= go subst
        where
          subUnifs = liftA2 deduceUnifs (use handleCounter) (use sub)
          go accum subst
            | null subst = return accum
            | otherwise = do
              fixSubGraph True sub subst
              subst' <- subUnifs
              fixSubGraph True sub subst'
              let subst'' = subst' `apply` subst
              fixBounds True bounds subst''
              propagateBounds bounds sub
              boundsUnifs bounds >>= go (subst'' `apply` accum)
  subst <- fixBoth State.subKinding State.kindingBounds
  subst' <- fixBoth State.subConsting State.constingBounds
  if null subst && null subst'
    then return False
    else safePropertiesUpdate $ (_2 . kinding %~ apply subst) .
         (_2 . consting %~ apply subst')

unsafeTypizeUpdate ::
     ((TypeVar, PrimType) -> (TypeVar, PrimType)) -> Inferencer ()
unsafeTypizeUpdate change =
  State.typings %= Bimap.fromList . (change <$>) . Bimap.toList

unsafeHandlizeUpdate ::
     ((TypeVar, Properties) -> (TypeVar, Properties)) -> Inferencer ()
unsafeHandlizeUpdate change =
  State.typeProps %= Bimap.fromList . (change <$>) . Bimap.toList

isSingleton :: Set a -> Bool
isSingleton = (== 1) . Set.size

safePropertiesUpdate ::
     ((TypeVar, Properties) -> (TypeVar, Properties)) -> Inferencer Bool
safePropertiesUpdate change = do
  propss' <- uses State.typeProps ((change <$>) . Bimap.toList)
  let typeProps' = Bimap.fromList propss'
      collapsedKeys = mapCollect propss'
      collapsedValues = mapCollect (swap <$> propss')
      goValues [] tSubst cSubst kSubst = return (tSubst, cSubst, kSubst)
      goValues ((props:props':propss):others) tSubst cSubst kSubst = do
        (tSubst', _) <-
          apply tSubst (props ^. typing) `unify`
          apply tSubst (props' ^. typing)
        (cSubst', _) <-
          apply cSubst (props ^. consting) `unifyLax`
          apply cSubst (props' ^. consting)
        (kSubst', _) <-
          apply kSubst (props ^. kinding) `unifyLax`
          apply kSubst (props' ^. kinding)
        goValues
          ((props' : propss) : others)
          (tSubst' `apply` tSubst)
          (cSubst' `apply` cSubst)
          (kSubst' `apply` kSubst)
      goValues (_:others) tSubst cSubst kSubst =
        goValues others tSubst cSubst kSubst
      goKeys [] subst = return subst
      goKeys ((tVar:tVar':tVars):others) subst = do
        (subst', _) <- apply subst tVar `unify` apply subst tVar'
        goKeys ((tVar' : tVars) : others) $ subst' `apply` subst
      goKeys (_:others) subst = goKeys others subst
  State.typeProps .= typeProps'
  if all isSingleton collapsedKeys && all isSingleton collapsedValues
    then return False
    else do
      let performForgotten = do
            (tSubst, cSubst, kSubst) <-
              goValues
                (Set.toList . snd <$> Map.toList collapsedKeys)
                (mempty :: Map TypeVar Type)
                (mempty :: Map TypeVar TypeVar)
                (mempty :: Map TypeVar TypeVar)
            subst <-
              goKeys
                (Set.toList . snd <$> Map.toList collapsedValues)
                (mempty :: Map TypeVar TypeVar)
            return (tSubst, cSubst, kSubst, subst)
      case performForgotten of
        Left errs -> do
          State.addUnificationErrors errs
          return False
        Right (tSubst, cSubst, kSubst, subst) -> do
          State.renaming %= apply subst
          fixSubGraph True State.subConsting cSubst
          fixSubGraph True State.subKinding kSubst
          fixBounds True State.constingBounds cSubst
          fixBounds True State.kindingBounds kSubst
          (True <$) . safePropertiesUpdate $ apply subst .
            (_2 . typing %~ apply tSubst) .
            (_2 . consting %~ applyShallow cSubst) .
            (_2 . kinding %~ applyShallow kSubst)

fixProperties :: Inferencer Bool
fixProperties = uses State.renaming apply >>= safePropertiesUpdate

mapCollect :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
mapCollect = Map.fromListWith mappend . fmap (_2 %~ Set.singleton)

onCandidates :: [t1] -> (t1 -> t2 -> Maybe a) -> t2 -> Maybe a
onCandidates (candidate:candidates) lookup' database =
  case candidate `lookup'` database of
    Just result -> return result
    Nothing -> onCandidates candidates lookup' database
onCandidates [] _ _ = Nothing

fixTypings :: Inferencer Bool
fixTypings = do
  types' <- uses State.renaming (fmap . apply) <*> uses State.typings Bimap.toList
  let typings' = Bimap.fromList types'
      collapsedKeys = mapCollect types'
      collapsedValues = mapCollect $ swap <$> types'
      goValues [] subst = return subst
      goValues ((primType:primType':primTypes):others) subst =
        case apply subst primType `unify` apply subst primType' of
          Left errs -> do
            State.addUnificationErrors errs
            return subst
          Right (subst', _) ->
            goValues ((primType' : primTypes) : others) $ subst' `apply` subst
      goValues (_:others) subst = goValues others subst
      goKeys [] subst = return subst
      goKeys ((tVar:tVar':tVars):others) subst =
        case apply subst tVar `unify` apply subst tVar' of
          Left errs -> do
            State.addUnificationErrors errs
            return subst
          Right (subst', _) ->
            goKeys ((tVar' : tVars) : others) $ subst' `apply` subst
      goKeys (_:others) subst = goKeys others subst
  State.typings .= typings'
  if all isSingleton collapsedKeys && all isSingleton collapsedValues
    then return False
    else do
      subst <-
        goValues
          (Set.toList . snd <$> Map.toList collapsedValues)
          (mempty :: Subst TypeVar) >>=
        goKeys (Set.toList . snd <$> Map.toList collapsedKeys)
      State.renaming %= apply subst
      True <$ fixTypings

fixFacts :: Facts -> Inferencer Facts
fixFacts facts = uses State.renaming $ flip fmap facts . apply

wrapParent :: HasCallStack => FlatFacts -> Inferencer a -> Inferencer a
wrapParent flatFacts wrapped =
  case determineParent flatFacts of
    Just parent -> State.pushParent parent *> wrapped <* State.popParent
    Nothing -> wrapped
  where
    determineParent [VarType tVar `Equality` _] = Just tVar
    determineParent _ = logicError

unwrapParent :: Inferencer a -> Inferencer a
unwrapParent unwrapped = do
  parent <- getParent
  State.popParent *> unwrapped <* State.pushParent parent

flattenFacts :: HasCallStack => [NestedFact Type] -> FlatFacts
flattenFacts = fmap go
  where
    go (Fact fact) = fact
    go _ = logicError

reverseFacts :: HasCallStack => FlatFacts -> FlatFacts
reverseFacts = fmap go
  where
    go (ClassFact name t) = ClassConstraint name t
    go _ = logicError

reduceTemplates :: Facts -> Inferencer Facts
reduceTemplates [] = return []
reduceTemplates (fact:facts) =
  case fact of
    NestedFact (tVars :. [ClassConstraint name t] :=> nesteds) -> do
      subst <- refresh tVars
      let tVars' = apply subst `Set.map` tVars
      State.addClassScheme name $ tVars' :. flattenFacts (subst `apply` nesteds) :=>
        apply subst t
      continue
    NestedFact (tVars :. [ClassFact name t] :=> nesteds) ->
        State.lookupFunDep name >>= \case
          Nothing -> goOtherwise
          Just rules -> do
            traverse_ go rules
            continue
            where class':args = unfoldApp t
                  go rule = do
                    tVars' <-
                      const (State.freshTypeHelperWithHandle GenericType) `traverse`
                      args
                    let t' =
                          foldApp $ class' :
                          zipWith
                            (uncurry chooseArg)
                            (zip args (toType <$> tVars'))
                            rule
                        fs = zipWith typeEquality tVars' args
                    State.addClassFact (fromString (trileanSeq rule) `addPrefix` name) $
                      Set.fromList tVars' :.
                      fs :=>
                      t'
                  chooseArg arg tVar = trilean arg tVar tVar
      where
        goOtherwise = State.lookupClassScheme name >>= \case
          Nothing -> skip
          Just (tVars' :. facts' :=> t') -> do
            subst <- refresh tVars'
            State.addClassFact name $ (tVars <> (apply subst `Set.map` tVars')) :.
              ((t `typeEquality` apply subst t') :
               reverseFacts [f | Fact f <- nesteds] <>
               apply subst facts') :=>
              t
            subst' <- refresh tVars
            reduceTemplates $
              (Fact <$> typeEquality (subst `apply` t') (subst' `apply` t) : flattenFacts (subst' `apply` nesteds)) <> facts <> (Fact <$> reverseFacts (apply subst <$> facts'))
    Fact (ClassFunDeps name rules) -> do
      State.addFunDeps name rules
      continue
    _ -> skip
  where
    skip = (fact :) <$> continue
    continue = reduceTemplates facts

reduceTrivial :: Facts -> Inferencer (Bool, Facts)
reduceTrivial [] = fixAll $> (False, [])
reduceTrivial (fact:facts) =
  case fact of
    Fact (SubType t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $ Fact (tVar `subKind` tVar') :
        Fact (tVar `typingEquality` tVar') :
        Fact (tVar `subConst` tVar') :
        facts
    Fact (KindEquality t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $ Fact (tVar `subKind` tVar') : Fact (tVar' `subKind` tVar) :
        facts
    Fact (SubKind t t') -> do
      props <- simplify t >>= State.getProps
      props' <- simplify t' >>= State.getProps
      State.pushSubKind props props'
      continue
    Fact (SubConst t t') -> do
      props <- simplify t >>= State.getProps
      props' <- simplify t' >>= State.getProps
      State.pushSubConst props props'
      continue
    Fact (ConstEquality t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $ Fact (tVar `subConst` tVar') : Fact (tVar' `subConst` tVar) :
        facts
    Fact (TypingEquality t t') -> do
      props <- simplify t >>= State.getProps
      props' <- simplify t' >>= State.getProps
      pushTyping props props'
      fixFacts facts >>= continueWith
    Fact (Equality t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      case tVar `unify` tVar' of
        Left errs -> State.addUnificationErrors errs
        Right (subst, _) -> State.renaming %= (subst `apply`)
      _ <- fixAll
      fixFacts facts >>= continueWith
    Fact (ConstnessBounds bounds t) -> do
      props <- simplify t >>= State.getProps
      State.pushConstBounds props bounds
      continue
    Fact (KindBounds (Bounds (Ordered minKind) (Ordered maxKind)) t) -> do
      props <- simplify t >>= State.getProps
      State.pushKindBounds props $ minKind `Bounds` maxKind
      continue
    Fact (OnRegister reg t) -> do
      props <- simplify t >>= State.getProps
      kind <- registerKind reg
      State.pushKindBounds props $ kind `Bounds` kind
      continue
    Fact (Lock t) -> do
      tVar <- simplify t
      State.lock t tVar
      continue
    Fact FactComment {} -> continue
    NestedFact (tVars :. facts' :=> nesteds) ->
      case facts' of
        [ClassConstraint {}] -> skip
        [ClassFact {}] -> skip
        _ -> do
          (change, nesteds') <- wrapParent facts' $ reduceTrivial nesteds
          fmap (first (change ||)) . skipWith . NestedFact $ tVars :. facts' :=> nesteds'
    _ -> skip
  where
    skip = skipWith fact
    skipWith fact' = second (fact' :) <$> reduceTrivial facts
    continue = continueWith facts
    continueWith = fmap (first $ const True) . reduceTrivial

reduceConstraint :: Facts -> Inferencer (Bool, Facts)
reduceConstraint [] = fixAll $> (False, [])
reduceConstraint (fact:facts) =
  case fact of
    Fact (ClassConstraint name t) ->
      State.lookupFunDep name >>= \case
          Nothing -> goOtherwise
          Just rules -> do
            State.currentFunDeps += 1
            current <- use State.currentFunDeps
            max' <- use State.maxFunDeps
            if current < max'
              then continueWith facts'
              else return (False, facts' <> facts)
            where
              facts' = fmap go rules
              go rule =
                    Fact $
                    classConstraint
                      (fromString (trileanSeq rule) `addPrefix` name)
                      t
      where
        goOtherwise = State.lookupFact name >>= \case
          Nothing -> skip
          Just schemes' -> do
            tType <- simplify t >>= State.getTyping
            let go accum =
                  \case
                    (tVars :. fs :=> t'):others -> do
                      tType' <- simplify t' >>= State.getTyping
                      if tType `instanceOf` tType'
                        then do
                          subst <- refresh tVars
                          let freshT = subst `apply` t'
                              newFacts =
                                typeEquality t freshT : (apply subst <$> fs)
                          go (fmap Fact newFacts : accum) others
                        else go accum others
                    [] ->
                      case accum of
                        h:_ -> continueWith h
                        _ -> skip
            go mempty schemes'
    Fact (ClassFact name t) ->
      State.lookupClassScheme name >>= \case
        Nothing -> skip
        Just (tVars :. facts' :=> t') -> do
          subst <- refresh tVars
          State.addClassFact name $ mempty :. [] :=> t
          continueWith $
            Fact <$> typeEquality t (subst `apply` t') :
             (apply subst <$> facts')
    NestedFact (tVars :. facts' :=> nesteds) -> do
      (changed, nesteds') <- wrapParent facts' $ reduceConstraint nesteds
      (_1 %~ (|| changed)) <$>
        skipWith (NestedFact (tVars :. facts' :=> nesteds'))
    Fact InstType {} -> skip
    Fact _ -> skip
  where
    skip = skipWith fact
    skipWith fact' = (_2 %~ (fact' :)) <$> reduceConstraint facts
    continueWith facts' = return (True, facts' <> facts)

reduceMany :: Facts -> Inferencer (Bool, Facts)
reduceMany = repeatStep reduceTrivial >=> reduceTemplates . snd >=> go
  where
    go facts = do
      result@(change, facts') <- repeatStep reduceTrivial facts >>= reduceConstraint . snd
      if change
        then go facts'
        else return result
    repeatStep what facts = do
      result@(change, facts') <- what facts
      if change
        then repeatStep what facts'
        else return result

-- | runs the whole inference algorithm
reduce :: Facts -> Inferencer (Bool, Facts)
reduce facts = do
  (change, facts') <- reduceMany facts
  nullErrorState >>= \case
    False -> return (change, facts')
    _ -> do
      State.currentFunDeps .= 0
      let (facts'', sccs) = makeCallGraph facts'
      (change', facts''') <- closeSCCs facts'' sccs
      return (change || change', facts''')

-- | the subtype dimension minimization phase
minimizeSubs :: TypeVar -> Set TypeVar -> Inferencer ()
minimizeSubs parent pTypeVars = do
  pTypeConstings <-
    fmap Set.fromList . traverse State.getConsting $ Set.toList pTypeVars
  pTypeKindings <-
    fmap Set.fromList . traverse State.getKinding $ Set.toList pTypeVars
  constings <- uses State.subConsting (transformMap pTypeConstings)
  constBounds <- use State.constingBounds
  kindings <- uses State.subKinding (transformMap pTypeKindings)
  kindBounds <- use State.kindingBounds
  let (cSubsts, cRest) = minimizeInner parent pTypeConstings constBounds constings
      cSubst = foldTVarSubsts cSubsts
      (kSubsts, kRest) = minimizeInner parent pTypeKindings kindBounds kindings
      kSubst = foldTVarSubsts kSubsts
  fixSubGraph True State.subConsting cSubst
  fixBounds True State.constingBounds cSubst
  fixSubGraph True State.subKinding kSubst
  fixBounds True State.kindingBounds kSubst
  cRest `for_` \case
    Just ((_, limits), tVar)
      | null limits -> minimizeBounds State.constingBounds tVar
      | otherwise -> State.subConsting %= Map.insert tVar limits
    Nothing -> return ()
  kRest `for_` \case
    Just ((_, limits), tVar)
      | null limits -> minimizeBounds State.kindingBounds tVar
      | otherwise -> State.subKinding %= Map.insert tVar limits
    Nothing -> return ()
  void $
    safePropertiesUpdate
      ((_2 . consting %~ apply cSubst) . (_2 . kinding %~ apply kSubst))
  where
    transformMap pTypeThings =
      Map.toList . fmap (Set.filter (setFilter pTypeThings)) .
      Map.filterWithKey (mapFilter pTypeThings)
    setFilter pTypeThings =
      flip predecessor parent . tVarParent `fOr` (`Set.member` pTypeThings) `fOr`
      (`Set.member` pTypeVars)
    mapFilter pTypeThings from _ =
      tVarParent from == parent && not (from `Set.member` pTypeThings)

-- | helper function for subtype minimization
minimizeInner ::
     ( Bounded a
     , Lattice a
     , Eq a
     , Ord (Ordered a)
     , Applicative f
     , Nullable (f ((Ordered a, Set TypeVar), TypeVar))
     )
  => TypeVar
  -> Set TypeVar
  -> Map TypeVar (Bounds a)
  -> [(TypeVar, Set TypeVar)]
  -> ([Map TypeVar TypeVar], [f ((Ordered a, Set TypeVar), TypeVar)])
minimizeInner parent pTypeThings boundsMap subGraph =
  let scc =
        Graph.stronglyConnCompR $
        (\(from, to) -> (from `State.readBoundsFrom` boundsMap, from, Set.toList to)) <$>
        subGraph
      depends = getDepends scc mempty
      clusters =
        Map.toList . Map.fromListWith mappend .
        fmap (\(a, (b, c)) -> ((Ordered <$> b, c), Set.singleton a)) $
        Map.toList depends
   in unzip $ fromClusters <$> clusters
  where
    fromClusters ((Ordered lower `Bounds` _, limits), tVars)
      | null limits = def
      | Set.size limits == 1 =
        let tVar' = Set.findMin limits
         in if State.readLowerBound tVar' boundsMap == lower
              then let Right (subst', _) =
                         apply subst tVar `unify` apply subst tVar'
                    in (subst' `apply` subst, nullVal)
              else def
      | otherwise = def
      where
        def = (subst, pure ((Ordered lower, limits), tVar))
        Right (subst, Just tVar) = unifyFold $ Set.toList tVars
    getDepends [] acc = acc
    getDepends (Graph.AcyclicSCC (bounds, tVar, tVars):others) acc
      | tVarParent tVar == parent =
        getDepends others $
        Map.insert tVar (bounds, mconcat (getLimits <$> tVars)) acc
      | otherwise = getDepends others acc
      where
        getLimits tVar'
          | tVarParent tVar' == parent && not (tVar' `Set.member` pTypeThings) =
            Set.filter relevant . view _2 . fromMaybe (mempty, mempty) $ tVar' `Map.lookup`
            acc
          | otherwise =
            if relevant tVar'
              then Set.singleton tVar'
              else mempty
        relevant tVar' =
          not . isTrivialOrAbsurd $ State.readBoundsFrom tVar' boundsMap <> bounds
    getDepends _ _ = logicError

-- | helper function for minimization of bounds
minimizeBounds ::
     Bounded a
  => Lens' InferencerState (Map TypeVar (Bounds a))
  -> TypeVar
  -> Inferencer ()
minimizeBounds what tVar = do
  low <- uses what (tVar `State.readLowerBound`)
  what %= Map.insert tVar (low `Bounds` low)

freeSubtypeVars :: Set TypeVar -> Inferencer ([TypeVar], [TypeVar])
freeSubtypeVars ts = do
  parent <- getParent
  let
    isFreeSubtypeVar tIngs with tVar@TypeVar {} =
      not (tVar `Map.member` with) && tVarParent tVar == parent &&
      not (tVar `Set.member` tIngs)
    isFreeSubtypeVar _ _ NoType = False
  (consts, kinds) <- uses State.typeProps $ unzip . fmap mineSubs . Bimap.elems
  subConsts <- use State.subConsting
  subKinds <- use State.subKinding
  tConsts <- fmap Set.fromList . traverse State.getConsting $ Set.toList ts
  tKinds <- fmap Set.fromList . traverse State.getKinding $ Set.toList ts
  return
    ( filter (isFreeSubtypeVar tConsts subConsts) consts
    , filter (isFreeSubtypeVar tKinds subKinds) kinds)
  where
    mineSubs props = (props ^. consting, props ^. kinding)

minimizeFree :: Set TypeVar -> Inferencer ()
minimizeFree ts = do
  (consts, kinds) <- freeSubtypeVars ts
  minimizeBounds State.constingBounds `traverse_` consts
  minimizeBounds State.kindingBounds `traverse_` kinds

floatSubs :: Set TypeVar -> Inferencer ()
floatSubs ts = do
  (consts, kinds) <- (both %~ Set.fromList) <$> freeSubtypeVars ts
  constBounds <- use State.constingBounds
  kindBounds <- use State.kindingBounds
  Set.intersection consts kinds `for_` \tVar -> do
    tVar' <- makeFloated tVar
    State.constingBounds %= Map.insert tVar' (tVar `State.readBoundsFrom` constBounds)
    State.kindingBounds %= Map.insert tVar' (tVar `State.readBoundsFrom` kindBounds)
  Set.difference consts kinds `for_` \tVar -> do
    tVar' <- makeFloated tVar
    State.constingBounds %= Map.insert tVar' (tVar `State.readBoundsFrom` constBounds)
  Set.difference kinds consts `for_` \tVar -> do
    tVar' <- makeFloated tVar
    State.kindingBounds %= Map.insert tVar' (tVar `State.readBoundsFrom` kindBounds)
  where
    makeFloated tVar = do
      int <- nextHandleCounter
      return tVar {tVarId = int, tVarParent = NoType}

reParent :: Data d => TypeVar -> Set TypeVar -> d -> d
reParent newParent oldParents
  | oldParents == Set.singleton newParent = id
reParent newParent oldParents = go
  where
    go :: Data d => d -> d
    go = tVarCase *|* gmapT go
    tVarCase tVar@TypeVar {tVarParent = parent}
      | parent `Set.member` oldParents = tVar {tVarParent = newParent}
      | otherwise = tVar
    tVarCase NoType = NoType

toUnifsComponent :: Facts -> Inferencer Facts
toUnifsComponent [] = return []
toUnifsComponent (Fact (InstType (VarType scheme) inst):others) =
  State.fromOldName scheme >>= State.lookupScheme >>= \case
    Just (tVars :. facts :=> t) -> do
      instSubst <- refresh tVars
      let facts' = Fact . apply instSubst <$> facts
          t' = instSubst `apply` t
      ((Fact (inst `Equality` t') : facts') <>) <$> toUnifsComponent others
    Nothing -> (Fact (VarType scheme `Equality` inst) :) <$> toUnifsComponent others
toUnifsComponent (fact:others) = (fact :) <$> toUnifsComponent others

closeScheme :: Facts -> Set TypeVar -> Inferencer Facts
closeScheme facts tVars = do
  let tVarsList = Set.toList tVars
  parent <- getParent
  relevantVars <- Set.toList . mappend (freeTypeVars facts) . Set.unions <$> traverse State.collectPrimeTVarsAll tVarsList
  relevantTypes <- traverse State.reconstruct relevantVars
  constings <- traverse State.getConsting relevantVars
  kindings <- traverse State.getKinding relevantVars
  typings <- liftA2 (zip3 relevantVars) (traverse State.reconstruct relevantVars) (traverse State.getTyping relevantVars)
  let toTrivialDeps = fmap $ second Set.singleton . dupe
  subConsts <-
    uses State.subConsting $ filter (presentIn tVarsList) .
    (toTrivialDeps constings <>) .
    Map.toList
  subKinds <-
    uses State.subKinding $ filter (presentIn tVarsList) . (toTrivialDeps kindings <>) .
    Map.toList
  let constingsSubst = Map.fromList $ zip constings relevantTypes
      constingFacts =
        [ constEquality t' t
        | (t', t) <- zip (constingsSubst `apply` fmap toType constings) relevantTypes
        , t' /= t
        ]
      kindingsSubst = Map.fromList $ zip kindings relevantTypes
      kindingFacts =
        [ kindEquality t' t
        | (t', t) <- zip (kindingsSubst `apply` fmap toType kindings) relevantTypes
        , t' /= t
        ]
  constFacts <-
    translateSubs
      State.constingBounds
      constingsSubst
      SubConst
      ConstnessBounds
      subConsts
  kindFacts <-
    translateSubs
      State.kindingBounds
      kindingsSubst
      SubKind
      (KindBounds . fmap Ordered)
      subKinds
  (determineFacts, factsRest) <-
    do let createTyping' t = do
             tVar <- simplify t
             State.reconstruct tVar
           construct (Fact (ClassConstraint name t):others) = do
             t' <- createTyping' t
             let pair = (t', ClassConstraint name t')
             (_1 %~ (pair :)) <$> construct others
           construct (fact:others) = (_2 %~ (fact :)) <$> construct others
           construct [] = return ([], [])
       (factPairs, others) <- construct facts
       let (filtered, rest) =
             partition
               (any
                  ((`Set.member` Set.fromList relevantVars) `fOr` parentedBy parent `fOr`
                   (`Set.member` tVars)) .
                freeTypeVars .
                view _1)
               factPairs
       return (view _2 <$> filtered, (Fact . view _2 <$> rest) <> others)
  let typingFacts =
        [ typingEquality t t'
        | (t, t'', t') <- typings
        , let useFilter =
                (`Set.member` freeTypeVars t'') `fOr` parentedBy parent `fOr`
                   (`Set.member` tVars) `fOr` (`Set.member` freeTypeVars facts')
        , toType t /= t'
        , useFilter t
        ]
      facts' =
        determineFacts <> constingFacts <> constFacts <>
        kindingFacts <>
        kindFacts
  tVars `for_` \tVar -> do
    t <- State.reconstruct tVar
    let scheme =
          Set.filter
            ((`Set.member` freeTypeVars t) `fOr` parentedBy parent `fOr`
             (`Set.member` tVars))
            (freeTypeVars facts' <> freeTypeVars t) :.
          (typingFacts <> facts') :=>
          t
    registerScheme tVar scheme
  return factsRest
  where
    translateSubs _ _ _ _ [] = return []
    translateSubs bounds reconstructor subConstr boundsConstr ((tVar, limits):others) =
      uses bounds (tVar `Map.lookup`) >>= \case
        Nothing -> translateOthers
        Just bounds' ->
          (boundsConstr bounds' t :) . (facts' <>) <$> translateOthers
      where
        t = reconstructor `apply` toType tVar
        limits' = filter (/= tVar) $ Set.toList limits
        translateOthers =
          translateSubs bounds reconstructor subConstr boundsConstr others
        facts' = subConstr t . apply reconstructor . toType <$> limits'
    parentedBy parent TypeVar {tVarParent = par} = parent `predecessor` par
    parentedBy _ _ = False
    presentIn where' (key, _) = key `Set.member` Set.fromList where'

registerScheme :: TypeVar -> Scheme Type -> Inferencer ()
registerScheme tVar =
  \case
    tVars :. facts :=> nesteds -> do
      subst <- refresh tVars
      let tVars' = apply subst `Set.map` tVars
          nesteds' = apply subst nesteds
      void . reduceMany $ Fact <$> facts
      State.addScheme tVar $ tVars' :. apply subst facts :=> nesteds'

closeSCCs ::
     Facts -> [SCC (Fact, TypeVar, [TypeVar])] -> Inferencer (Bool, Facts)
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
      State.typings %= Bimap.fromList . rePar . Bimap.toList
      State.typeProps %= Bimap.fromList . rePar . Bimap.toList
      State.subConsting %= rePar
      State.constingBounds %= rePar
      State.subKinding %= rePar
      State.kindingBounds %= rePar
      State.renaming %= rePar
      State.pushParent parent
      (_, facts') <-
        traverse transformFact (fmap rePar trios) >>= reduceMany .
        (<> fmap rePar facts) .
        concat
      parents' <- uses State.renaming ((<$> parents) . apply)
      boundBySignatureVars <- Set.unions <$> State.collectPrimeTVarsAll `traverse` parents'
      minimizeSubs parent boundBySignatureVars
      minimizeFree boundBySignatureVars
      _ <- fixAll
      parent' <- uses State.renaming (`apply` parent)
      State.popParent *> State.pushParent parent'
      floatSubs boundBySignatureVars
      _ <- fixAll
      parent'' <- uses State.renaming (`apply` parent')
      State.popParent *> State.pushParent parent''
      (_, facts'') <- fixFacts facts' >>= reduceMany
      parent''' <- uses State.renaming (`apply` parent'')
      State.popParent *> State.pushParent parent'''
      parents'' <- uses State.renaming ((<$> parents) . apply)
      facts''' <- closeScheme facts'' $ Set.fromList parents''
      (_1 .~ True) <$> (State.popParent *> closeSCCs facts''' others)
    transformFact (NestedFact (_ :. [fact] :=> fs), _, _) = do
      fact' <- uses State.renaming (`apply` fact)
      (Fact fact' :) <$> toUnifsComponent fs
    transformFact _ = logicError
    getParents =
      \case
        [] -> []
        (_, tVar, _):rest -> tVar : getParents rest

makeCallGraph :: Facts -> (Facts, [SCC (Fact, TypeVar, [TypeVar])])
makeCallGraph = (_2 %~ stronglyConnCompR) . foldr transform ([], [])
  where
    transform fact =
      case fact of
        NestedFact (_ :. [Equality (VarType tVar) _] :=> fs) ->
          _2 %~ ((fact, tVar, foldr out [] fs) :)
        NestedFact (_ :. [Equality {}] :=> _) -> logicError
        _ -> _1 %~ (fact :)
    out fact =
      case fact of
        Fact (InstType (VarType scheme) _) -> (scheme :)
        Fact InstType {} -> logicError
        _ -> id

collectCounts :: Facts -> Subst Int
collectCounts = foldr countIn mempty
  where
    countIn =
      \case
        NestedFact (_ :. [Equality (VarType tVar) _] :=> _) ->
          Map.insertWith (+) tVar 1
        NestedFact (_ :. [Equality {}] :=> _) ->
          logicError
        _ -> id

collectPairs :: Way -> Int -> Map TypeVar (Set TypeVar) -> [(TypeVar, TypeVar)]
collectPairs way propss from = pairs <&> both %~ \i -> fromJust $ i `Map.lookup` varMap
  where
    edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> Map.toList from)
    varMap =
      Map.fromList $ (\tVar -> (tVarId tVar, tVar)) <$>
      uncurry (<>) (unzip edges)
    graph = Graph.buildG (1, propss) $ (both %~ tVarId) <$> edges
    vs = Map.keys varMap
    list' cond = [(v, v') | v <- vs, v' <- vs, cond v v']
    pairs =
      list' $ \v v' ->
        case way of
          Forward -> v /= v' && Graph.path graph v v'
          Backward -> v /= v' && Graph.path graph v' v
          Both -> v < v' && Graph.path graph v v' && Graph.path graph v' v

deduceUnifs :: Int -> Map TypeVar (Set TypeVar) -> Map TypeVar TypeVar
deduceUnifs propss which = go pairs mempty
  where
    pairs = collectPairs Both propss which
    go [] subst = subst
    go ((tVar, tVar'):others) subst =
      case apply subst tVar `unifyLax` apply subst tVar' of
        Left _ -> logicError
        Right (subst', _) -> go others $ subst' `apply` subst

propagateBounds ::
     (Lattice a, Bounded a)
  => Lens' InferencerState (Map TypeVar (Bounds a))
  -> Getter InferencerState (Map TypeVar (Set TypeVar))
  -> Inferencer ()
propagateBounds which by = do
  pairs <- liftA2 (collectPairs Forward) (use handleCounter) (use by)
  for_ pairs $ \(sup, sub) -> do
    uses which (sub `Map.lookup`) >>=
      traverse_
        ((which %=) . Map.insertWith (<>) sup . (upperBound .~ maxBound))
    uses which (sup `Map.lookup`) >>=
      traverse_
        ((which %=) . Map.insertWith (<>) sub . (lowerBound .~ minBound))

boundsUnifs ::
     (PartialOrd a, Eq a, Ord (Ordered a), Bounded a)
  => Getter InferencerState (Map TypeVar (Bounds a))
  -> Inferencer (Subst TypeVar)
boundsUnifs which = do
  whichList <- filter (isTrivialOrAbsurd . view _2) <$> uses which Map.toList
  let trivialGroups =
        mapFold $ (_2 %~ Set.singleton) . (_1 %~ Ordered . normalizeAbsurd) .
        swap <$>
        whichList
  let nontrivialTrivialGroups =
        (Set.toList <$>) . Map.elems $
        Map.filter ((> 1) . Set.size) trivialGroups
  return $ go nontrivialTrivialGroups mempty
  where
    go ((x:y:others):rest) subst =
      case apply subst x `unifyLax` apply subst y of
        Left _ -> logicError
        Right (subst', _) -> go ((x : others) : rest) $ subst' `apply` subst
    go (_:rest) subst = go rest subst
    go [] subst = subst

registerKind :: Text -> Inferencer DataKind
registerKind = notYetImplemented

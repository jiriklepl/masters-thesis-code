{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

-- TODO: add the overlap check for instances
module CMM.Inference where


import safe Control.Applicative (Applicative(liftA2))
import safe Control.Lens (Lens')
import safe Control.Lens.Getter (Getter, (^.), use, uses, view)
import safe Control.Lens.Setter ((%=), (%~), (.=), (.~))
import safe Control.Lens.Traversal (both)
import safe Control.Lens.Tuple (_1, _2)
import safe Control.Monad ((>=>), when)
import safe Data.Bifunctor (second, Bifunctor (first))
import safe Data.Data (Data(gmapT))
import safe Data.Foldable (Foldable(foldl'), for_, traverse_)
import safe Data.Functor (($>), (<&>), void)
import safe Data.Graph (SCC(AcyclicSCC, CyclicSCC), stronglyConnCompR)
import safe qualified Data.Graph as Graph
import safe Data.List
  ( partition




  )
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
  , FlatFact(ClassConstraint, ClassFact, ClassFunDeps, ConstUnion,
         ConstnessBounds, FactComment, InstType, KindBounds, KindUnion,
         Lock, OnRegister, SubConst, SubKind, SubType, Typing, Union)
  , FlatFacts
  , NestedFact(Fact, NestedFact)
  , Qual((:=>))
  , Scheme((:.))
  , classConstraint
  , constUnion
  , kindUnion
  , subConst
  , subKind
  , typeConstraint
  , typeUnion
  )
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter (getHandleCounter, nextHandleCounter)
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , TypeHole(EmptyTypeHole, LVInstTypeHole, MemberTypeHole,
         MethodTypeHole, NamedTypeHole, SimpleTypeHole)
  )
import safe CMM.Inference.Refresh (Refresher(refresher))
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
  , Type(ComplType, ErrorType, VarType)
  , foldApp
  , unfoldApp
  )
import safe CMM.Inference.TypeAnnot (TypeAnnot(NoTypeAnnot))
import safe CMM.Inference.TypeCompl
  ( PrimType
  , TypeCompl(AddrType, AppType, FunctionType, TupleType)
  )
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , consting
  , handleId
  , initTypeHandle
  , kinding
  , typing
  )
import safe CMM.Inference.TypeKind (TypeKind(GenericType), getTypeKind)
import safe CMM.Inference.TypeVar
  ( TypeVar(NoType, TypeVar, tVarId, tVarParent)
  , overLeaf
  , predecessor
  )
import safe CMM.Inference.Unify (instanceOf, unify, unifyFold, unifyLax)
import safe CMM.Inference.Utils (fieldClassPrefix, trileanSeq, funDepsClassPrefix)
import safe CMM.Utils (HasCallStack, addPrefix, getPrefix, hasPrefix)
import safe qualified CMM.Inference.State.Impl as State

class FactCheck a where
  factCheck :: a -> Inferencer ()

instance (Foldable t, FactCheck a) => FactCheck (t a) where
  factCheck = traverse_ factCheck

instance {-# OVERLAPPING #-} FactCheck (TypeCompl Type) where
  factCheck =
    \case
      TupleType ts -> factCheck ts
      FunctionType ts t -> factCheck ts *> factCheck t
      AppType t t' -> factCheck t *> factCheck t'
      AddrType t -> factCheck t
      _ -> return ()

instance FactCheck Type where
  factCheck =
    \case
      VarType t -> factCheck t
      ComplType t -> factCheck t
      _ -> return ()

instance {-# OVERLAPPING #-} FactCheck Fact where
  factCheck fact =
    case fact of
      Fact f -> factCheck f
      NestedFact (tVars :. facts :=> fact') ->
        factCheck tVars *> factCheck facts *> factCheck fact'

instance FactCheck TypeVar where
  factCheck tVar = do
    State.handlize %= Bimap.tryInsert tVar (initTypeHandle NoTypeAnnot tVar)

elaborate :: PrimType -> Inferencer (TypeCompl Type)
elaborate primType = do
  handles <- use State.handlize
  let oneLevel t = maybe (VarType t) (view typing) (t `Bimap.lookup` handles)
  return $ oneLevel <$> primType

simplify :: HasCallStack => Type -> Inferencer TypeVar
simplify =
  \case
    ErrorType _ -> undefined
    VarType tVar -> do
      uses State.handlize (tVar `Bimap.lookup`) >>= \case
        Just _ -> return tVar
        Nothing -> State.handlizeTVar tVar
    ComplType complType ->
      traverse simplify complType >>= \primType ->
        uses State.typize (primType `Bimap.lookupR`) >>= \case
          Nothing -> do
            tVar <- State.freshTypeHelperWithHandle $ getTypeKind primType
            State.typize %= Bimap.insert tVar primType
            t <- elaborate primType
            State.handlize %= Bimap.adjust (typing .~ ComplType t) tVar
            return tVar
          Just tVar -> return tVar

pushTyping :: TypeHandle -> TypeHandle -> Inferencer ()
pushTyping handle handle' = do
  let t = handle ^. typing
      t' = handle' ^. typing
  case unify t t' of
    Left errs -> State.addUnificationErrors errs
    Right (subst, _) -> do
      let fixIt = do
            fixTypize >>= \case
              True ->
                fixHandlize >>= \case
                  True -> fixIt
                  False -> fixSubs
              False -> fixSubs
      safeHandlizeUpdate (_2 . typing %~ apply subst) >>= flip when (void fixIt)

mineAST :: (HasTypeHole a, Foldable n) => n a -> Inferencer ()
mineAST = traverse_ (addHandles . getTypeHole)
  where
    addHandle handle = State.handlize %= Bimap.insert (handleId handle) handle
    addHandles =
      \case
        EmptyTypeHole -> return ()
        NamedTypeHole handle _ -> addHandle handle
        SimpleTypeHole handle -> addHandle handle
        LVInstTypeHole handle handle' -> addHandle handle *> addHandle handle'
        MethodTypeHole handle handle' handle'' ->
          addHandle handle *> addHandle handle' *> addHandle handle''
        MemberTypeHole handle classHandles handles handles' ->
          addHandle handle *> traverse_ addHandle handles *> traverse_ (addHandle . snd) classHandles *>
          traverse_ addHandle handles'

fixAll :: Inferencer Bool
fixAll = do
  results <- sequence [fixTypize, fixHandlize, fixSubs]
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
  unifs' <- use State.unifs
  let fixBoth ::
           (Bounded a, Eq a, Ord (Ordered a), Lattice a)
        => Lens' InferencerState (Map TypeVar (Set TypeVar))
        -> Lens' InferencerState (Map TypeVar (Bounds a))
        -> Inferencer (Subst TypeVar)
      fixBoth sub bounds = do
        fixSubGraph False sub unifs'
        fixBounds False bounds unifs'
        subst <- subUnifs
        fixSubGraph True sub subst
        fixBounds True bounds $ subst `apply` unifs'
        propagateBounds bounds sub
        boundsUnifs bounds >>= go subst
        where
          subUnifs = liftA2 deduceUnifs getHandleCounter (use sub)
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
    else safeHandlizeUpdate $ (_2 . kinding %~ apply subst) .
         (_2 . consting %~ apply subst')

unsafeTypizeUpdate ::
     ((TypeVar, PrimType) -> (TypeVar, PrimType)) -> Inferencer ()
unsafeTypizeUpdate change =
  State.typize %= Bimap.fromList . (change <$>) . Bimap.toList

unsafeHandlizeUpdate ::
     ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle)) -> Inferencer ()
unsafeHandlizeUpdate change =
  State.handlize %= Bimap.fromList . (change <$>) . Bimap.toList

isSingleton :: Set a -> Bool
isSingleton = (== 1) . Set.size

safeHandlizeUpdate ::
     ((TypeVar, TypeHandle) -> (TypeVar, TypeHandle)) -> Inferencer Bool
safeHandlizeUpdate change = do
  handles' <- uses State.handlize ((change <$>) . Bimap.toList)
  let handlize' = Bimap.fromList handles'
      collapsedKeys = mapCollect handles'
      collapsedValues = mapCollect (swap <$> handles')
      goValues [] tSubst cSubst kSubst = return (tSubst, cSubst, kSubst)
      goValues ((handle:handle':handles):others) tSubst cSubst kSubst = do
        (tSubst', _) <-
          apply tSubst (handle ^. typing) `unify`
          apply tSubst (handle' ^. typing)
        (cSubst', _) <-
          apply cSubst (handle ^. consting) `unifyLax`
          apply cSubst (handle' ^. consting)
        (kSubst', _) <-
          apply kSubst (handle ^. kinding) `unifyLax`
          apply kSubst (handle' ^. kinding)
        goValues
          ((handle' : handles) : others)
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
  State.handlize .= handlize'
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
          State.unifs %= apply subst
          fixSubGraph True State.subConsting cSubst
          fixSubGraph True State.subKinding kSubst
          fixBounds True State.constingBounds cSubst
          fixBounds True State.kindingBounds kSubst
          (True <$) . safeHandlizeUpdate $ apply subst .
            (_2 . typing %~ apply tSubst) .
            (_2 . consting %~ applyShallow cSubst) .
            (_2 . kinding %~ applyShallow kSubst)

fixHandlize :: Inferencer Bool
fixHandlize = uses State.unifs apply >>= safeHandlizeUpdate

mapCollect :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
mapCollect = Map.fromListWith mappend . fmap (_2 %~ Set.singleton)

onCandidates :: [t1] -> (t1 -> t2 -> Maybe a) -> t2 -> Maybe a
onCandidates (candidate:candidates) lookup' database =
  case candidate `lookup'` database of
    Just result -> return result
    Nothing -> onCandidates candidates lookup' database
onCandidates [] _ _ = Nothing

fixTypize :: Inferencer Bool
fixTypize = do
  types' <- uses State.unifs (fmap . apply) <*> uses State.typize Bimap.toList
  let typize' = Bimap.fromList types'
      collapsedKeys = mapCollect types'
      collapsedValues = mapCollect $ swap <$> types'
    -- TODO: reduce duplication; consider continuing despite errors
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
  State.typize .= typize'
  if all isSingleton collapsedKeys && all isSingleton collapsedValues
    then return False
    else do
      subst <-
        goValues
          (Set.toList . snd <$> Map.toList collapsedValues)
          (mempty :: Subst TypeVar) >>=
        goKeys (Set.toList . snd <$> Map.toList collapsedKeys)
      State.unifs %= apply subst
      True <$ fixTypize

fixFacts :: Facts -> Inferencer Facts
fixFacts facts = uses State.unifs $ flip fmap facts . apply

wrapParent :: HasCallStack => FlatFacts -> Inferencer a -> Inferencer a
wrapParent flatFacts wrapped =
  case determineParent flatFacts of
    Just parent -> State.pushParent parent *> wrapped <* State.popParent
    Nothing -> wrapped
  where
    determineParent [VarType tVar `Union` _] = Just tVar
    determineParent _ = undefined

unwrapParent :: Inferencer a -> Inferencer a
unwrapParent unwrapped = do
  parent <- getParent
  State.popParent *> unwrapped <* State.pushParent parent

flattenFacts :: HasCallStack => [NestedFact Type] -> FlatFacts
flattenFacts = fmap go
  where
    go (Fact fact) = fact
    go _ = undefined -- TODO: error

reverseFacts :: HasCallStack => FlatFacts -> FlatFacts
reverseFacts = fmap go
  where
    go (ClassFact name t) = ClassConstraint name t
    go _ = undefined -- TODO: error

reduceTemplates :: Facts -> Inferencer Facts
reduceTemplates [] = return []
reduceTemplates (fact:facts) =
  case fact of
    NestedFact (tVars :. [ClassConstraint name t] :=> nesteds) -> do
      subst <- refresher tVars
      let tVars' = apply subst `Set.map` tVars
      State.addClassScheme name $ tVars' :. flattenFacts (subst `apply` nesteds) :=>
        apply subst t
      continue
    NestedFact (tVars :. [ClassFact name t] :=> nesteds)
      | hasPrefix name
      , let prefix = getPrefix name
      , prefix == fieldClassPrefix || prefix == funDepsClassPrefix -> do
        State.lookupFunDep name >>= \case
          Nothing -> skip
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
                        fs = zipWith typeUnion tVars' args
                    State.addClassFact (fromString (trileanSeq rule) `addPrefix` name) $
                      Set.fromList tVars' :.
                      fs :=>
                      t'
                  chooseArg arg tVar = trilean arg tVar tVar
      | otherwise -> do
        State.lookupClassScheme name >>= \case
          Nothing -> skip
          Just (tVars' :. facts' :=> t') -> do
            subst <- refresher tVars'
            State.addClassFact name $ (tVars <> (apply subst `Set.map` tVars')) :.
              ((t `typeUnion` apply subst t') :
               reverseFacts [f | Fact f <- nesteds] <>
               apply subst facts') :=>
              t
            subst' <- refresher tVars
            reduceTemplates $
              (Fact <$> typeUnion (subst `apply` t') (subst' `apply` t) : flattenFacts (subst' `apply` nesteds)) <> facts <> (Fact <$> reverseFacts (apply subst <$> facts'))
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
        Fact (tVar `typeConstraint` tVar') :
        Fact (tVar `subConst` tVar') :
        facts
    Fact (KindUnion t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $ Fact (tVar `subKind` tVar') : Fact (tVar' `subKind` tVar) :
        facts
    Fact (SubKind t t') -> do
      handle <- simplify t >>= State.getHandle
      handle' <- simplify t' >>= State.getHandle
      State.pushSubKind handle handle'
      continue
    Fact (SubConst t t') -> do
      handle <- simplify t >>= State.getHandle
      handle' <- simplify t' >>= State.getHandle
      State.pushSubConst handle handle'
      continue
    Fact (ConstUnion t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      continueWith $ Fact (tVar `subConst` tVar') : Fact (tVar' `subConst` tVar) :
        facts
    Fact (Typing t t') -> do
      handle <- simplify t >>= State.getHandle
      handle' <- simplify t' >>= State.getHandle
      pushTyping handle handle'
      fixFacts facts >>= continueWith
    Fact (Union t t') -> do
      tVar <- simplify t
      tVar' <- simplify t'
      case tVar `unify` tVar' of
        Left errs -> State.addUnificationErrors errs
        Right (subst, _) -> State.unifs %= (subst `apply`)
      _ <- fixAll
      fixFacts facts >>= continueWith
    Fact (ConstnessBounds bounds t) -> do
      handle <- simplify t >>= State.getHandle
      State.pushConstBounds handle bounds
      continue
    Fact (KindBounds (Bounds (Ordered minKind) (Ordered maxKind)) t) -> do
      handle <- simplify t >>= State.getHandle
      State.pushKindBounds handle $ minKind `Bounds` maxKind
      continue
    Fact (OnRegister reg t) -> do
      handle <- simplify t >>= State.getHandle
      kind <- registerKind reg
      State.pushKindBounds handle $ kind `Bounds` kind
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
    Fact (ClassConstraint name t)
      | hasPrefix name
      , let prefix = getPrefix name
      , prefix == fieldClassPrefix || prefix == funDepsClassPrefix ->
        State.lookupFunDep name >>= \case
          Nothing -> undefined -- TODO: logic error
          Just rules -> continueWith $ fmap go rules
            where go rule =
                    Fact $
                    classConstraint
                      (fromString (trileanSeq rule) `addPrefix` name)
                      t
      | otherwise ->
        State.lookupFact name >>= \case
          Nothing -> skip
          Just schemes' -> do
            tType <- simplify t >>= State.getTyping
            let go accum =
                  \case
                    (tVars :. fs :=> t'):others -> do
                      tType' <- simplify t' >>= State.getTyping
                      if tType `instanceOf` tType'
                        then do
                          subst <- refresher tVars
                          let freshT = subst `apply` t'
                              newFacts =
                                typeUnion t freshT : (apply subst <$> fs)
                          go (fmap Fact newFacts : accum) others
                        else go accum others
                    [] ->
                      case accum of
                        [h] -> continueWith h
                        _ -> skip
            go mempty schemes'
    Fact (ClassFact name t) ->
      State.lookupClassScheme name >>= \case
        Nothing -> skip
        Just (tVars :. facts' :=> t') -> do
          subst <- refresher tVars
          State.addClassFact name $ mempty :. [] :=> t
          continueWith $
            Fact <$> typeUnion t (subst `apply` t') :
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
reduceMany = repeat reduceTrivial >=> reduceTemplates . snd >=> go
  where
    go facts = do
      result@(change, facts') <- repeat reduceTrivial facts >>= reduceConstraint . snd
      if change
        then go facts'
        else return result
    repeat what facts = do
      result@(change, facts') <- what facts
      if change
        then repeat what facts'
        else return result

reduce :: Facts -> Inferencer (Bool, Facts)
reduce facts = do
  (change, facts') <- reduceMany facts
  let (facts'', sccs) = makeCallGraph facts'
  (change', facts''') <- closeSCCs facts'' sccs
  return (change || change', facts''')

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
  let (cSubsts, cRest) = laundry parent pTypeConstings constBounds constings
      cSubst = foldTVarSubsts cSubsts
      (kSubsts, kRest) = laundry parent pTypeKindings kindBounds kindings
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
    safeHandlizeUpdate
      ((_2 . consting %~ apply cSubst) . (_2 . kinding %~ apply kSubst))
  where
    transformMap pTypeThings =
      Map.toList . fmap (Set.filter (setFilter pTypeThings)) .
      Map.filterWithKey (mapFilter pTypeThings)
    setFilter pTypeThings =
      predecessor parent . tVarParent `fOr` (`Set.member` pTypeThings) `fOr`
      (`Set.member` pTypeVars)
    mapFilter pTypeThings from _ =
      tVarParent from == parent && not (from `Set.member` pTypeThings)

laundry ::
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
laundry parent pTypeThings boundsMap subGraph =
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
    getDepends _ _ = undefined -- TODO: error

minimizeBounds ::
     Bounded a
  => Lens' InferencerState (Map TypeVar (Bounds a))
  -> TypeVar
  -> Inferencer ()
minimizeBounds what tVar = do
  low <- uses what (tVar `State.readLowerBound`)
  what %= Map.insert tVar (low `Bounds` low)

freeParented :: Set TypeVar -> Inferencer ([TypeVar], [TypeVar])
freeParented ts = do
  parent <- getParent
  let
    isFreeParented tIngs with tVar@TypeVar {} =
      not (tVar `Map.member` with) && tVarParent tVar == parent &&
      not (tVar `Set.member` tIngs)
    isFreeParented _ _ NoType = False
  (consts, kinds) <- uses State.handlize $ unzip . fmap mineSubs . Bimap.elems
  subConsts <- use State.subConsting
  subKinds <- use State.subKinding
  tConsts <- fmap Set.fromList . traverse State.getConsting $ Set.toList ts
  tKinds <- fmap Set.fromList . traverse State.getKinding $ Set.toList ts
  return
    ( filter (isFreeParented tConsts subConsts) consts
    , filter (isFreeParented tKinds subKinds) kinds)
  where
    mineSubs handle = (handle ^. consting, handle ^. kinding)

minimizeFree :: Set TypeVar -> Inferencer ()
minimizeFree ts = do
  (consts, kinds) <- freeParented ts
  minimizeBounds State.constingBounds `traverse_` consts
  minimizeBounds State.kindingBounds `traverse_` kinds

floatSubs :: Set TypeVar -> Inferencer ()
floatSubs ts = do
  (consts, kinds) <- (both %~ Set.fromList) <$> freeParented ts
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

unSchematize :: Facts -> Inferencer Facts
unSchematize [] = return []
unSchematize (Fact (InstType (VarType scheme) inst):others) =
  State.fromOldName scheme >>= State.lookupScheme >>= \case
    Just (tVars :. facts :=> t) -> do
      instSubst <- refresher tVars
      let facts' = Fact . apply instSubst <$> facts
          t' = instSubst `apply` t
      ((Fact (inst `Union` t') : facts') <>) <$> unSchematize others
    Nothing -> (Fact (VarType scheme `Union` inst) :) <$> unSchematize others
unSchematize (fact:others) = (fact :) <$> unSchematize others

-- TODO: check whether all typings are set (otherwise error)
schematize :: Facts -> Set TypeVar -> Inferencer Facts
schematize facts tVars = do
  let tVarsList = Set.toList tVars
  parent <- getParent
  x <- Set.toList . Set.unions <$> traverse State.collectPrimeTVarsAll tVarsList
  reX <- traverse State.reconstruct x
  constings <- traverse State.getConsting x
  kindings <- traverse State.getKinding x
  -- TODO: leave out trivial
  typings <- liftA2 (zip3 x) (traverse State.reconstruct x) (traverse State.getTyping x)
  let toTrivialDeps = fmap $ second Set.singleton . dupe
  subConsts <-
    uses State.subConsting $ filter (presentIn tVarsList) .
    (toTrivialDeps constings <>) .
    Map.toList
  subKinds <-
    uses State.subKinding $ filter (presentIn tVarsList) . (toTrivialDeps kindings <>) .
    Map.toList
  let constingsSubst = Map.fromList $ zip constings reX
      constingFacts =
        [ constUnion t' t
        | (t', t) <- zip (constingsSubst `apply` fmap toType constings) reX
        , t' /= t
        ]
      kindingsSubst = Map.fromList $ zip kindings reX
      kindingFacts =
        [ kindUnion t' t
        | (t', t) <- zip (kindingsSubst `apply` fmap toType kindings) reX
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
    do let elaborate' t = do
             tVar <- simplify t
             State.reconstruct tVar
           construct (Fact (ClassConstraint name t):others) = do
             t' <- elaborate' t
             let pair = (t', ClassConstraint name t')
             (_1 %~ (pair :)) <$> construct others
           construct (fact:others) = (_2 %~ (fact :)) <$> construct others
           construct [] = return ([], [])
       (factPairs, others) <- construct facts
       let (filtered, rest) =
             partition
               (any
                  ((`Set.member` Set.fromList x) `fOr` parentedBy parent `fOr`
                   (`Set.member` tVars)) .
                freeTypeVars .
                view _1)
               factPairs
       return (view _2 <$> filtered, (Fact . view _2 <$> rest) <> others)
  let typingFacts =
        [ typeConstraint t t'
        | (t, t'', t') <- typings
        , let useFilter =
                (`Set.member` freeTypeVars t'') `fOr` parentedBy parent
        , toType t /= t'
        , useFilter t
        ]
      facts' =
        typingFacts <> determineFacts <> constingFacts <> constFacts <>
        kindingFacts <>
        kindFacts
  tVars `for_` \tVar -> do
    t <- State.reconstruct tVar
    let scheme =
          Set.filter
            ((`Set.member` freeTypeVars t) `fOr` parentedBy parent `fOr`
             (`Set.member` tVars))
            (freeTypeVars facts' <> freeTypeVars t) :.
          facts' :=>
          t
    State.addScheme tVar scheme
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
    parentedBy parent TypeVar {tVarParent = par} = par `overLeaf` parent
    parentedBy _ _ = False
    presentIn where' (key, _) = key `Set.member` Set.fromList where'

registerScheme :: TypeVar -> Scheme Type -> Inferencer ()
registerScheme tVar =
  \case
    tVars :. facts :=> nesteds -> do
      subst <- refresher tVars
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
      State.typize %= Bimap.fromList . rePar . Bimap.toList
      State.handlize %= Bimap.fromList . rePar . Bimap.toList
      State.subConsting %= rePar
      State.constingBounds %= rePar
      State.subKinding %= rePar
      State.kindingBounds %= rePar
      State.unifs %= rePar
      State.pushParent parent
      (_, facts') <-
        traverse transformFact (fmap rePar trios) >>= reduceMany .
        (<> fmap rePar facts) .
        concat
      parents' <- uses State.unifs ((<$> parents) . apply)
      x <- Set.unions <$> State.collectPrimeTVarsAll `traverse` parents'
      --  error $ show x
      minimizeSubs parent x
      minimizeFree x
      _ <- fixAll
      parent' <- uses State.unifs (`apply` parent)
      State.popParent *> State.pushParent parent'
      floatSubs x
      _ <- fixAll
      parent'' <- uses State.unifs (`apply` parent')
      State.popParent *> State.pushParent parent''
      (_, facts'') <- fixFacts facts' >>= reduceMany
      parent''' <- uses State.unifs (`apply` parent'')
      State.popParent *> State.pushParent parent'''
      parents'' <- uses State.unifs ((<$> parents) . apply)
      facts''' <- schematize facts'' $ Set.fromList parents''
      (_1 .~ True) <$> (State.popParent *> closeSCCs facts''' others)
    transformFact (NestedFact (_ :. [fact] :=> fs), _, _) = do
      fact' <- uses State.unifs (`apply` fact)
      (Fact fact' :) <$> unSchematize fs
    transformFact _ = undefined
    getParents =
      \case
        [] -> []
        (_, tVar, _):rest -> tVar : getParents rest

makeCallGraph :: Facts -> (Facts, [SCC (Fact, TypeVar, [TypeVar])])
makeCallGraph = (_2 %~ stronglyConnCompR) . foldr transform ([], [])
  where
    transform fact =
      case fact of
        NestedFact (_ :. [Union (VarType tVar) _] :=> fs) ->
          _2 %~ ((fact, tVar, foldr out [] fs) :)
        NestedFact (_ :. [Union {}] :=> _) -> undefined -- TODO: logic error, broken contract
        _ -> _1 %~ (fact :)
    out fact =
      case fact of
        Fact (InstType (VarType scheme) _) -> (scheme :)
        Fact InstType {} -> undefined -- TODO: logic error, broken contract
        _ -> id

collectCounts :: Facts -> Subst Int
collectCounts = foldr countIn mempty
  where
    countIn =
      \case
        NestedFact (_ :. [Union (VarType tVar) _] :=> _) ->
          Map.insertWith (+) tVar 1
        NestedFact (_ :. [Union {}] :=> _) ->
          undefined -- TODO: logic error, broken contract
        _ -> id

collectPairs :: Way -> Int -> Map TypeVar (Set TypeVar) -> [(TypeVar, TypeVar)]
collectPairs way handles from = pairs <&> both %~ \i -> fromJust $ i `Map.lookup` varMap
  where
    edges = concat ((\(f, t) -> (f, ) <$> Set.toList t) <$> Map.toList from)
    varMap =
      Map.fromList $ (\tVar -> (tVarId tVar, tVar)) <$>
      uncurry (<>) (unzip edges)
    graph = Graph.buildG (1, handles) $ (both %~ tVarId) <$> edges
    vs = Map.keys varMap
    list' cond = [(v, v') | v <- vs, v' <- vs, cond v v']
    pairs =
      list' $ \v v' ->
        case way of
          Forward -> v /= v' && Graph.path graph v v'
          Backward -> v /= v' && Graph.path graph v' v
          Both -> v < v' && Graph.path graph v v' && Graph.path graph v' v

deduceUnifs :: Int -> Map TypeVar (Set TypeVar) -> Map TypeVar TypeVar
deduceUnifs handles which = go pairs mempty
  where
    pairs = collectPairs Both handles which
    go [] subst = subst
    go ((tVar, tVar'):others) subst =
      case apply subst tVar `unifyLax` apply subst tVar' of
        Left _ -> undefined -- logic error
        Right (subst', _) -> go others $ subst' `apply` subst

propagateBounds ::
     (Lattice a, Bounded a)
  => Lens' InferencerState (Map TypeVar (Bounds a))
  -> Getter InferencerState (Map TypeVar (Set TypeVar))
  -> Inferencer ()
propagateBounds which by = do
  pairs <- liftA2 (collectPairs Forward) getHandleCounter (use by)
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
        Left _ -> undefined -- logic error
        Right (subst', _) -> go ((x : others) : rest) $ subst' `apply` subst
    go (_:rest) subst = go rest subst
    go [] subst = subst

registerKind :: Text -> Inferencer DataKind
registerKind = undefined

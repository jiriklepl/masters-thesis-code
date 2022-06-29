{-# LANGUAGE Safe #-}

module CMM.Inference.State
  ( module CMM.Inference.State.Impl
  , module CMM.Inference.State
  ) where

import safe Control.Lens.Getter ((^.), uses, view)
import safe Control.Lens.Setter ((%=), (<>=))
import safe Control.Monad (filterM, unless)
import safe Data.Data (Data)
import safe Data.Foldable (fold, traverse_)
import safe Data.Foldable.Extra (anyM)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromJust, fromMaybe, isJust)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe Prettyprinter (Pretty(pretty))

import safe qualified CMM.Data.Bimap as Bimap
import safe CMM.Data.Bounds (Bounds(Bounds), lowerBound, upperBound)
import safe CMM.Data.Trilean (Trilean)
import safe CMM.Err.Error (Error(Error))
import safe CMM.Err.Severity (Severity(ErrorLevel))
import safe CMM.Err.State (ErrorState(ErrorState), HasErrorState(errorState))
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (Qual((:=>)), Scheme((:.)))
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.FunDeps (funDepsSimplify, addTrivialDep)
import safe CMM.Inference.Subst (apply)
import safe CMM.Inference.Type (ToType(toType), Type(ComplType, VarType))
import safe CMM.Inference.TypeHandle (TypeHandle, consting, kinding, typing)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Unify.Error (UnificationError)
import safe CMM.Utils (HasCallStack, backQuoteShow)

import safe CMM.Inference.State.Impl
  ( Inferencer
  , InferencerState(InferencerState))
import safe qualified CMM.Inference.State.Impl as State

pushParent :: TypeVar -> Inferencer ()
pushParent parent = State.currentParent %= (parent :)

popParent :: Inferencer ()
popParent = State.currentParent %= tail

getHandle :: TypeVar -> Inferencer TypeHandle
getHandle = fmap fromJust . tryGetHandle

tryGetHandle :: TypeVar -> Inferencer (Maybe TypeHandle)
tryGetHandle tVar =
  uses State.handlize (flip Bimap.lookup) <*> uses State.unifs (`apply` tVar)

readBoundsFrom :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> Bounds a
readBoundsFrom = (fromMaybe (minBound `Bounds` maxBound) .) . Map.lookup

readLowerBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readLowerBound = (view lowerBound .) . readBoundsFrom

readUpperBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readUpperBound = (view upperBound .) . readBoundsFrom

getConsting :: TypeVar -> Inferencer TypeVar
getConsting tVar = view consting <$> getHandle tVar

readConstingBounds :: TypeVar -> Inferencer (Bounds Constness)
readConstingBounds tVar = uses State.constingBounds (readBoundsFrom tVar)

readKindingBounds :: TypeVar -> Inferencer (Bounds DataKind)
readKindingBounds tVar = uses State.kindingBounds (readBoundsFrom tVar)

getKinding :: TypeVar -> Inferencer TypeVar
getKinding tVar = view kinding <$> getHandle tVar

getTyping :: TypeVar -> Inferencer Type
getTyping tVar = view typing <$> getHandle tVar

collectPrimeTVars :: TypeVar -> Inferencer (Set TypeVar)
collectPrimeTVars tVar =
  uses State.typize (Bimap.lookup tVar) >>= \case
    Just primType -> fold <$> traverse collectPrimeTVars primType
    Nothing -> return $ Set.singleton tVar

collectPrimeTVarsAll :: TypeVar -> Inferencer (Set TypeVar)
collectPrimeTVarsAll tVar =
  uses State.typize (Bimap.lookup tVar) >>= \case
    Just primType ->
      Set.insert tVar . fold <$> traverse collectPrimeTVarsAll primType
    Nothing -> return $ Set.singleton tVar

infix 6 `insertEdge`

insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

pushSubKind :: TypeHandle -> TypeHandle -> Inferencer ()
pushSubKind handle handle' =
  State.subKinding %= view kinding handle `insertEdge` view kinding handle'

pushSubConst :: TypeHandle -> TypeHandle -> Inferencer ()
pushSubConst handle handle' =
  State.subConsting %= view consting handle `insertEdge` view consting handle'

pushKindBounds :: TypeHandle -> Bounds DataKind -> Inferencer ()
pushKindBounds handle bounds =
  State.kindingBounds %= Map.insertWith (<>) (handle ^. kinding) bounds

addFunDeps :: Text -> [[Trilean]] -> Inferencer ()
addFunDeps name rules = State.funDeps %= Map.insert name (addTrivialDep $ funDepsSimplify rules)

hasFunDeps :: Text -> Inferencer Bool
hasFunDeps = fmap isJust . lookupFunDep

lookupFunDep :: Text -> Inferencer (Maybe [[Trilean]])
lookupFunDep = uses State.funDeps . Map.lookup

pushConstBounds :: TypeHandle -> Bounds Constness -> Inferencer ()
pushConstBounds handle bounds =
  State.constingBounds %= Map.insertWith (<>) (handle ^. consting) bounds

lock :: (ToType a, HasCallStack) => a -> TypeVar -> Inferencer ()
lock parent tVar = State.lockedVars %= Map.insertWith msg tVar tParent -- TODO msg
  where
    tParent = toType parent
    msg new old =
      error $
      backQuoteShow tVar <>
      " already locked by " <>
      backQuoteShow old <> " (attempted lock by " <> backQuoteShow new <> ")"

lockVars ::
     (ToType a, HasCallStack, Foldable f) => a -> f TypeVar -> Inferencer ()
lockVars = traverse_ . lock

ensureLocked :: TypeVar -> TypeVar -> Inferencer ()
ensureLocked parent tVar = do
  locked <- isLocked tVar
  unless locked . lockVars parent $ Set.singleton tVar

isLocked :: TypeVar -> Inferencer Bool
isLocked = uses State.lockedVars . Map.member

isUnlocked :: TypeVar -> Inferencer Bool
isUnlocked = fmap not . isLocked

lookupScheme :: TypeVar -> Inferencer (Maybe (Scheme Type))
lookupScheme = uses State.schemes . Map.lookup

lookupClassScheme :: Text -> Inferencer (Maybe (Scheme Type))
lookupClassScheme = uses State.classSchemes . Map.lookup

lookupFact :: Text -> Inferencer (Maybe [Scheme Type])
lookupFact = uses State.classFacts . Map.lookup

isOpen :: Data a => Scheme a -> Inferencer Bool
isOpen =
  \case
    tVars :. facts :=> nesteds -> isUnlocked `anyM` freeOuter
      where freeOuter = freeInner Set.\\ tVars
            freeInner = freeTypeVars facts <> freeTypeVars nesteds

getUnlockedVars :: Data a => Scheme a -> Inferencer [TypeVar]
getUnlockedVars =
  \case
    tVars :. facts :=> nesteds -> do
      filterM isUnlocked $ Set.toList freeOuter
      where freeOuter = freeInner Set.\\ tVars
            freeInner = freeTypeVars facts <> freeTypeVars nesteds

sanitizeClosed ::
     (HasCallStack, Data a, Pretty a, Pretty DataKind)
  => Scheme a
  -> Inferencer ()
sanitizeClosed scheme = do
  freeVars <- getUnlockedVars scheme
  msg freeVars -- TODO msg
  where
    msg [] = return ()
    msg freeVars =
      error $
      "scheme " <>
      show (pretty scheme) <>
      " is open, free variables: " <> show (pretty freeVars)

addClassFact ::
     HasCallStack => Text -> Scheme Type -> Inferencer ()
addClassFact name scheme =
  case scheme of
    tVars :. _ :=> t -> do
      lockVars t tVars
      State.classFacts %= Map.insertWith mappend name [scheme]

addScheme ::
     (HasCallStack, Pretty DataKind) => TypeVar -> Scheme Type -> Inferencer ()
addScheme tVar scheme =
  case scheme of
    tVars :. _ :=> _ -> do
      lockVars tVar tVars
      sanitizeClosed scheme
      State.schemes %= Map.insert tVar scheme

addClassScheme ::
     (HasCallStack, Pretty DataKind) => Text -> Scheme Type -> Inferencer ()
addClassScheme name scheme =
  case scheme of
    tVars :. _ :=> t -> do
      lockVars t tVars
      sanitizeClosed scheme
      State.classSchemes %= Map.insertWith msg name scheme -- TODO msg
  where
    msg new old =
      error $
      "class " <>
      backQuoteShow name <>
      " already registered with scheme " <>
      backQuoteShow old <> " (attempted adding " <> backQuoteShow new <> ")"

fromOldName :: TypeVar -> Inferencer TypeVar
fromOldName tVar = uses State.unifs (`apply` tVar)

reconstruct :: TypeVar -> Inferencer Type
reconstruct tVar =
  uses State.typize (Bimap.lookup tVar) >>= \case
    Just primType -> ComplType <$> traverse reconstruct primType
    Nothing -> return $ VarType tVar

reconstructOld :: TypeVar -> Inferencer Type
reconstructOld tVar = fromOldName tVar >>= reconstruct

addUnificationErrors :: [UnificationError] -> Inferencer ()
addUnificationErrors errs =
  errorState <>= ErrorState (Error ErrorLevel <$> errs)

{-# LANGUAGE Safe #-}

module CMM.Inference.State
  ( module CMM.Inference.State.Impl
  , module CMM.Inference.State
  ) where

import safe Control.Lens ((%=), (<>=), (^.), uses, view)
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

import safe qualified CMM.Data.Bimap as Bimap
import safe CMM.Data.Bounds (Bounds(Bounds), lowerBound, upperBound)
import safe CMM.Data.Trilean (Trilean)
import safe CMM.Err.Error (Error(Error))
import safe CMM.Err.Severity (Severity(ErrorLevel))
import safe CMM.Err.State (ErrorState(ErrorState), HasErrorState(errorState))
import safe CMM.Inference.BuiltIn ()
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (Qual((:=>)), Scheme((:.)))
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.FunDeps (addTrivialDep, funDepsSimplify)
import safe CMM.Inference.Properties (Properties, consting, kinding, typing)
import safe CMM.Inference.Subst (apply)
import safe CMM.Inference.Type (ToType(toType), Type(ComplType, VarType))
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Inference.Unify.Error (UnificationError)
import safe CMM.Utils (HasCallStack, backQuoteShow)

import safe CMM.Inference.State.Impl
  ( Inferencer
  , InferencerState(InferencerState)
  )
import safe qualified CMM.Inference.State.Impl as State

-- | adds a new parent to the list of parents of the nested contexts
pushParent :: TypeVar -> Inferencer ()
pushParent parent = State.currentParent %= (parent :)

-- | pops the top parent to the list of parents of the nested contexts
popParent :: Inferencer ()
popParent = State.currentParent %= tail

-- | gets the type properties representing the type represented by the given type variable
getProps :: HasCallStack => TypeVar -> Inferencer Properties
getProps = fmap fromJust . tryGetProps

-- | gets `Just` the type properties representing the type represented by the given type variable,
--   or `Nothing` if the type variable does represent none
tryGetProps :: TypeVar -> Inferencer (Maybe Properties)
tryGetProps tVar =
  uses State.typeProps (flip Bimap.lookup) <*>
  uses State.renaming (`apply` tVar)

-- | looks up the closest known concrete values bounding the given variable from the given database
readBoundsFrom :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> Bounds a
readBoundsFrom = (fromMaybe (minBound `Bounds` maxBound) .) . Map.lookup

-- | same as `readBoundsFrom`, but returns just the lower bound
readLowerBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readLowerBound = (view lowerBound .) . readBoundsFrom

-- | same as `readBoundsFrom`, but returns just the upper bound
readUpperBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readUpperBound = (view upperBound .) . readBoundsFrom

-- | returns the type variable representing the constness of the given type variable
getConsting :: HasCallStack => TypeVar -> Inferencer TypeVar
getConsting tVar = view consting <$> getProps tVar

-- | Same as `readBoundsFrom`, but using the monadic state for the database of constnesses
readConstingBounds :: TypeVar -> Inferencer (Bounds Constness)
readConstingBounds tVar = uses State.constingBounds (readBoundsFrom tVar)

-- | Same as `readBoundsFrom`, but using the monadic state for the database of data kinds
readKindingBounds :: TypeVar -> Inferencer (Bounds DataKind)
readKindingBounds tVar = uses State.kindingBounds (readBoundsFrom tVar)

-- | returns the type variable representing the data kind of the given type variable
getKinding :: HasCallStack => TypeVar -> Inferencer TypeVar
getKinding tVar = view kinding <$> getProps tVar

-- | returns the type representing the typing of the given type variable
getTyping :: HasCallStack => TypeVar -> Inferencer Type
getTyping tVar = view typing <$> getProps tVar

-- | returns the leaf type variables from the reconstructed definition of the type represented by the given type variable
collectPrimeTVars :: TypeVar -> Inferencer (Set TypeVar)
collectPrimeTVars tVar =
  uses State.primPatterns (Bimap.lookup tVar) >>= \case
    Just primType -> fold <$> traverse collectPrimeTVars primType
    Nothing -> return $ Set.singleton tVar

-- | returns the type variables representing EACH subterm in the reconstructed definition of the type represented by the given type variable
collectPrimeTVarsAll :: TypeVar -> Inferencer (Set TypeVar)
collectPrimeTVarsAll tVar =
  uses State.primPatterns (Bimap.lookup tVar) >>= \case
    Just primType ->
      Set.insert tVar . fold <$> traverse collectPrimeTVarsAll primType
    Nothing -> return $ Set.singleton tVar

infix 6 `insertEdge`

-- | inserts an edge between the two given elements to the given graph
insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

-- | pushes an edge to the `State.subKinding` graph
pushSubKind :: Properties -> Properties -> Inferencer ()
pushSubKind props props' =
  State.subKinding %= view kinding props `insertEdge` view kinding props'

-- | pushes an edge to the `State.subConsting` graph
pushSubConst :: Properties -> Properties -> Inferencer ()
pushSubConst props props' =
  State.subConsting %= view consting props `insertEdge` view consting props'

-- | adds the kind bounds of the given type
pushKindBounds :: Properties -> Bounds DataKind -> Inferencer ()
pushKindBounds props bounds =
  State.kindingBounds %= Map.insertWith (<>) (props ^. kinding) bounds

-- | adds the functional dependency for the given class
addFunDeps :: Text -> [[Trilean]] -> Inferencer ()
addFunDeps name rules = State.funDeps %= Map.insert name rules'
  where
    rules' = addTrivialDep $ funDepsSimplify rules

-- | returns `True` iff the given class has any functional dependencies set
hasFunDeps :: Text -> Inferencer Bool
hasFunDeps = fmap isJust . lookupFunDep

-- | looks up the functional dependency set of the given class
lookupFunDep :: Text -> Inferencer (Maybe [[Trilean]])
lookupFunDep = uses State.funDeps . Map.lookup

-- | adds the constness bounds of the given type
pushConstBounds :: Properties -> Bounds Constness -> Inferencer ()
pushConstBounds props bounds =
  State.constingBounds %= Map.insertWith (<>) (props ^. consting) bounds

-- | "locks" the given type variable (just marking it as "locked" - can be done only once for each type variable)
lock :: (ToType a, HasCallStack) => a -> TypeVar -> Inferencer ()
lock parent tVar = State.lockedVars %= Map.insertWith msg tVar tParent -- TODO msg
  where
    tParent = toType parent
    msg new old =
      error $
      backQuoteShow tVar <>
      " already locked by " <>
      backQuoteShow old <> " (attempted lock by " <> backQuoteShow new <> ")"

-- | see `lock`
lockVars ::
     (ToType a, HasCallStack, Foldable f) => a -> f TypeVar -> Inferencer ()
lockVars = traverse_ . lock

-- | see `lock`, this is a version not locking the variable if it is already locked
ensureLocked :: TypeVar -> TypeVar -> Inferencer ()
ensureLocked parent tVar = do
  locked <- isLocked tVar
  unless locked . lockVars parent $ Set.singleton tVar

-- | returns `True` iff the given type variable has been locked, see `lock`
isLocked :: TypeVar -> Inferencer Bool
isLocked = uses State.lockedVars . Map.member

-- | not `isLocked`
isUnlocked :: TypeVar -> Inferencer Bool
isUnlocked = fmap not . isLocked

-- | looks up the function scheme represented by the given type variable
lookupScheme :: TypeVar -> Inferencer (Maybe (Scheme Type))
lookupScheme = uses State.schemes . Map.lookup

-- | looks up the class scheme represented by the given type variable
lookupClassScheme :: Text -> Inferencer (Maybe (Scheme Type))
lookupClassScheme = uses State.classSchemes . Map.lookup

-- | looks up the function scheme represented by the given type variable
lookupFact :: Text -> Inferencer (Maybe [Scheme Type])
lookupFact = uses State.classFacts . Map.lookup

-- | returns `True` iff the given scheme has any unquantified unlocked type variables inside
isOpen :: Data a => Scheme a -> Inferencer Bool
isOpen =
  \case
    tVars :. facts :=> nesteds -> isUnlocked `anyM` freeOuter
      where freeOuter = freeInner Set.\\ tVars
            freeInner = freeTypeVars facts <> freeTypeVars nesteds

-- | returns the list of the unquantified unlocked type variables inside the given scheme
getUnlockedVars :: Data a => Scheme a -> Inferencer [TypeVar]
getUnlockedVars =
  \case
    tVars :. facts :=> nesteds -> do
      filterM isUnlocked $ Set.toList freeOuter
      where freeOuter = freeInner Set.\\ tVars
            freeInner = freeTypeVars facts <> freeTypeVars nesteds

-- | checks whether the given scheme has no unlocked free variables, otherwise adds them to the quantifier and locks them
sanitizeClosed ::
     (HasCallStack, Data a, ToType b) => Scheme a -> b -> Inferencer (Scheme a)
sanitizeClosed scheme@(tVars :. q) b = do
  freeVars <- getUnlockedVars scheme
  lockVars b freeVars
  return $ (tVars <> Set.fromList freeVars) :. q

-- | adds a class fact to the monadic inferencer state
addClassFact :: HasCallStack => Text -> Scheme Type -> Inferencer ()
addClassFact name scheme =
  case scheme of
    tVars :. _ :=> t -> do
      lockVars t tVars
      State.classFacts %= Map.insertWith mappend name [scheme]

-- | adds a function scheme to the monadic inferencer state after sanitizing it
addScheme :: HasCallStack => TypeVar -> Scheme Type -> Inferencer ()
addScheme tVar scheme =
  case scheme of
    tVars :. _ :=> _ -> do
      lockVars tVar tVars
      scheme' <- sanitizeClosed scheme tVar
      State.schemes %= Map.insert tVar scheme'

-- | adds a class scheme to the monadic inferencer state after sanitizing it
addClassScheme :: HasCallStack => Text -> Scheme Type -> Inferencer ()
addClassScheme name scheme =
  case scheme of
    tVars :. _ :=> t -> do
      lockVars t tVars
      scheme' <- sanitizeClosed scheme t
      State.classSchemes %= Map.insertWith msg name scheme' -- TODO msg
  where
    msg new old =
      error $
      "class " <>
      backQuoteShow name <>
      " already registered with scheme " <>
      backQuoteShow old <> " (attempted adding " <> backQuoteShow new <> ")"

-- | translates a type variable to its alive representant if if has been forgotten
fromOldName :: TypeVar -> Inferencer TypeVar
fromOldName tVar = uses State.renaming (`apply` tVar)

-- | reconstructs a type from the given type variable
reconstruct :: TypeVar -> Inferencer Type
reconstruct tVar =
  uses State.primPatterns (Bimap.lookup tVar) >>= \case
    Just primType -> ComplType <$> traverse reconstruct primType
    Nothing -> return $ VarType tVar

-- | reconstructs a type from the given type variable after applying `fromOldName`
reconstructOld :: TypeVar -> Inferencer Type
reconstructOld tVar = fromOldName tVar >>= reconstruct

-- | adds the unification errors from the given list to the error state of the monadic inferencer state
addUnificationErrors :: [UnificationError] -> Inferencer ()
addUnificationErrors errs =
  errorState <>= ErrorState (Error ErrorLevel <$> errs)

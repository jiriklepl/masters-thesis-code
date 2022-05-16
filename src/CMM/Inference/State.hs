{-# LANGUAGE Safe #-}

module CMM.Inference.State
  ( module CMM.Inference.State.Impl
  , module CMM.Inference.State
  ) where

import safe Control.Applicative (Applicative((<*>)))
import safe Control.Lens.Getter ((^.), uses, view)
import safe Control.Lens.Setter ((%=), (<>=))
import safe Control.Monad (Functor(fmap), Monad((>>=), return))
import safe Control.Monad.State (State)
import safe Data.Foldable (fold)
import safe Data.Function (($), (.), flip)
import safe Data.Functor ((<$>))
import safe Data.List (head, tail)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import safe Data.Monoid ((<>))
import safe Data.Ord (Ord)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Traversable (Traversable(traverse))

import safe qualified CMM.Data.Bimap as Bimap
import safe CMM.Data.Bounded (Bounded(maxBound, minBound))
import safe CMM.Data.Bounds (Bounds(Bounds), lowerBound, upperBound)
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Subst (apply)
import safe CMM.Inference.Type (Type(ComplType, VarType))
import safe CMM.Inference.TypeAnnot (TypeAnnot(NoTypeAnnot))
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , consting
  , initTypeHandle
  , kinding
  , typing, handleId
  )
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Inference.TypeVar (TypeVar(TypeVar), typeVarIdLast)

import safe CMM.Err.Error (Error(Error))
import safe CMM.Err.Severity (Severity(ErrorLevel))
import safe CMM.Err.State (ErrorState(ErrorState), HasErrorState(errorState))
import safe CMM.Inference.HandleCounter (nextHandleCounter, freshAnnotatedTypeHelperWithParent)
import safe CMM.Inference.State.Impl
  ( InferencerState(InferencerState)
  , classFacts
  , classSchemes
  , constingBounds
  , currentParent
  , handlize
  , initInferencer
  , kindingBounds
  , schemes
  , subConsting
  , subKinding
  , typize
  , unifs, Inferencer
  )
import safe CMM.Inference.Unify.Error (UnificationError)
import safe CMM.Inference.GetParent ( GetParent(getParent) )

pushParent :: TypeVar -> Inferencer ()
pushParent parent = currentParent %= (parent :)

popParent :: Inferencer ()
popParent = currentParent %= tail

freshTypeHelper :: TypeKind -> Inferencer TypeVar
freshTypeHelper = freshAnnotatedTypeHelper NoTypeAnnot

freshAnnotatedTypeHelper :: TypeAnnot -> TypeKind -> Inferencer TypeVar
freshAnnotatedTypeHelper annot tKind = do
  handle <- getParent >>= freshAnnotatedTypeHelperWithParent annot tKind
  let tVar = handleId handle
  handlize %= Bimap.insert tVar handle
  return tVar

getHandle :: TypeVar -> Inferencer TypeHandle
getHandle = fmap fromJust . tryGetHandle

tryGetHandle :: TypeVar -> Inferencer (Maybe TypeHandle)
tryGetHandle tVar =
  uses handlize (flip Bimap.lookup) <*> uses unifs (`apply` tVar)

readBoundsFrom :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> Bounds a
readBoundsFrom = (fromMaybe (minBound `Bounds` maxBound) .) . Map.lookup

readLowerBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readLowerBound = (view lowerBound .) . readBoundsFrom

readUpperBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readUpperBound = (view upperBound .) . readBoundsFrom

getConsting :: TypeVar -> Inferencer TypeVar
getConsting tVar = view consting <$> getHandle tVar

readConstingBounds :: TypeVar -> Inferencer (Bounds Constness)
readConstingBounds tVar = uses constingBounds (readBoundsFrom tVar)

readKindingBounds :: TypeVar -> Inferencer (Bounds DataKind)
readKindingBounds tVar = uses kindingBounds (readBoundsFrom tVar)

getKinding :: TypeVar -> Inferencer TypeVar
getKinding tVar = view kinding <$> getHandle tVar

getTyping :: TypeVar -> Inferencer Type
getTyping tVar = view typing <$> getHandle tVar

collectPrimeTVars :: TypeVar -> Inferencer (Set TypeVar)
collectPrimeTVars tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> fold <$> traverse collectPrimeTVars primType
    Nothing -> return $ Set.singleton tVar

handlizeTVar :: TypeVar -> Inferencer TypeVar
handlizeTVar tVar = do
  handlize %= Bimap.insert tVar (initTypeHandle NoTypeAnnot tVar)
  return tVar

infix 6 `insertEdge`

insertEdge :: Ord a => a -> a -> Map a (Set a) -> Map a (Set a)
insertEdge a b = Map.insertWith Set.union a (Set.singleton b)

pushSubKind :: TypeHandle -> TypeHandle -> Inferencer ()
pushSubKind handle handle' =
  subKinding %= view kinding handle `insertEdge` view kinding handle'

pushSubConst :: TypeHandle -> TypeHandle -> Inferencer ()
pushSubConst handle handle' =
  subConsting %= view consting handle `insertEdge` view consting handle'

pushKindBounds :: TypeHandle -> Bounds DataKind -> Inferencer ()
pushKindBounds handle bounds =
  kindingBounds %= Map.insertWith (<>) (handle ^. kinding) bounds

pushConstBounds :: TypeHandle -> Bounds Constness -> Inferencer ()
pushConstBounds handle bounds =
  constingBounds %= Map.insertWith (<>) (handle ^. consting) bounds

registerScheme :: TypeVar -> Scheme Type -> Inferencer ()
registerScheme tVar scheme = schemes %= Map.insert tVar scheme

freshTypeHelperWithHandle :: TypeKind -> Inferencer TypeVar
freshTypeHelperWithHandle kind = freshTypeHelper kind >>= handlizeTVar

fromOldName :: TypeVar -> Inferencer TypeVar
fromOldName tVar = uses unifs (`apply` tVar)

reconstruct :: TypeVar -> Inferencer Type
reconstruct tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> ComplType <$> traverse reconstruct primType
    Nothing -> return $ VarType tVar

reconstructOld :: TypeVar -> Inferencer Type
reconstructOld tVar = fromOldName tVar >>= reconstruct

addUnificationErrors :: [UnificationError] -> Inferencer ()
addUnificationErrors errs =
  errorState <>= ErrorState (Error ErrorLevel <$> errs)

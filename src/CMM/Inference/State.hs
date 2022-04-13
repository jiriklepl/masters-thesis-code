{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module CMM.Inference.State where

import safe Control.Lens.Getter ((^.), use, uses, view)
import safe Control.Lens.Setter ((%=), (+=))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadIO, MonadState)
import safe Data.Bimap (Bimap)
import safe qualified Data.Bimap as Bimap
import safe Data.Foldable (Foldable(fold))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromJust, fromMaybe)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe CMM.Data.Bounds (Bounds(Bounds), lowerBound, upperBound)
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Subst (apply)
import safe CMM.Inference.Type
  ( PrimType
  , Scheme
  , Type(ComplType, VarType)
  , TypeVar(NoType, TypeVar)
  )
import safe CMM.Inference.TypeAnnot (TypeAnnot(NoTypeAnnot))
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , consting
  , initTypeHandle
  , kinding
  , typing
  )
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Inference.Unify (UnificationError)

data Inferencer =
  Inferencer
    -- | Maps variables to their respective superKinding variables (forms a graph)
    { _subKinding :: Map TypeVar (Set TypeVar)
    -- | Maps variables to their respective superKinding variables (forms a graph)
    , _kindingBounds :: Map TypeVar (Bounds DataKind)
    -- | Maps variables to their respective subConsting variables (forms a graph - should overlap with transposed `_kinding` graph)
    , _subConsting :: Map TypeVar (Set TypeVar)
    -- | Maps variables to their respective subConsting variables (forms a graph - should overlap with transposed `_kinding` graph)
    , _constingBounds :: Map TypeVar (Bounds Constness)
    -- | TODO: Unifs
    , _unifs :: Map TypeVar TypeVar
    -- | TODO: Typize
    , _typize :: Bimap TypeVar PrimType
    -- | TODO: Handlize
    , _handlize :: Bimap TypeVar TypeHandle
    -- | TODO
    , _handleCounter :: Int
    -- | TODO
    , _errors :: [UnificationError]
    -- | TODO
    , _classSchemes :: Map Text (Scheme Type, [Scheme Type])
    -- | TODO
    , _classFacts :: Map Text (Set TypeVar)
    -- | TODO
    , _schemes :: Map TypeVar (Scheme Type)
    -- | TODO
    , _currentParent :: [TypeVar]
    }
  deriving (Show)

initInferencer :: Int -> Inferencer
initInferencer handleCounter =
  Inferencer
    { _subKinding = mempty
    , _kindingBounds = mempty
    , _subConsting = mempty
    , _constingBounds = mempty
    , _unifs = mempty
    , _typize = Bimap.empty
    , _handlize = Bimap.empty
    , _handleCounter = handleCounter
    , _classSchemes = mempty
    , _classFacts = mempty
    , _errors = mempty
    , _schemes = mempty
    , _currentParent = [globalTVar]
    }

type MonadInferencer m = (MonadState Inferencer m, MonadIO m)

globalTVar :: TypeVar
globalTVar = NoType

makeLenses ''Inferencer

getHandleCounter :: MonadInferencer m => m Int
getHandleCounter = use handleCounter

nextHandleCounter :: MonadInferencer m => m Int
nextHandleCounter = handleCounter += 1 >> use handleCounter

pushParent :: MonadInferencer m => TypeVar -> m ()
pushParent parent = currentParent %= (parent :)

getParent :: MonadInferencer m => m TypeVar
getParent = uses currentParent head

popParent :: MonadInferencer m => m ()
popParent = currentParent %= tail

freshTypeHelper :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHelper = freshAnnotatedTypeHelper NoTypeAnnot

freshAnnotatedTypeHelper ::
     MonadInferencer m => TypeAnnot -> TypeKind -> m TypeVar
freshAnnotatedTypeHelper annot tKind =
  getParent >>= freshAnnotatedTypeHelperWithParent annot tKind

freshAnnotatedTypeHelperWithParent ::
     MonadInferencer m => TypeAnnot -> TypeKind -> TypeVar -> m TypeVar
freshAnnotatedTypeHelperWithParent annot tKind parent = do
  tVar <- (\counter -> TypeVar counter tKind parent) <$> nextHandleCounter
  handlize %= Bimap.insert tVar (initTypeHandle annot tVar)
  return tVar

getHandle :: MonadInferencer m => TypeVar -> m TypeHandle
getHandle = fmap fromJust . tryGetHandle

tryGetHandle :: MonadInferencer m => TypeVar -> m (Maybe TypeHandle)
tryGetHandle tVar =
  uses handlize (flip Bimap.lookup) <*> uses unifs (`apply` tVar)

readBoundsFrom :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> Bounds a
readBoundsFrom = (fromMaybe (minBound `Bounds` maxBound) .) . Map.lookup

readLowerBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readLowerBound = (view lowerBound .) . readBoundsFrom

readUpperBound :: Bounded a => TypeVar -> Map TypeVar (Bounds a) -> a
readUpperBound = (view upperBound .) . readBoundsFrom

getConsting :: MonadInferencer m => TypeVar -> m TypeVar
getConsting tVar = view consting <$> getHandle tVar

readConstingBounds :: MonadInferencer m => TypeVar -> m (Bounds Constness)
readConstingBounds tVar = uses constingBounds (readBoundsFrom tVar)

readKindingBounds :: MonadInferencer m => TypeVar -> m (Bounds DataKind)
readKindingBounds tVar = uses kindingBounds (readBoundsFrom tVar)

getKinding :: MonadInferencer m => TypeVar -> m TypeVar
getKinding tVar = view kinding <$> getHandle tVar

getTyping :: MonadInferencer m => TypeVar -> m Type
getTyping tVar = view typing <$> getHandle tVar

collectPrimeTVars :: MonadState Inferencer m => TypeVar -> m (Set TypeVar)
collectPrimeTVars tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> fold <$> traverse collectPrimeTVars primType
    Nothing -> return $ Set.singleton tVar

handlizeTVar :: MonadInferencer m => TypeVar -> m TypeVar
handlizeTVar tVar = do
  handlize %= Bimap.insert tVar (initTypeHandle NoTypeAnnot tVar)
  return tVar

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

registerScheme :: MonadInferencer m => TypeVar -> Scheme Type -> m ()
registerScheme tVar scheme = schemes %= Map.insert tVar scheme

freshTypeHelperWithHandle :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHelperWithHandle kind = freshTypeHelper kind >>= handlizeTVar

fromOldName :: MonadState Inferencer m => TypeVar -> m TypeVar
fromOldName tVar = uses unifs (`apply` tVar)

reconstruct :: MonadState Inferencer m => TypeVar -> m Type
reconstruct tVar =
  uses typize (Bimap.lookup tVar) >>= \case
    Just primType -> ComplType <$> traverse reconstruct primType
    Nothing -> return $ VarType tVar

reconstructOld :: MonadState Inferencer m => TypeVar -> m Type
reconstructOld tVar = fromOldName tVar >>= reconstruct

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.State.Impl where

import safe Control.Lens ( makeFieldsNoPrefix, uses, (%=) )

import safe qualified CMM.Data.Bimap as Bimap
import safe Control.Monad.State (State)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Data.Bimap (Bimap)
import safe CMM.Data.Bounds (Bounds)
import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Data.Trilean (Trilean)
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter
  ( HasHandleCounter(handleCounter)
  , freshTypeHelperWithParent
  )
import safe CMM.Inference.Refresh (Refresh(refresh))
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeCompl (PrimType)
import safe CMM.Inference.Properties (Properties, propsId, initProperties)
import safe CMM.Inference.TypeKind (HasTypeKind(getTypeKind), TypeKind)
import safe CMM.Inference.TypeVar (TypeVar(NoType))
import safe CMM.Options (Options(Options))
import safe qualified CMM.Options as Options

data InferencerState =
  InferencerState
    { _subKinding :: Map TypeVar (Set TypeVar)
    -- ^ Maps variables to their respective superKinding variables (forms a graph)
    , _kindingBounds :: Map TypeVar (Bounds DataKind)
    -- ^ Maps variables to their respective subConsting variables (forms a graph - should overlap with transposed `_kinding` graph)
    , _subConsting :: Map TypeVar (Set TypeVar)
    -- ^ Maps variables to their respective subConsting variables (forms a graph - should overlap with transposed `_kinding` graph)
    , _constingBounds :: Map TypeVar (Bounds Constness)
    -- ^ TODO: Result renaming
    , _renaming :: Map TypeVar TypeVar
    -- ^ TODO: Typize
    , _typings :: Bimap TypeVar PrimType
    -- ^ TODO: Handlize
    , _typeProps :: Bimap TypeVar Properties
    -- ^ Maps variables to their respective superKinding variables (forms a graph)
    , _handleCounter :: Int
    -- ^ TODO
    , _errorState :: ErrorState
    -- ^ TODO
    , _classSchemes :: Map Text (Scheme Type)
    -- ^ TODO
    , _classFacts :: Map Text [Scheme Type]
    -- ^ TODO
    , _funDeps :: Map Text [[Trilean]]
    -- ^ TODO
    , _schemes :: Map TypeVar (Scheme Type)
    -- ^ TODO
    , _lockedVars :: Map TypeVar Type
    , _maxFunDeps :: Int
    , _currentFunDeps :: Int
    , _currentParent :: [TypeVar]
    -- ^ TODO
    }
  deriving (Show)

initInferencer :: Options -> InferencerState
initInferencer Options {Options.maxFunDeps = maxFunDeps, Options.handleStart = handleCounter'} =
  InferencerState
    { _subKinding = nullVal
    , _kindingBounds = nullVal
    , _subConsting = nullVal
    , _constingBounds = nullVal
    , _renaming = nullVal
    , _typings = Bimap.empty
    , _typeProps = nullVal
    , _handleCounter = handleCounter'
    , _classSchemes = nullVal
    , _classFacts = nullVal
    , _funDeps = nullVal
    , _errorState = nullVal
    , _schemes = nullVal
    , _maxFunDeps = maxFunDeps
    , _currentFunDeps = 0
    , _lockedVars = nullVal
    , _currentParent = [globalTVar]
    }

globalTVar :: TypeVar
globalTVar = NoType

makeFieldsNoPrefix ''InferencerState

type Inferencer = State InferencerState

instance GetParent Inferencer TypeVar where
  getParent = uses currentParent head

instance Refresh Inferencer where
  refresh tVars =
    sequence $
    Map.fromSet
      (freshTypeHelper . getTypeKind)
      tVars

freshTypeHelper :: TypeKind -> Inferencer TypeVar
freshTypeHelper tKind = do
  props <- getParent >>= freshTypeHelperWithParent tKind
  let tVar = propsId props
  typeProps %= Bimap.insert tVar props
  return tVar

freshTypeHelperWithHandle :: TypeKind -> Inferencer TypeVar
freshTypeHelperWithHandle kind = freshTypeHelper kind >>= typePropsTVar

typePropsTVar :: TypeVar -> Inferencer TypeVar
typePropsTVar tVar = do
  typeProps %= Bimap.insert tVar (initProperties tVar)
  return tVar

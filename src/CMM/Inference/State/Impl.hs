{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.State.Impl where

import safe Prelude

import safe Control.Lens.TH (makeLenses)

import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Data.Bimap (Bimap)
import safe CMM.Data.Bounds (Bounds)
import safe CMM.Inference.Constness (Constness)
import safe CMM.Inference.DataKind (DataKind)
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeCompl (PrimType)
import safe CMM.Inference.TypeHandle (TypeHandle)
import safe CMM.Inference.TypeVar (TypeVar)
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

makeLenses ''Inferencer

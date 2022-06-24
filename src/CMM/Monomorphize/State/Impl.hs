{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Monomorphize.State.Impl where

import safe Control.Lens.TH (makeLenses)

import safe Control.Monad.State (State)
import safe Data.Eq (Eq)
import safe Data.Function (($))
import safe Data.Int (Int)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Monoid (Monoid(mappend, mempty))
import safe Data.Ord (Ord)
import safe Data.Semigroup (Semigroup((<>)))
import safe Data.Set (Set)
import safe Text.Show (Show)

import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Monomorphize.Schematized (Schematized)

newtype PolyGenerate =
  PolyGenerate
    { getPolyGenerate :: Map TypeVar (Set TypeVar)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyGenerate where
  PolyGenerate a <> PolyGenerate b = PolyGenerate $ Map.unionWith mappend a b

instance Monoid PolyGenerate where
  mempty = PolyGenerate mempty

newtype PolyMethods =
  PolyMethods
    { getPolyMethods :: Map TypeVar (Set TypeVar)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyMethods where
  PolyMethods a <> PolyMethods b = PolyMethods $ Map.unionWith mappend a b

instance Monoid PolyMethods where
  mempty = PolyMethods mempty

newtype PolyData =
  PolyData
    { getPolyData :: Map TypeVar (Map TypeVar TypeVar)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyData where
  PolyData a <> PolyData b = PolyData $ Map.unionWith mappend a b

instance Monoid PolyData where
  mempty = PolyData mempty

type PolySchemes a = Map TypeVar (Scheme Type, Schematized a)

data MonomorphizeState a =
  MonoMorphizeState
    { _polyMethods :: PolyMethods
    , _polyData :: PolyData
    , _polyGenerate :: PolyGenerate
    , _polyMemory :: PolyGenerate
    , _polyWaves :: Int
    , _polySchemes :: PolySchemes a
    }

makeLenses ''MonomorphizeState

initMonomorphizeState :: MonomorphizeState a
initMonomorphizeState =
  MonoMorphizeState
    { _polyGenerate = mempty
    , _polyData = mempty
    , _polyMethods = mempty
    , _polySchemes = mempty
    , _polyMemory = mempty
    , _polyWaves = 0
    }

type Monomorphizer a = State (MonomorphizeState a)

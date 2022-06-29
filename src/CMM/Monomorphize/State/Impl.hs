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
import CMM.Monomorphize.Settings ( MonomorphizerSettings(MonomorphizerSettings) )
import CMM.Parser.HasPos ( SourcePos )
import CMM.AST.Wrap ( ASTWrapper )
import CMM.AST.Annot ( Annot )

newtype PolyMemory =
  PolyMemory
    { getPolyMemory :: Map TypeVar (Set Type)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyMemory where
  PolyMemory a <> PolyMemory b = PolyMemory $ Map.unionWith mappend a b

instance Monoid PolyMemory where
  mempty = PolyMemory mempty

newtype PolyGenerate =
  PolyGenerate
    { getPolyGenerate :: Map TypeVar [(TypeVar, Annot ASTWrapper SourcePos)]
    }
  deriving (Show)

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
  MonomorphizeState
    { _polyMethods :: PolyMethods
    , _polyData :: PolyData
    , _polyGenerate :: PolyGenerate
    , _polyMemory :: PolyMemory
    , _polyStorage :: PolyMemory
    , _polyWaves :: Int
    , _maxPolyWaves :: Int
    , _polySchemes :: PolySchemes a
    }

makeLenses ''MonomorphizeState

initMonomorphizeState :: MonomorphizerSettings -> MonomorphizeState a
initMonomorphizeState MonomorphizerSettings {} =
  MonomorphizeState
    { _polyGenerate = mempty
    , _polyData = mempty
    , _polyMethods = mempty
    , _polySchemes = mempty
    , _polyMemory = mempty
    , _polyStorage = mempty
    , _polyWaves = 0
    , _maxPolyWaves = 50
    }

type Monomorphizer a = State (MonomorphizeState a)

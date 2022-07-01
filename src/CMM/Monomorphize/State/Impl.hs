{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Monomorphize.State.Impl where

import safe Control.Lens.TH (makeLenses)

import safe Control.Monad.State (State)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)

import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Monomorphize.Schematized (Schematized)
import CMM.Parser.HasPos ( SourcePos )
import CMM.AST.Wrap ( ASTWrapper )
import CMM.AST.Annot ( Annot )
import CMM.Options (Options (Options))
import qualified CMM.Options as Options
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

initMonomorphizeState :: Options -> MonomorphizeState a
initMonomorphizeState Options {Options.maxCycles = maxPolyWaves'} =
  MonomorphizeState
    { _polyGenerate = mempty
    , _polyData = mempty
    , _polyMethods = mempty
    , _polySchemes = mempty
    , _polyMemory = mempty
    , _polyStorage = mempty
    , _polyWaves = 0
    , _maxPolyWaves = maxPolyWaves'
    }

type Monomorphizer a = State (MonomorphizeState a)

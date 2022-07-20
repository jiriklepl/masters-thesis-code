{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Monomorphize.State.Impl where

import safe Control.Lens (makeLenses)

import safe Control.Monad.State (State)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)

import safe CMM.AST.Annot (Annot)
import safe CMM.AST.Wrap (ASTWrapper)
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeVar (TypeVar)
import safe CMM.Monomorphize.Schematized (Schematized)
import safe CMM.Options (Options(Options))
import safe qualified CMM.Options as Options
import safe CMM.Parser.GetPos (SourcePos)

-- | maps each scheme to already memorized instances
newtype PolyMemory =
  PolyMemory
    { getPolyMemory :: Map TypeVar (Set Type)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyMemory where
  PolyMemory a <> PolyMemory b = PolyMemory $ Map.unionWith mappend a b

instance Monoid PolyMemory where
  mempty = PolyMemory mempty

-- | maps each scheme to the list of pairs of monotype to be instantiated to and wrapped references
newtype PolyGenerate =
  PolyGenerate
    { getPolyGenerate :: Map TypeVar [(TypeVar, Annot ASTWrapper SourcePos)]
    }
  deriving (Show)

instance Semigroup PolyGenerate where
  PolyGenerate a <> PolyGenerate b = PolyGenerate $ Map.unionWith mappend a b

instance Monoid PolyGenerate where
  mempty = PolyGenerate mempty

-- | maps each method to the list of its instances
newtype PolyMethods =
  PolyMethods
    { getPolyMethods :: Map TypeVar (Set TypeVar)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyMethods where
  PolyMethods a <> PolyMethods b = PolyMethods $ Map.unionWith mappend a b

instance Monoid PolyMethods where
  mempty = PolyMethods mempty

-- | maps each field accessor to the list of its instances
newtype PolyData =
  PolyData
    { getPolyData :: Map TypeVar (Map TypeVar TypeVar)
    }
  deriving (Eq, Ord, Show)

instance Semigroup PolyData where
  PolyData a <> PolyData b = PolyData $ Map.unionWith mappend a b

instance Monoid PolyData where
  mempty = PolyData mempty

-- | the database of all polytype schemes
type PolySchemes a = Map TypeVar (Scheme Type, Schematized a)

-- | the monomorphizer state
data MonomorphizeState a =
  MonomorphizeState
    { _polyMethods :: PolyMethods -- ^ maps each method to the list of its instances
    , _polyData :: PolyData -- ^ maps each field accessor to the list of its instances
    , _polyGenerate :: PolyGenerate -- ^ maps each scheme to the list of pairs of monotype to be instantiated to and wrapped references
    , _polyMemory :: PolyMemory -- ^ maps each scheme to already asked to instance monotypes
    , _polyStorage :: PolyMemory -- ^ maps each scheme to already instanced monotypes
    , _polyWaves :: Int -- ^ the number of currently done monomorphization waves
    , _maxPolyWaves :: Int -- ^ the maximum allowed number of monomorphization waves
    , _polySchemes :: PolySchemes a -- ^ the database of all polytype schemes
    }

makeLenses ''MonomorphizeState

-- | initializes an empty monomorphizer state according to the given `Options` object
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

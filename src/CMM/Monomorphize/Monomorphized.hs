{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Monomorphize.Monomorphized where

import safe Control.Lens.TH (makeLenses)

import safe Control.Lens.Getter (view)
import safe Control.Lens.Setter ((.~), (%~))
import safe Control.Lens.Tuple
import Data.Eq (Eq)
import Data.Foldable (Foldable(foldMap))
import Data.Function (($), (.))
import Data.Functor (Functor (fmap), (<$>))
import safe Data.Functor.Const (Const)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Monoid(mappend, mempty))
import Data.Ord (Ord)
import Data.Semigroup (Semigroup((<>)))
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import Data.Traversable (Traversable)
import safe Data.Void (Void)
import Text.Show (Show)

import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Monomorphize.Schematized (Schematized)
import CMM.Inference.Subst
import CMM.Inference.TypeVar

newtype PolyGenerate =
  PolyGenerate
    { getPolyGenerate :: Map TypeHandle (Set TypeHandle)
    }
  deriving (Eq, Ord, Show)

newtype PolyMethods =
  PolyMethods
    { getPolyMethods :: Map TypeHandle (Set TypeHandle)
    }
  deriving (Eq, Ord, Show)

newtype PolyData =
  PolyData
    { getPolyData :: Map TypeVar (Map TypeVar TypeVar)
    }
  deriving (Eq, Ord, Show)

type PolySchemes a = Map TypeHandle (Scheme Type, Schematized a)

instance Semigroup PolyGenerate where
  PolyGenerate a <> PolyGenerate b = PolyGenerate $ Map.unionWith mappend a b

instance Monoid PolyGenerate where
  mempty = PolyGenerate mempty

instance Semigroup PolyMethods where
  PolyMethods a <> PolyMethods b = PolyMethods $ Map.unionWith mappend a b

instance Monoid PolyMethods where
  mempty = PolyMethods mempty

instance Semigroup PolyData where
  PolyData a <> PolyData b = PolyData $ Map.unionWith mappend a b

instance Monoid PolyData where
  mempty = PolyData mempty

addImpl :: Monoid a1 => (Map k a2 -> a1) -> k -> a2 -> a1 -> a1
addImpl what key value = mappend . what $ Map.singleton key value

addSetImpl :: Monoid a1 => (Map k (Set a2) -> a1) -> k -> a2 -> a1 -> a1
addSetImpl what key value = addImpl what key $ Set.singleton value

data Monomorphized n a =
  Monomorphized
    { _node :: Maybe (n a)
    , _polyMethods :: PolyMethods
    , _polyData :: PolyData
    , _polyGenerate :: PolyGenerate
    , _polySchemes :: PolySchemes a
    }
  deriving (Show)

monomorphized ::
     Maybe (n a)
  -> PolyMethods
  -> PolyData
  -> PolyGenerate
  -> PolySchemes a
  -> Monomorphized n a
monomorphized n m d gen s =
  Monomorphized
    {_node = n, _polyGenerate = gen, _polyData = d, _polyMethods = m, _polySchemes = s}

monomorphizedTrivial :: Maybe (n a) -> Monomorphized n a
monomorphizedTrivial n = monomorphized n mempty mempty mempty mempty

makeLenses ''Monomorphized

renewPolyData :: Map TypeVar TypeVar -> Monomorphized n a -> Monomorphized n a
renewPolyData (unifs :: Map TypeVar TypeVar) mono =
  polyData .~ PolyData toData $ mono
  where
    toData = Map.fromList $ (_2 %~ Map.fromList . fmap (apply unifs)) . (_1 %~ apply unifs) <$> fromData
    fromData = fmap (_2 %~ Map.toList) . Map.toList . getPolyData $ view polyData mono

addGenerate :: TypeHandle -> TypeHandle -> Monomorphized n a -> Monomorphized n a
addGenerate scheme inst = polyGenerate %~ addSetImpl PolyGenerate scheme inst

addMethod :: TypeHandle -> TypeHandle -> Monomorphized n a -> Monomorphized n a
addMethod scheme inst = polyMethods %~ addSetImpl PolyMethods scheme inst

addData :: TypeVar
  -> TypeVar
  -> TypeVar
  -> Monomorphized n a
  -> Monomorphized n a
addData struct field inst = polyData %~ addImpl PolyData field (Map.singleton inst struct)

instance Nullable (Monomorphized n a) where
  nullVal = monomorphizedTrivial Nothing

instance Semigroup (n a) => Semigroup (Monomorphized n a) where
  Monomorphized n g m d s <> Monomorphized n' g' m' d' s' =
    Monomorphized (n <> n') (g <> g') (m <> m') (d <> d') (s <> s')

instance Semigroup (n a) => Monoid (Monomorphized n a) where
  mempty = Monomorphized Nothing mempty mempty mempty mempty

deriving instance Functor n => Functor (Monomorphized n)

deriving instance Foldable n => Foldable (Monomorphized n)

deriving instance Traversable n => Traversable (Monomorphized n)

withMaybeNode :: Maybe (n' a) -> Monomorphized n a -> Monomorphized n' a
withMaybeNode n mono = mono {_node = n}

withNode :: n' a -> Monomorphized n a -> Monomorphized n' a
withNode n mono = Just n `withMaybeNode` mono

getNode :: Monomorphized n a -> Maybe (n a)
getNode = view node

unNode :: Monomorphized n a -> Monomorphized (Const Void) a
unNode = withMaybeNode Nothing

unPolyGenerate :: Monomorphized n a -> Monomorphized n a
unPolyGenerate = polyGenerate .~ mempty

foldGetSchemes :: Foldable f => f (Monomorphized n a) -> PolySchemes a
foldGetSchemes = foldMap $ view polySchemes

foldGetGenerate :: Foldable f => f (Monomorphized n a) -> PolyGenerate
foldGetGenerate = foldMap $ view polyGenerate

foldGetMethods :: Foldable f => f (Monomorphized n a) -> PolyMethods
foldGetMethods = foldMap $ view polyMethods

foldGetData :: Foldable f => f (Monomorphized n a) -> PolyData
foldGetData = foldMap $ view polyData

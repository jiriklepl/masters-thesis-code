{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Monomorphize.Monomorphized where

import safe Control.Lens.Getter (view)
import safe Control.Lens.Setter ((.~))
import safe Control.Lens.TH (makeLenses)
import safe Data.Functor.Const (Const)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Void (Void)

import safe CMM.Data.Nullable (Nullable(..))
import safe CMM.Inference.Fact (Scheme)
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)
import safe CMM.Monomorphize.Schematized (Schematized)

newtype PolyGenerate =
  PolyGenerate
    { getPolyGenerate :: Map TypeHandle (Set TypeHandle)
    }
  deriving (Eq, Ord, Show)

type PolySchemes a = Map TypeHandle (Scheme Type, Schematized a)

instance Semigroup PolyGenerate where
  PolyGenerate a <> PolyGenerate b = PolyGenerate $ Map.unionWith mappend a b

instance Monoid PolyGenerate where
  mempty = PolyGenerate mempty

addGenerate :: TypeHandle -> Set TypeHandle -> PolyGenerate -> PolyGenerate
addGenerate key value = mappend . PolyGenerate $ Map.singleton key value

data Monomorphized n a =
  Monomorphized
    { _node :: Maybe (n a)
    , _polyGenerate :: PolyGenerate
    , _polySchemes :: PolySchemes a
    }
  deriving (Show)

monomorphized ::
     Maybe (n a) -> PolyGenerate -> PolySchemes a -> Monomorphized n a
monomorphized n gen s =
  Monomorphized {_node = n, _polyGenerate = gen, _polySchemes = s}

monomorphizedTrivial :: Maybe (n a) -> Monomorphized n a
monomorphizedTrivial n = monomorphized n mempty mempty

makeLenses ''Monomorphized

instance Nullable (Monomorphized n a) where
  nullVal = monomorphizedTrivial Nothing

instance Semigroup (n a) => Semigroup (Monomorphized n a) where
  Monomorphized n g s <> Monomorphized n' g' s' =
    Monomorphized (n <> n') (g <> g') (s <> s')

instance Semigroup (n a) => Monoid (Monomorphized n a) where
  mempty = Monomorphized Nothing mempty mempty

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

foldGetSchemes ::
     (Foldable f, Functor f) => f (Monomorphized n a) -> PolySchemes a
foldGetSchemes = foldMap $ view polySchemes

foldGetGenerate ::
     (Foldable f, Functor f) => f (Monomorphized n a) -> PolyGenerate
foldGetGenerate = foldMap $ view polyGenerate

{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

module CMM.Monomorphize.Monomorphized where

import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Control.Lens.TH (makeLenses)
import safe Control.Lens.Getter (view)
import safe Data.Functor.Const (Const)
import safe Data.Void (Void)

import safe CMM.Inference.Type ( Scheme, Type )
import safe CMM.Monomorphize.Schematized ( Schematized )
import safe CMM.Inference.TypeHandle ( TypeHandle )
import safe CMM.Data.Nullable ( Nullable(..) )
import Control.Lens.Setter ((.~))

data Monomorphized n a = Monomorphized
  { _node :: Maybe (n a)
  , _polyGenerate :: Map TypeHandle (Set TypeHandle)
  , _polySchemes :: Map TypeHandle (Scheme Type, Schematized a)
  }
  deriving (Show)

monomorphized ::
  Maybe (n a)
  -> Map TypeHandle (Set TypeHandle)
  -> Map TypeHandle (Scheme Type, Schematized a)
  -> Monomorphized n a
monomorphized n gen s = Monomorphized { _node = n, _polyGenerate = gen, _polySchemes = s }

monomorphizedTrivial :: Maybe (n a) -> Monomorphized n a
monomorphizedTrivial n = monomorphized n mempty mempty

makeLenses ''Monomorphized

instance Nullable (Monomorphized n a) where
  nullVal = monomorphizedTrivial Nothing

instance Semigroup (n a) => Semigroup (Monomorphized n a) where
  Monomorphized n g s <> Monomorphized n' g' s' = Monomorphized (n <> n') (g <> g') (s <> s')

instance Semigroup (n a) => Monoid (Monomorphized n a) where
  mempty = Monomorphized Nothing mempty mempty

deriving instance Functor n => Functor (Monomorphized n)
deriving instance Foldable n => Foldable (Monomorphized n)
deriving instance Traversable n => Traversable (Monomorphized n)

withMaybeNode :: Maybe (n' a) -> Monomorphized n a -> Monomorphized n' a
withMaybeNode n mono = mono {_node = n}

withNode ::
  n' a
  -> Monomorphized n a
  -> Monomorphized n' a
withNode n mono = Just n `withMaybeNode` mono

getNode :: Monomorphized n a -> Maybe (n a)
getNode = view node

unNode :: Monomorphized n a -> Monomorphized (Const Void) a
unNode = withMaybeNode Nothing

unPolyGenerate :: Monomorphized n a -> Monomorphized n a
unPolyGenerate = polyGenerate .~ mempty

foldGetSchemes ::
  (Foldable f, Functor f) => f (Monomorphized n a)
  -> Map TypeHandle (Scheme Type, Schematized a)
foldGetSchemes = foldMap $ view polySchemes

foldGetGenerate ::
  (Foldable f, Functor f) => f (Monomorphized n a)
  -> Map TypeHandle (Set TypeHandle)
foldGetGenerate = foldMap $ view polyGenerate


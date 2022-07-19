{-# LANGUAGE Safe #-}

module CMM.Inference.HandleCounter where

import safe Control.Lens ( use, (+=), Lens' )
import safe Control.Monad.State (MonadState)

import safe CMM.Inference.TypeAnnot (TypeAnnot)
import safe CMM.Inference.Properties (Properties, initProperties)
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Inference.TypeVar (TypeVar, typeVarIdLast)

-- | Lens for getting a handle counter from an object having a handle counter
class a ~ Int =>
      HasHandleCounter s a
  | s -> a
  where
  handleCounter :: Lens' s a

-- | increments the handle counter counter in the given monadic state and returns new handle identifier
nextHandleCounter :: (MonadState s m, HasHandleCounter s a) => m Int
nextHandleCounter = handleCounter += 1 >> use handleCounter

-- takes handle annotation, type kind and a parent type variable and returns a fresh type handle
freshAnnotatedTypeHelperWithParent ::
     (MonadState s m, HasHandleCounter s Int)
  => TypeAnnot
  -> TypeKind
  -> TypeVar
  -> m Properties
freshAnnotatedTypeHelperWithParent annot tKind parent =
  initProperties annot . typeVarIdLast tKind parent <$> nextHandleCounter

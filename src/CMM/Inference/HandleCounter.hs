{-# LANGUAGE Safe #-}

module CMM.Inference.HandleCounter where

import safe Control.Lens (Lens', (+=), use)
import safe Control.Monad.State (MonadState)

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

-- takes type kind and a parent type variable and returns a fresh type handle
freshTypeHelperWithParent ::
     (MonadState s m, HasHandleCounter s Int)
  => TypeKind
  -> TypeVar
  -> m Properties
freshTypeHelperWithParent tKind parent =
  initProperties . typeVarIdLast tKind parent <$> nextHandleCounter

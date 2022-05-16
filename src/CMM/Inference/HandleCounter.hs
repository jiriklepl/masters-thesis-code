{-# LANGUAGE Safe #-}

module CMM.Inference.HandleCounter where

import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((+=), (.=))
import safe Control.Lens.Type (Lens')
import safe Control.Monad (Monad((>>)))
import safe Control.Monad.State (MonadState)
import safe Data.Int (Int)
import safe Data.Monoid (Sum(getSum))
import safe Data.Function ( (.) )
import safe Data.Functor ( (<$>) )

import safe CMM.Inference.TypeAnnot ( TypeAnnot )
import safe CMM.Inference.TypeKind ( TypeKind )
import safe CMM.Inference.TypeVar ( TypeVar, typeVarIdLast )
import safe CMM.Inference.TypeHandle ( initTypeHandle, TypeHandle )

type HandleCounter = Sum Int

class a ~ HandleCounter =>
      HasHandleCounter s a
  | s -> a
  where
  handleCounter :: Lens' s a

getHandleCounter :: (MonadState s m, HasHandleCounter s a) => m Int
getHandleCounter = uses handleCounter getSum

setHandleCounter ::
     (MonadState s m, HasHandleCounter s a) => HandleCounter -> m ()
setHandleCounter counter = handleCounter .= counter

nextHandleCounter :: (MonadState s m, HasHandleCounter s a) => m Int
nextHandleCounter = handleCounter += 1 >> getHandleCounter

freshAnnotatedTypeHelperWithParent :: (MonadState s m,
 HasHandleCounter s HandleCounter) =>
  TypeAnnot -> TypeKind -> TypeVar -> m TypeHandle
freshAnnotatedTypeHelperWithParent annot tKind parent =
  initTypeHandle annot . typeVarIdLast tKind parent <$> nextHandleCounter

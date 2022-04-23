{-# LANGUAGE Safe #-}

module CMM.Inference.HandleCounter where

import safe Control.Lens.Type (Lens')
import safe Control.Monad.State (MonadState)
import safe Data.Int ( Int )
import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((+=), (.=))
import safe Control.Monad ( Monad((>>)) )
import safe Data.Monoid ( Sum (getSum) )

type HandleCounter = Sum Int

class a ~ HandleCounter => HasHandleCounter s a | s -> a where
  handleCounter :: Lens' s a

getHandleCounter :: (MonadState s m, HasHandleCounter s a) => m Int
getHandleCounter = uses handleCounter getSum

setHandleCounter :: (MonadState s m, HasHandleCounter s a) => HandleCounter -> m ()
setHandleCounter counter = handleCounter .= counter

nextHandleCounter :: (MonadState s m, HasHandleCounter s a) => m Int
nextHandleCounter = handleCounter += 1 >> getHandleCounter

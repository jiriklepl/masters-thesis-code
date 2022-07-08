{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Lens where

import safe Control.Lens ( use, (.=), Lens )
import safe Control.Monad.State (MonadState)

infix 4 `exchange`

-- | Performs an exchange of the value a variable in a 'MonadState' monad accessed via a 'Lens' with a given new value.
--
-- returns the old value while storing the new value
exchange :: MonadState s m => Lens s s a a -> a -> m a
l `exchange` b = use l <* (l .= b)

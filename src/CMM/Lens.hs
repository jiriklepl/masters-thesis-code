{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Lens where

import safe Control.Lens.Getter (use)
import safe Control.Lens.Setter ((.=))
import safe Control.Lens.Type (Lens)
import safe Control.Monad.State.Lazy (MonadState)

infix 4 `exchange`

-- | Performs an exchange of the value a variable in a `MonadState` monad accessed via a `Lens` with a given new value.
-- | returns the old value while storing the new value
exchange :: MonadState s m => Lens s s a a -> a -> m a
l `exchange` b = use l <* (l .= b)

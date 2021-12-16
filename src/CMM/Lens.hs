{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Lens where

import safe Control.Lens.Getter (use)
import safe Control.Lens.Setter ((.=))
import safe Control.Lens.Type (Lens)
import safe Control.Monad.State.Lazy (MonadState)

infix 4 `exchange`

exchange :: MonadState s m => Lens s s a a -> a -> m a
l `exchange` b = use l <* (l .= b)

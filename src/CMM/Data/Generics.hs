{-# LANGUAGE Safe #-}

module CMM.Data.Generics where

import safe Data.Data (Typeable)
import safe Data.Generics.Aliases (extM, extT)

infixr 3 <*|*>

-- | An alias of flipped `extM`. Its behavior resembles that of the `<|>` method of `Alternative`, including the evaluation order (but mind the infixr fixity).
(<*|*>) ::
     (Monad m, Typeable a, Typeable b) => (b -> m b) -> (a -> m a) -> a -> m a
(<*|*>) = flip extM

infixr 3 *|*

-- | An alias of flipped `extT`. Its behavior resembles that of the `<|>` method of `Alternative`, including the evaluation order (but mind the infixr fixity).
(*|*) :: (Typeable a, Typeable b) => (b -> b) -> (a -> a) -> a -> a
(*|*) = flip extT

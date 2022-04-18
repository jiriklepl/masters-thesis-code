{-# LANGUAGE Safe #-}

module CMM.Control.Monad where

import safe Prelude

infixl 1 >>@=

(>>@=) :: (Monad m, Functor f) => f (m a) -> (a -> m b) -> f (m b)
f >>@= g = (>>= g) <$> f

{-# LANGUAGE Safe #-}

module CMM.Control.Monad where

import safe Control.Applicative ((<$>))
import safe Control.Monad (Functor, Monad((>>=)))

infixl 1 >>@=

(>>@=) :: (Monad m, Functor f) => f (m a) -> (a -> m b) -> f (m b)
f >>@= g = (>>= g) <$> f

{-# LANGUAGE Safe #-}

module CMM.Control.Applicative where

import Control.Applicative

liftA4 ::
     Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
liftA5 f a b c d e = liftA4 f a b c d <*> e

liftA6 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g -> h)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
  -> f h
liftA6 f a b c d e g = liftA5 f a b c d e <*> g

infixl 4 <*<

(<*<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) = liftA2 (.)

infixl 4 >*>

(>*>) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>*>) = liftA2 (flip (.))
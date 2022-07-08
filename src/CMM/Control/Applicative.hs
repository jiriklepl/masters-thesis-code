{-# LANGUAGE Safe #-}

module CMM.Control.Applicative where

import safe Control.Applicative (liftA2, liftA3)

-- | A case of liftA<N> the functions for 4 arguments
liftA4 ::
     Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

-- | A case of liftA<N> the functions for 5 arguments
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

infixl 4 <:>

-- | A lifted (:)
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

-- | A case of liftA<N> the functions for 6 arguments
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

-- | Applicative version of the dot operator
(<*<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) = liftA2 (.)

infixl 4 >*>

-- | Applicative version of the flipped dot operator
(>*>) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>*>) = liftA2 (flip (.))

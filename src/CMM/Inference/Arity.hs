{-# LANGUAGE Safe #-}

module CMM.Inference.Arity where

-- | A class representing an arity of the given object
class Arity a where
  arity :: a -> Int

-- | returns `True` iff the given object is nullary
nullary :: Arity a => a -> Bool
nullary = (== 0) . arity

-- | returns `True` iff the given object is unary
unary :: Arity a => a -> Bool
unary = (== 1) . arity

-- | returns `True` iff the given object is binary
binary :: Arity a => a -> Bool
binary = (== 2) . arity

-- | returns `True` iff the given object is ternary
ternary :: Arity a => a -> Bool
ternary = (== 3) . arity

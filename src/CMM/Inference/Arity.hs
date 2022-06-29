{-# LANGUAGE Safe #-}

module CMM.Inference.Arity where


class Arity a where
  arity :: a -> Int

nullary :: Arity a => a -> Bool
nullary = (== 0) . arity

unary :: Arity a => a -> Bool
unary = (== 1) . arity

binary :: Arity a => a -> Bool
binary = (== 2) . arity

ternary :: Arity a => a -> Bool
ternary = (== 3) . arity

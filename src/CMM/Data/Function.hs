{-# LANGUAGE Safe #-}

module CMM.Data.Function where

import safe Data.Foldable (Foldable(foldl'))
import safe Data.Function ((&))

infixr 1 `fOr`

-- | performs `||` on the results of two functions for the given argument
fOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f g x = f x || g x

infixr 2 `fAnd`

-- | performs `&&` on the results of two functions for the given argument
fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = f x && g x

-- | Applies a list of functions, one-by-one, to the given argument
applyAll :: [a -> a] -> a -> a
applyAll fs a = foldl' (&) a fs

-- | Pipe-like composition of functions
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

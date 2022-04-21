{-# LANGUAGE Safe #-}

module CMM.Data.Function where

import safe Data.Bool (Bool, (&&), (||))

fOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fOr f g x = f x || g x

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = f x && g x

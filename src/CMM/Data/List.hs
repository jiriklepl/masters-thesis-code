{-# LANGUAGE Safe #-}

module CMM.Data.List where

import safe Data.Bool (Bool, otherwise)
import safe Data.Foldable (foldr)

import safe CMM.Data.Num (Num((+)))
import safe Data.Eq (Eq((==)))

-- | @'count' p as@ returns the number of items in the list @as@ that satisfy the predicate @p@
count :: Num t1 => (t2 -> Bool) -> [t2] -> t1
count f = foldr go 0
  where
    go x i
      | f x = i + 1
      | otherwise = i

uniq :: Eq a => [a] -> [a]
uniq = uniqOn (==)

uniqOn :: (a -> a -> Bool) -> [a] -> [a]
uniqOn p = go
  where
    go (x:xs@(y:_))
      | p x y = go xs
      | otherwise = x : go xs
    go xs = xs

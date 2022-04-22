{-# LANGUAGE Safe #-}

module CMM.Data.List where

import safe Data.Bool (Bool, otherwise)
import safe Data.Foldable (foldr)

import safe CMM.Data.Num (Num((+)))

count :: Num t1 => (t2 -> Bool) -> [t2] -> t1
count f = foldr go 0
  where
    go x i
      | f x = i + 1
      | otherwise = i

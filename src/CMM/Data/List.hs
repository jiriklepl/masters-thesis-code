{-# LANGUAGE Safe #-}

module CMM.Data.List where

-- | @'count' p as@ returns the number of items in the list @as@ that satisfy the predicate @p@
count :: Num t1 => (t2 -> Bool) -> [t2] -> t1
count f = foldr go 0
  where
    go x i
      | f x = i + 1
      | otherwise = i

-- | Goes through an (ordered) list and removes consecutive duplicates
uniq :: Eq a => [a] -> [a]
uniq = uniqOn (==)

-- | Goes through an (ordered) list and removes consecutive duplicates as dictated by the given predicate
uniqOn :: (a -> a -> Bool) -> [a] -> [a]
uniqOn p = go
  where
    go (x:xs@(y:_))
      | p x y = go xs
      | otherwise = x : go xs
    go xs = xs

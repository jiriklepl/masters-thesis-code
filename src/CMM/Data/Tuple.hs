{-# LANGUAGE Safe #-}

module CMM.Data.Tuple where

-- | An uncurry function for 4 arguments
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- | Removes the first element from a tuple of 3
complFst3 :: (a, b, c) -> (b, c)
complFst3 (_, b, c) = (b, c)

-- | Removes the second element from a tuple of 3
complSnd3 :: (a, b, c) -> (a, c)
complSnd3 (a, _, c) = (a, c)

-- | Removes the third element from a tuple of 3
complThd3 :: (a, b, c) -> (a, b)
complThd3 (a, b, _) = (a, b)

-- | Removes the first element from a tuple of 4
complFst4 :: (a, b, c, d) -> (b, c, d)
complFst4 (_, b, c, d) = (b, c, d)

-- | Removes the second element from a tuple of 4
complSnd4 :: (a, b, c, d) -> (a, c, d)
complSnd4 (a, _, c, d) = (a, c, d)

-- | Removes the third element from a tuple of 4
complThd4 :: (a, b, c, d) -> (a, b, d)
complThd4 (a, b, _, d) = (a, b, d)

-- | Removes the fourth element from a tuple of 4
complFrth4 :: (a, b, c, d) -> (a, b, c)
complFrth4 (a, b, c, _) = (a, b, c)

-- | Takes a tuple containing a value and a list of values, transforms it to a list of tuples
submergeTuple :: (a, [b]) -> [(a, b)]
submergeTuple (a, bs) = (a, ) <$> bs

-- | Like `submergeTuple`, but with the elements swapped
submergeTuple' :: ([a], b) -> [(a, b)]
submergeTuple' (as, b) = (, b) <$> as

{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module CMM.Data.Tuple where

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

complFst3 :: (a, b, c) -> (b, c)
complFst3 (_, b, c) = (b, c)

complSnd3 :: (a, b, c) -> (a, c)
complSnd3 (a, _, c) = (a, c)

complThd3 :: (a, b, c) -> (a, b)
complThd3 (a, b, _) = (a, b)

complFst4 :: (a, b, c, d) -> (b, c, d)
complFst4 (_, b, c, d) = (b, c, d)

complSnd4 :: (a, b, c, d) -> (a, c, d)
complSnd4 (a, _, c, d) = (a, c, d)

complThd4 :: (a, b, c, d) -> (a, b, d)
complThd4 (a, b, _, d) = (a, b, d)

complFrth4 :: (a, b, c, d) -> (a, b, c)
complFrth4 (a, b, c, _) = (a, b, c)

submergeTuple :: (a, [b]) -> [(a, b)]
submergeTuple (a, bs) = (a,) <$> bs

submergeTuple' :: ([a], b) -> [(a, b)]
submergeTuple' (as, b) = (,b) <$> as

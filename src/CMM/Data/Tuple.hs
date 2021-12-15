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

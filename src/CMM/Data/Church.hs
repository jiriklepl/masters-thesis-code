{-# LANGUAGE Safe #-}

module CMM.Data.Church where

church :: (Num n, Eq n) => n -> (a -> a) -> a -> a
church 0 _ = id
church n f = f . church (n - 1) f

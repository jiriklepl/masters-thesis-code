{-# LANGUAGE Safe #-}

module CMM.Data.Church where

import safe Data.Eq (Eq)
import safe Data.Function ((.), id)

import safe CMM.Data.Num (Num((-)))

church :: (Num n, Eq n) => n -> (a -> a) -> a -> a
church 0 _ = id
church n f = f . church (n - 1) f

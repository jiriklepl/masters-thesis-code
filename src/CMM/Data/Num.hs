{-# LANGUAGE Safe #-}

module CMM.Data.Num
  ( Num((+), (-), (*), abs, negate, signum, fromInteger)
  ) where

import safe Prelude (Num((*), (+), (-), abs, fromInteger, negate, signum))

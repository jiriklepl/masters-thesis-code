{-# LANGUAGE Safe #-}

module CMM.Inference.GetParent where

import safe Control.Monad ( Monad )

class Monad m => GetParent m t | m -> t where
  getParent :: m t

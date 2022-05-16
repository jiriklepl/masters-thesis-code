{-# LANGUAGE Safe #-}

module CMM.Inference.GetParent where

import safe Control.Monad ( Monad )
import safe CMM.Inference.TypeVar ( TypeVar )

class Monad m => GetParent m where
  getParent :: m TypeVar

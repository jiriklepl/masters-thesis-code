{-# LANGUAGE Safe #-}

module CMM.Err.IsError where

import safe Data.Data (Typeable)
import safe Data.Eq (Eq)
import safe Text.Show (Show)

class (Show error, Eq error, Typeable error) =>
      IsError error

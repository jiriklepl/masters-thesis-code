{-# LANGUAGE Safe #-}

module CMM.Err.IsError where

import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Text.Show (Show)

import safe Prettyprinter ( Pretty )

class (Show error, Pretty error, Eq error, Data error) =>
      IsError error

{-# LANGUAGE Safe #-}

module CMM.Err.IsError where

import safe Data.Data (Data)

import safe Prettyprinter (Pretty)

-- | Class for error objects
class (Show error, Pretty error, Eq error, Data error) =>
      IsError error

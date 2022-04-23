{-# LANGUAGE Safe #-}

module CMM.Err.Severity where

import safe Data.Eq (Eq)
import safe Data.Ord (Ord)
import safe Text.Show (Show)

import safe CMM.Data.Bounded (Bounded)

data Severity
  = InfoLevel
  | WarningLevel
  | ErrorLevel
  | FatalLevel
  deriving (Eq, Ord, Show)

deriving instance Bounded Severity

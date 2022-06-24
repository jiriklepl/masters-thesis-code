{-# LANGUAGE Safe #-}

module CMM.Err.Severity where

import safe Data.Eq (Eq)
import safe Data.Ord (Ord)
import safe Text.Show (Show)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty) )

import safe CMM.Data.Bounded (Bounded)

data Severity
  = InfoLevel
  | WarningLevel
  | ErrorLevel
  deriving (Eq, Ord, Show, Data)

deriving instance Bounded Severity

instance Pretty Severity where
  pretty = \case
    InfoLevel -> "Info"
    WarningLevel -> "Warning"
    ErrorLevel -> "Error"

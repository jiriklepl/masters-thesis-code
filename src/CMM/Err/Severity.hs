{-# LANGUAGE Safe #-}

module CMM.Err.Severity where

import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty) )


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

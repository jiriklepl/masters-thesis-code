{-# LANGUAGE Safe #-}

module CMM.Err.Severity where

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty))

-- | Represents a severity of an `Error`
data Severity
  = InfoLevel -- ^ Just some information due to verbosity settings
  | WarningLevel -- ^ The error is not fatal, but is wrong
  | ErrorLevel -- ^ The error is fatal, the compiler does not perform any more  steps of the compilation pipeline
  deriving (Eq, Ord, Show, Data)

deriving instance Bounded Severity

instance Pretty Severity where
  pretty =
    \case
      InfoLevel -> "Info"
      WarningLevel -> "Warning"
      ErrorLevel -> "Error"

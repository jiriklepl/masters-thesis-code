{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Err.Error where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Data (cast, Data)
import safe Data.Generics
    ( Data(gfoldl, dataTypeOf, toConstr, gunfold),
      Constr,
      DataType,
      mkConstr,
      mkDataType,
      Fixity(Prefix))

import safe Prettyprinter ( Pretty(pretty), (<+>))

import safe CMM.Err.IsError (IsError)
import safe CMM.Err.Severity (Severity (InfoLevel, WarningLevel, ErrorLevel))

data Error where
  Error
    :: forall err . IsError err =>
       { _errSeverity :: Severity
       , _errContent :: err}
    -> Error

instance Data Error where
  gfoldl _ z e = z e
  gunfold _ z _ = z undefined
  toConstr _ = con
  dataTypeOf _ = ty

con :: Constr
con = mkConstr ty "Error" [] Prefix

ty :: DataType
ty = mkDataType "CMM.Err.Error.My" [con]

deriving instance Show Error

instance Eq Error where
  Error s e == Error s' e'
    | s == s' =
      case cast e' of
        Just e'' -> e == e''
        Nothing -> False
    | otherwise = False

instance Pretty Error where
  pretty Error {_errSeverity = severity, _errContent = content} = pretty severity <> ":" <+> pretty content

-- | Takes an object representing an error and wraps it in `Error` with `InfoLevel` severity
makeInfo :: IsError err => err -> Error
makeInfo = Error InfoLevel

-- | Returns `True` iff the given `Error` object represents an info
isInfo :: Error -> Bool
isInfo Error {_errSeverity = severity} = severity == InfoLevel

-- | Takes an object representing an error and wraps it in `Error` with `WarningLevel` severity
makeWarning :: IsError err => err -> Error
makeWarning = Error WarningLevel

-- | Returns `True` iff the given `Error` object represents a warning
isWarning :: Error -> Bool
isWarning Error {_errSeverity = severity} = severity == WarningLevel

-- | Takes an object representing an error and wraps it in `Error` with `ErrorLevel` severity
makeError :: IsError err => err -> Error
makeError = Error ErrorLevel

-- | Returns `True` iff the given `Error` object represents an error (with `ErrorLevel` severity)
isError :: Error -> Bool
isError Error {_errSeverity = severity} = severity == ErrorLevel

makeFieldsNoPrefix ''Error

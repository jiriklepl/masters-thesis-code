{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Err.Error where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Bool (Bool(False), otherwise)
import safe Data.Data (cast)
import safe Data.Eq (Eq((==)))
import safe Data.Maybe (Maybe(Just, Nothing))
import safe Text.Show (Show)

import safe CMM.Err.IsError (IsError)
import safe CMM.Err.Severity (Severity)

data Error where
  Error
    :: forall err . IsError err =>
       { _errSeverity :: Severity
       , _errContent :: err}
    -> Error

deriving instance Show Error

instance Eq Error where
  Error s e == Error s' e'
    | s == s' =
      case cast e' of
        Just e'' -> e == e''
        Nothing -> False
    | otherwise = False

makeFieldsNoPrefix ''Error

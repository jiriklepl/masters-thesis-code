{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Err.State where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Control.Lens.Getter (view)
import safe Control.Lens.Setter ((%=))
import safe Control.Monad.State (MonadState)
import safe Data.Eq (Eq((==)))
import safe Data.Function (($))
import safe Data.Int (Int)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Semigroup (Semigroup((<>)))
import safe Text.Show (Show)

import safe CMM.Data.List (count)
import safe CMM.Err.Error (Error(Error), HasErrSeverity(errSeverity))
import safe CMM.Err.IsError (IsError)
import safe CMM.Err.Severity (Severity(ErrorLevel, InfoLevel, WarningLevel))
import safe Control.Lens.Type (Lens')

newtype ErrorState =
  ErrorState
    { _errors :: [Error]
    }
  deriving (Show, Eq)

instance Semigroup ErrorState where
  ErrorState errs <> ErrorState errs' = ErrorState $ errs <> errs'

instance Monoid ErrorState where
  mempty = ErrorState mempty

class HasErrorState s a | s -> a where
  errorState :: Lens' s a

makeFieldsNoPrefix ''ErrorState

countErrors :: Severity -> ErrorState -> Int
countErrors s (ErrorState errs) = count checkSeverity errs
  where
    checkSeverity e = view errSeverity e == s

addError :: IsError err => Severity -> err -> ErrorState -> ErrorState
addError s e (ErrorState errs) = ErrorState $ Error s e : errs

registerInfo ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerInfo err = errorState %= addError InfoLevel err

registerWarning ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerWarning err = errorState %= addError WarningLevel err

registerError ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerError err = errorState %= addError ErrorLevel err

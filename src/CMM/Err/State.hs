{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Err.State where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Control.Lens.Getter (view)
import safe Control.Lens.Setter ((%=))
import safe Control.Lens.Type (Lens')
import safe Control.Monad.State (MonadState)
import safe Data.Eq (Eq((==)))
import safe Data.Function (($), id)
import safe Data.Int (Int)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Semigroup (Semigroup((<>)))
import safe Text.Show (Show)
import Data.Data ( Data )

import safe CMM.Data.List (count)
import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Err.Error (Error(Error), HasErrSeverity(errSeverity))
import safe CMM.Err.IsError (IsError)
import safe CMM.Err.Severity (Severity(ErrorLevel, InfoLevel, WarningLevel))

newtype ErrorState =
  ErrorState
    { _errors :: [Error]
    }
  deriving (Show, Eq, Data)

instance Semigroup ErrorState where
  ErrorState errs <> ErrorState errs' = ErrorState $ errs <> errs'

instance Monoid ErrorState where
  mempty = ErrorState mempty

instance Nullable ErrorState where
  nullVal = ErrorState mempty

class a ~ ErrorState =>
      HasErrorState s a
  | s -> a
  where
  errorState :: Lens' s a

instance HasErrorState ErrorState ErrorState where
  errorState = id

makeFieldsNoPrefix ''ErrorState

countSeverity :: Severity -> ErrorState -> Int
countSeverity s = \case
  ErrorState errs -> count checkSeverity errs
  where
    checkSeverity e = view errSeverity e == s

countErrors :: ErrorState -> Int
countErrors = countSeverity ErrorLevel

countWarnings :: ErrorState -> Int
countWarnings = countSeverity WarningLevel

countInfos :: ErrorState -> Int
countInfos = countSeverity InfoLevel

addError :: IsError err => Severity -> err -> ErrorState -> ErrorState
addError s e = \case
  ErrorState errs -> ErrorState $ Error s e : errs

registerInfo ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerInfo err = errorState %= addError InfoLevel err

registerWarning ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerWarning err = errorState %= addError WarningLevel err

registerError ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerError err = errorState %= addError ErrorLevel err

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Err.State where

import safe Control.Lens (Lens', (%=), makeFieldsNoPrefix, uses, view)
import safe Control.Monad.State (MonadState)
import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), vsep)

import safe CMM.Data.List (count)
import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Err.Error (Error(Error), HasErrSeverity(errSeverity))
import safe CMM.Err.IsError (IsError)
import safe CMM.Err.Severity (Severity(ErrorLevel, InfoLevel, WarningLevel))

-- | Contains the list of `Error`s
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

instance Pretty ErrorState where
  pretty =
    \case
      ErrorState errs -> vsep . fmap pretty $ reverse errs

-- | A lens for all state objects containing `ErrorState`
class a ~ ErrorState =>
      HasErrorState s a
  | s -> a
  where
  errorState :: Lens' s a

instance HasErrorState ErrorState ErrorState where
  errorState = id

makeFieldsNoPrefix ''ErrorState

-- | The `ErrorState` retrieved from the monadic state is empty
nullErrorState :: (HasErrorState s e, MonadState s m) => m Bool
nullErrorState = uses errorState (== nullVal)

-- | The `ErrorState` retrieved from the monadic state has no errors
noErrorsState :: (HasErrorState s e, MonadState s m) => m Bool
noErrorsState = uses errorState $ (== 0) . countErrors

-- | Counts all error objects with the given severity level in the given `ErrorState`
countSeverity :: Severity -> ErrorState -> Int
countSeverity s =
  \case
    ErrorState errs -> count checkSeverity errs
  where
    checkSeverity e = view errSeverity e == s

-- | Counts all errors inside the given `ErrorState`
countErrors :: ErrorState -> Int
countErrors = countSeverity ErrorLevel

-- | Counts all warnings inside the given `ErrorState`
countWarnings :: ErrorState -> Int
countWarnings = countSeverity WarningLevel

-- | Counts all infos inside the given `ErrorState`
countInfos :: ErrorState -> Int
countInfos = countSeverity InfoLevel

-- | Adds an error object with the given `Severity` to the given `ErrorState`
addError :: IsError err => Severity -> err -> ErrorState -> ErrorState
addError s e =
  \case
    ErrorState errs -> ErrorState $ Error s e : errs

-- | Adds the given error object to the `ErrorState` in the monadic state as info
registerInfo ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerInfo err = errorState %= addError InfoLevel err

-- | Adds the given error object to the `ErrorState` in the monadic state as warning
registerWarning ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerWarning err = errorState %= addError WarningLevel err

-- | Adds the given error object to the `ErrorState` in the monadic state as error
registerError ::
     (IsError err, HasErrorState s ErrorState, MonadState s m) => err -> m ()
registerError err = errorState %= addError ErrorLevel err

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Parser.ASTError where

import safe Control.Monad.State (MonadState)
import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), (<+>))

import safe CMM.Err.IsError (IsError)
import safe CMM.Err.State
  ( ErrorState
  , HasErrorState
  , registerError
  , registerInfo
  , registerWarning
  )
import safe CMM.Parser.GetPos (GetPos(getPos), SourcePos, sourcePosPretty)

-- | holds an error along with a source location for nicer error messages
data ASTError err =
  ASTError SourcePos err
  deriving (Eq, Show, Functor, Foldable, Traversable, Data)

deriving instance IsError err => IsError (ASTError err)

instance Pretty err => Pretty (ASTError err) where
  pretty =
    \case
      ASTError pos err -> pretty (sourcePosPretty pos) <+> pretty err

-- | wraps the given error to a new `ASTError` with a location retrieved from the given object
--   the result is then added to the error state of the monadic state as an info
registerASTInfo ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, GetPos n)
  => n
  -> err
  -> m ()
registerASTInfo n err = registerInfo $ n `makeASTError` err

-- | wraps the given error to a new `ASTError` with a location retrieved from the given object
--   the result is then added to the error state of the monadic state as an error
registerASTError ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, GetPos n)
  => n
  -> err
  -> m ()
registerASTError n err = registerError $ n `makeASTError` err

-- | wraps the given error to a new `ASTError` with a location retrieved from the given object
--   the result is then added to the error state of the monadic state as a warning
registerASTWarning ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, GetPos n)
  => n
  -> err
  -> m ()
registerASTWarning n err = registerWarning $ n `makeASTError` err

-- makes an `ASTError` from the given error object and a location retrieved from the given object
makeASTError :: GetPos a => a -> err -> ASTError err
makeASTError n err = getPos n `ASTError` err

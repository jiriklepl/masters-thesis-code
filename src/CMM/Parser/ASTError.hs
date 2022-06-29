{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Parser.ASTError where

import safe Control.Monad.State (MonadState)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty), (<+>) )

import safe CMM.Err.IsError (IsError)
import safe CMM.Err.State
  ( ErrorState
  , HasErrorState
  , registerError
  , registerInfo
  , registerWarning
  )
import safe CMM.Parser.HasPos (HasPos(getPos), SourcePos, sourcePosPretty)

data ASTError err =
  ASTError SourcePos err
  deriving (Eq, Show, Functor, Foldable, Traversable, Data)

deriving instance IsError err => IsError (ASTError err)

instance Pretty err => Pretty (ASTError err) where
  pretty = \case
    ASTError pos err -> pretty (sourcePosPretty pos) <+> pretty err

registerASTInfo ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, HasPos n)
  => n
  -> err
  -> m ()
registerASTInfo n err = registerInfo $ n `makeASTError` err

registerASTError ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, HasPos n)
  => n
  -> err
  -> m ()
registerASTError n err = registerError $ n `makeASTError` err

registerASTWarning ::
     (IsError err, HasErrorState s ErrorState, MonadState s m, HasPos n)
  => n
  -> err
  -> m ()
registerASTWarning n err = registerWarning $ n `makeASTError` err

makeASTError :: HasPos a => a -> err -> ASTError err
makeASTError n err = getPos n `ASTError` err

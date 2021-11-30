{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module CMM.AST.Variables.State where

import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State (MonadIO, MonadState)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Prettyprinter (Pretty)

import safe CMM.AST.HasName (HasName(..))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Pretty ()
import safe CMM.Warnings (makeMessage, mkError, mkWarning)
import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((%=), (+=))

data CollectedVariables =
  CollectedVariables
    { _variables :: Set Text
    , _typeVariables :: Set Text
    , _errors :: Int
    , _warnings :: Int
    }

initCollectedVariables :: CollectedVariables
initCollectedVariables =
  CollectedVariables
    {_variables = mempty, _typeVariables = mempty, _errors = 0, _warnings = 0}

type MonadCollectVariables m = (MonadState CollectedVariables m, MonadIO m)

makeLenses ''CollectedVariables

registerError ::
     (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning ::
     (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message

addVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addVar node var = do
  uses variables (var `Set.member`) >>= \case
    True -> registerError node "Duplicate variable"
    False -> variables %= Set.insert var

addVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m) => n -> m n
addVarTrivial n = n <$ addVar n (getName n)

addTVar :: (HasPos n, Pretty n, MonadCollectVariables m) => n -> Text -> m ()
addTVar node tVar = do
  uses variables (tVar `Set.member`) >>= \case
    True -> registerError node "Duplicate type variable"
    False -> variables %= Set.insert tVar

addTVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m) => n -> m n
addTVarTrivial n = n <$ addTVar n (getName n)

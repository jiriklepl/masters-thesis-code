{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- TODO: make an alias for `Map Text (SourcePos, TypeKind)`
module CMM.AST.Variables.State where

import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((%=), (+=))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State (MonadIO, MonadState)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Text (Text)
import safe Prettyprinter (Pretty)

import safe CMM.AST.HasName (HasName(..))
import safe CMM.Inference.Type (TypeKind)
import safe CMM.Parser.HasPos (HasPos(getPos), SourcePos)
import safe CMM.Pretty ()
import safe CMM.Warnings (makeMessage, mkError, mkWarning)
import Control.Monad (unless)

data CollectedVariables =
  CollectedVariables
    { _variables :: Map Text (SourcePos, TypeKind)
    , _funcVariables :: Map Text (SourcePos, TypeKind)
    , _typeVariables :: Map Text (SourcePos, TypeKind)
    , _typeConstants :: Map Text (SourcePos, TypeKind)
    , _typeClasses :: Map Text (SourcePos, TypeKind, Set Text {- method decls -})
    , _errors :: Int
    , _warnings :: Int
    }

initCollectedVariables :: CollectedVariables
initCollectedVariables =
  CollectedVariables
    { _variables = mempty
    , _funcVariables = mempty
    , _typeVariables = mempty
    , _typeConstants = mempty
    , _typeClasses = mempty
    , _errors = 0
    , _warnings = 0
    }

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

addVar ::
     (HasPos n, Pretty n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> m ()
addVar node var tKind = do
  uses variables (var `Map.member`) >>= \case
    True -> registerError node "Duplicate variable"
    False -> variables %= Map.insert var (getPos node, tKind)

addVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> m n
addVarTrivial n tKind = n <$ addVar n (getName n) tKind

addTCon ::
     (HasPos n, Pretty n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> m ()
addTCon node tVar tKind = do
  uses typeConstants (tVar `Map.member`) >>= \case
    True -> registerError node "Duplicate type variable"
    False -> typeConstants %= Map.insert tVar (getPos node, tKind)

addTConTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> m n
addTConTrivial n tKind = n <$ addTCon n (getName n) tKind

addTVar ::
     (HasPos n, Pretty n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> m ()
addTVar node tVar tKind = do
    uses typeVariables (tVar `Map.member`) >>= flip unless
      (typeVariables %= Map.insert tVar (getPos node, tKind))

addTVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> m n
addTVarTrivial n tKind = n <$ addTVar n (getName n) tKind

addFVar ::
     (HasPos n, Pretty n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> m ()
addFVar node var tKind = do
  uses funcVariables (var `Map.member`) >>= \case
    True -> registerError node "Duplicate function variable"
    False -> funcVariables %= Map.insert var (getPos node, tKind)

addFVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> m n
addFVarTrivial n tKind = n <$ addFVar n (getName n) tKind

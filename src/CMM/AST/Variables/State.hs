{-# LANGUAGE Safe #-}

-- TODO: make an alias for `Map Text (SourcePos, TypeKind)`
module CMM.AST.Variables.State
  ( module CMM.AST.Variables.State.Impl
  , module CMM.AST.Variables.State
  ) where

import safe Prelude

import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((%=), (+=))
import safe Control.Monad (unless)
import safe Control.Monad.State (MonadIO, MonadState)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Text (Text)
import safe Prettyprinter (Pretty)

import safe CMM.AST.HasName (HasName(..))
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Parser.HasPos (HasPos(getPos))
import safe CMM.Pretty ()
import safe CMM.Warnings (makeMessage, mkError, mkWarning)

import safe CMM.AST.Variables.State.Impl

initCollector :: Collector
initCollector =
  Collector
    { _variables = mempty
    , _funcVariables = mempty
    , _funcInstVariables = mempty
    , _typeConstants = mempty
    , _typeVariables = mempty
    , _typeClasses = mempty
    , _structMembers = mempty
    , _errors = 0
    , _warnings = 0
    }

type MonadCollectVariables m = (MonadState Collector m, MonadIO m)

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

addTVar :: (HasPos n, MonadCollectVariables m) => n -> Text -> TypeKind -> m ()
addTVar node tVar tKind = do
  uses typeVariables (tVar `Map.member`) >>=
    flip unless (typeVariables %= Map.insert tVar (getPos node, tKind))

addTVarTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
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

addFIVar ::
     (HasPos n, Pretty n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> m ()
addFIVar node var tKind = do
  uses funcInstVariables (var `Map.member`) >>= \case
    True -> registerError node "Duplicate function variable"
    False -> funcInstVariables %= Map.insert var (getPos node, tKind)

addFIVarTrivial ::
     (HasPos n, Pretty n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> m n
addFIVarTrivial n tKind = n <$ addFVar n (getName n) tKind

addTClass ::
     (HasPos n, MonadCollectVariables m)
  => n
  -> Text
  -> TypeKind
  -> Set Text
  -> m ()
addTClass node tVar tKind methods = do
  uses typeClasses (tVar `Map.member`) >>=
    flip unless (typeClasses %= Map.insert tVar (getPos node, tKind, methods))

addTClassTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> Set Text
  -> m n
addTClassTrivial n tKind methods = n <$ addTClass n (getName n) tKind methods

addSMem :: (HasPos n, MonadCollectVariables m) => n -> Text -> TypeKind -> m ()
addSMem node tVar tKind = do
  uses structMembers (tVar `Map.member`) >>=
    flip unless (structMembers %= Map.insert tVar (getPos node, tKind))

addSMemTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addSMemTrivial n tKind = n <$ addSMem n (getName n) tKind

{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

-- TODO: make an alias for `Map Text (SourcePos, TypeKind)`
module CMM.AST.Variables.State
  ( module CMM.AST.Variables.State.Impl
  , module CMM.AST.Variables.State
  ) where

import safe Control.Lens.Getter (uses)
import safe Control.Lens.Setter ((%=))
import safe Control.Monad (Functor((<$)), Monad((>>=)), unless)
import safe Control.Monad.State (MonadState)
import safe Data.Bool (Bool(False, True))
import safe Data.Function (($), flip)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.AST.HasName (HasName(getName))
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Parser.HasPos (HasPos(getPos), SourcePos)
import safe CMM.Pretty ()

import safe CMM.AST.Variables.Error
  ( VariablesError
  , duplicateFunctionVariable
  , duplicateTypeConstant
  , duplicateTypeVariable
  , duplicateVariable
  )
import safe CMM.AST.Variables.State.Impl
  ( Collector(Collector)
  , funcInstVariables
  , funcVariables
  , initCollector
  , structMembers
  , typeClasses
  , typeConstants
  , typeVariables
  , variables
  )
import safe CMM.Parser.ASTError (registerASTError)
import Control.Lens.Type (Lens')

type MonadCollectVariables m = MonadState Collector m

addVar ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m ()
addVar = addVarImpl variables duplicateVariable

addVarImpl ::
     (MonadCollectVariables m, HasPos n, HasName n)
  => Lens' Collector (Map.Map Text (SourcePos, b))
  -> (n -> VariablesError)
  -> n
  -> b
  -> m ()
addVarImpl place err node tKind = do
  uses place (getName node `Map.member`) >>= \case
    True -> registerASTError node $ err node
    False -> place %= Map.insert (getName node) (getPos node, tKind)

addVarTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addVarTrivial n tKind = n <$ addVar n tKind

addTCon ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m ()
addTCon = addVarImpl typeConstants duplicateTypeConstant

addTConTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addTConTrivial n tKind = n <$ addTCon n tKind

addTVar ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m ()
addTVar = addVarImpl typeVariables duplicateTypeVariable

addTVarTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addTVarTrivial n tKind = n <$ addTVar n tKind

addFVar ::
     (HasPos n, MonadCollectVariables m, HasName n) => n -> TypeKind -> m ()
addFVar = addVarImpl funcVariables duplicateFunctionVariable

addFVarTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addFVarTrivial n tKind = n <$ addFVar n tKind

addFIVar ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m ()
addFIVar = addVarImpl funcInstVariables duplicateFunctionVariable

addFIVarTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addFIVarTrivial n tKind = n <$ addFVar n tKind

addTClass ::
     (HasPos n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> Set Text
  -> m ()
addTClass node tKind methods = do
  uses typeClasses (getName node `Map.member`) >>=
    flip
      unless
      (typeClasses %= Map.insert (getName node) (getPos node, tKind, methods))

addTClassTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m)
  => n
  -> TypeKind
  -> Set Text
  -> m n
addTClassTrivial n tKind methods = n <$ addTClass n tKind methods

addSMem ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m ()
addSMem node tKind = do
  uses structMembers (getName node `Map.member`) >>=
    flip
      unless
      (structMembers %= Map.insert (getName node) (getPos node, tKind))

addSMemTrivial ::
     (HasPos n, HasName n, MonadCollectVariables m) => n -> TypeKind -> m n
addSMemTrivial n tKind = n <$ addSMem n tKind

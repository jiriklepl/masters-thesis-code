{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Variables.State.Impl where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Parser.HasPos (SourcePos)

data CollectorState =
  CollectorState
    { _variables :: Map Text (SourcePos, TypeKind)
    , _funcVariables :: Map Text (SourcePos, TypeKind)
    , _funcInstVariables :: Map Text (SourcePos, TypeKind)
    , _typeConstants :: Map Text (SourcePos, TypeKind)
    , _typeAliases :: Map Text (SourcePos, TypeKind)
    , _typeVariables :: Map Text (SourcePos, TypeKind)
    , _typeClasses :: Map Text (SourcePos, TypeKind, Set Text) {- method decls -}
    , _structMembers :: Map Text (SourcePos, TypeKind)
    , _errorState :: ErrorState
    }

initCollector :: CollectorState
initCollector =
  CollectorState
    { _variables = mempty
    , _funcVariables = mempty
    , _funcInstVariables = mempty
    , _typeConstants = mempty
    , _typeAliases = mempty
    , _typeVariables = mempty
    , _typeClasses = mempty
    , _structMembers = mempty
    , _errorState = mempty
    }

makeFieldsNoPrefix ''CollectorState

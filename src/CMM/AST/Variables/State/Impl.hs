{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Variables.State.Impl where

import safe Control.Lens (makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Parser.GetPos (SourcePos)

-- | Contains various data used by the Variable `Collector`
--
-- The collector is used to infer (type) variable/constant declarations
data CollectorState =
  CollectorState
    { _variables :: Map Text (SourcePos, TypeKind) -- ^ Inferred variable declarations
    , _funcVariables :: Map Text (SourcePos, TypeKind) -- ^ Inferred function declarations
    , _funcInstVariables :: Map Text (SourcePos, TypeKind) -- ^ Inferred function instance declarations
    , _typeConstants :: Map Text (SourcePos, TypeKind) -- ^ Inferred type constant declarations
    , _typeAliases :: Map Text (SourcePos, TypeKind) -- ^ Inferred type alias declarations
    , _typeVariables :: Map Text (SourcePos, TypeKind) -- ^ Inferred type variable declarations
    , _typeClasses :: Map Text (SourcePos, TypeKind, Set Text) -- ^ Inferred type-class declarations
    , _structMembers :: Map Text (SourcePos, TypeKind) -- ^ Inferred structure member declarations
    , _errorState :: ErrorState -- ^ the `ErrorState` of `Collector`
    }

-- | Initiates an empty `CollectorState`
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

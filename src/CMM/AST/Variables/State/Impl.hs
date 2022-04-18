{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.AST.Variables.State.Impl where

import safe Prelude

import safe Control.Lens.TH (makeLenses)

import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Inference.TypeKind (TypeKind)
import safe CMM.Parser.HasPos (SourcePos)

data Collector =
  Collector
    { _variables :: Map Text (SourcePos, TypeKind)
    , _funcVariables :: Map Text (SourcePos, TypeKind)
    , _funcInstVariables :: Map Text (SourcePos, TypeKind)
    , _typeConstants :: Map Text (SourcePos, TypeKind)
    , _typeVariables :: Map Text (SourcePos, TypeKind)
    , _typeClasses :: Map Text (SourcePos, TypeKind, Set Text) {- method decls -}
    , _structMembers :: Map Text (SourcePos, TypeKind)
    , _errors :: Int
    , _warnings :: Int
    }

makeLenses ''Collector

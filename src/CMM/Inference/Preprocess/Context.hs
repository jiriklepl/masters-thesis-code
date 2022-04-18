{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Prelude

import safe Data.Text (Text)

import safe CMM.AST.HasName (HasName(..))
import safe CMM.Inference.Preprocess.HasTypeHole (HasTypeHole(..))
import safe CMM.Inference.Preprocess.TypeHole (TypeHole(EmptyTypeHole))
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)

data Context
  = GlobalCtx
  | ClassCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | InstanceCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | FunctionCtx (Text, TypeHole) TypeHandle
  -- | SectionCtx Text

instance HasName Context where
  getName GlobalCtx = undefined -- error
  getName (ClassCtx (name, _) _ _) = name
  getName (InstanceCtx (name, _) _ _) = name
  getName (FunctionCtx (name, _) _) = name

instance HasTypeHole Context where
  getTypeHole GlobalCtx = EmptyTypeHole
  getTypeHole (FunctionCtx (_, handle) _) = handle
  getTypeHole (ClassCtx (_, handle) _ _) = handle
  getTypeHole (InstanceCtx (_, handle) _ _) = handle

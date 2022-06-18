{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Data.Text (Text)

import safe GHC.Err (undefined)

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Inference.Preprocess.TypeHole (TypeHole(EmptyTypeHole), HasTypeHole(getTypeHole))
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)

data Context
  = GlobalCtx
  | ClassCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | FunctionCtx (Text, TypeHole) TypeHandle
  | InstanceCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | StructCtx (Text, TypeHole) (Text, Type)
  --- | SectionCtx Text

instance GetName Context where
  getName =
    \case
      GlobalCtx -> undefined -- error
      ClassCtx (name, _) _ _ -> name
      FunctionCtx (name, _) _ -> name
      InstanceCtx (name, _) _ _ -> name
      StructCtx (name, _) _ -> name

instance HasTypeHole Context where
  getTypeHole =
    \case
      GlobalCtx -> EmptyTypeHole
      ClassCtx (_, handle) _ _ -> handle
      FunctionCtx (_, handle) _ -> handle
      InstanceCtx (_, handle) _ _ -> handle
      StructCtx (_, handle) _ -> handle

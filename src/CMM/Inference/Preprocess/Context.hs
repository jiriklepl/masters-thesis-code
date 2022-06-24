{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Data.Data (Data)
import safe Data.Maybe (Maybe(Nothing))
import safe Data.Text (Text)

import safe GHC.Err (undefined)

import safe CMM.AST (Conv)
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , TypeHole(EmptyTypeHole)
  )
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)

data Context
  = GlobalCtx
  | ClassCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | FunctionCtx (Text, TypeHole) TypeHandle (Maybe Conv)
  | InstanceCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | StructCtx (Text, TypeHole) (Text, Type)
  deriving (Data)
  --- | SectionCtx Text

instance GetName Context where
  getName =
    \case
      GlobalCtx -> undefined -- error
      ClassCtx (name, _) _ _ -> name
      FunctionCtx (name, _) _ _ -> name
      InstanceCtx (name, _) _ _ -> name
      StructCtx (name, _) _ -> name

instance HasTypeHole Context where
  getTypeHole =
    \case
      GlobalCtx -> EmptyTypeHole
      ClassCtx (_, handle) _ _ -> handle
      FunctionCtx (_, handle) _ _ -> handle
      InstanceCtx (_, handle) _ _ -> handle
      StructCtx (_, handle) _ -> handle

instance GetConv Context where
  getConv =
    \case
      FunctionCtx _ _ mConv -> mConv
      _ -> Nothing

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe CMM.AST (Conv)
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , TypeHole
  )
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)

data Context
  = GlobalCtx
  | ClassCtx { ctxName ::Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type), superSupers :: [(Text, Type)] } -- className, classHandle, superClassHandles
  | FunctionCtx { ctxName :: Text, ctxHole :: TypeHole, ctxFunctionHandle :: TypeHandle, ctxFunctionConv :: Maybe Conv }
  | InstanceCtx { ctxName :: Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type), ctxSupers :: [(Text, Type)] } -- className, classHandle, superClassHandles
  | StructCtx { ctxName :: Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type) }
  deriving (Data)
  --- | SectionCtx Text

instance GetName Context where
  getName = ctxName

instance HasTypeHole Context where
  getTypeHole = ctxHole

instance GetConv Context where
  getConv = \case
      FunctionCtx {ctxFunctionConv} -> ctxFunctionConv
      _ -> Nothing

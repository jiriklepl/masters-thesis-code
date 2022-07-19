{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe CMM.AST (Conv)
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName (GetName(getName, mapName))
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration, setElaboration)
  , Elaboration
  )
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.Properties (Properties)
import CMM.Utils (logicError)

-- | An object representing the given context
data Context
  = GlobalCtx -- ^ Represents the global context
  | ClassCtx { ctxName ::Text, ctxElab :: Elaboration, ctxConstraint :: (Text, Type), superSupers :: [(Text, Type)] } -- ^ class name, class type properties, super-class type properties
  | FunctionCtx { ctxName :: Text, ctxElab :: Elaboration, ctxFunctionHandle :: Properties, ctxFunctionConv :: Maybe Conv } -- ^ function name, function type properties, the call convention (eg foreign "C")
  | InstanceCtx { ctxName :: Text, ctxElab :: Elaboration, ctxConstraint :: (Text, Type), ctxSupers :: [(Text, Type)] } -- ^ class name, class type properties, assumptions type properties
  | StructCtx { ctxName :: Text, ctxElab :: Elaboration, ctxConstraint :: (Text, Type) }
  deriving (Data)
  --- | SectionCtx Text

instance GetName Context where
  getName = ctxName
  mapName _ GlobalCtx {} = logicError
  mapName f ctx = ctx{ctxName = f `mapName` ctxName ctx}

instance HasElaboration Context where
  getElaboration = ctxElab
  setElaboration s ctx = ctx{ctxElab=s}

instance GetConv Context where
  getConv = \case
      FunctionCtx {ctxFunctionConv} -> ctxFunctionConv
      _ -> Nothing

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Context where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe CMM.AST (Conv)
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole, setTypeHole)
  , TypeHole
  )
import safe CMM.Inference.Type (Type)
import safe CMM.Inference.TypeHandle (TypeHandle)

-- | An object representing the given context
data Context
  = GlobalCtx -- ^ Represents the global context
  | ClassCtx { ctxName ::Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type), superSupers :: [(Text, Type)] } -- ^ class name, class handle, super-class handles
  | FunctionCtx { ctxName :: Text, ctxHole :: TypeHole, ctxFunctionHandle :: TypeHandle, ctxFunctionConv :: Maybe Conv } -- ^ function name, function handle, the call convention (eg foreign "C")
  | InstanceCtx { ctxName :: Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type), ctxSupers :: [(Text, Type)] } -- ^ class name, class handle, assumptions handles
  | StructCtx { ctxName :: Text, ctxHole :: TypeHole, ctxConstraint :: (Text, Type) }
  deriving (Data)
  --- | SectionCtx Text

instance GetName Context where
  getName = ctxName

instance HasTypeHole Context where
  getTypeHole = ctxHole
  setTypeHole s ctx = ctx{ctxHole=s}

instance GetConv Context where
  getConv = \case
      FunctionCtx {ctxFunctionConv} -> ctxFunctionConv
      _ -> Nothing

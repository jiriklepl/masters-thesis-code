{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module CMM.Inference.InferPreprocessor where

import Control.Lens.TH ( makeLenses )
import safe Control.Monad.State.Lazy ( MonadState )
import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST

data TypeHandle = NoType

class HasTypeHandle a where
    getTypeHandle :: a -> TypeHandle

instance HasTypeHandle a => HasTypeHandle (AST.Annot n a) where
  getTypeHandle = getTypeHandle . AST.takeAnnot

class HasTypeHandle b => WithTypeHandle a b | a -> b, b -> a where
    withTypeHandle :: TypeHandle -> a -> b

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor = InferPreprocessor
    {
    }

makeLenses ''InferPreprocessor

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor = InferPreprocessor
    {
    }

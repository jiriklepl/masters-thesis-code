{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- TODO: rename to ...State

module CMM.Inference.InferPreprocessor where

import safe Data.Map (Map)
import safe Data.Text (Text)
import safe Data.Function
import safe qualified Data.Map as Map
import Control.Lens.TH ( makeLenses )
import safe Control.Monad.State.Lazy ( MonadState )
import safe Control.Lens.Getter
import safe Control.Lens.Setter

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST

data TypeHandle
    = NoType
    | TypeVar Int
    | TypeTBits Int

data Fact
type Facts = [Fact]

class HasTypeHandle a where
    getTypeHandle :: a -> TypeHandle

instance HasTypeHandle a => HasTypeHandle (AST.Annot n a) where
  getTypeHandle = getTypeHandle . AST.takeAnnot

class HasTypeHandle b => WithTypeHandle a b | a -> b, b -> a where
    withTypeHandle :: TypeHandle -> a -> b

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor = InferPreprocessor
    { _variables :: [Map Text TypeHandle]
    , _typeVariables :: [Map Text TypeHandle]
    , _facts :: Facts
    , _handleCounter :: Int
    }

makeLenses ''InferPreprocessor

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor = InferPreprocessor
    { _variables = pure mempty
    , _typeVariables = pure mempty
    , _facts = mempty
    , _handleCounter = 0
    }


-- returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupVar = uses variables . lookupVarImpl

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupTVar = uses typeVariables . lookupVarImpl

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHandle
lookupVarImpl name = go
    where go (vars:others) = name `Map.lookup` vars & \case
            Just handle -> handle
            Nothing -> go others
          go [] = NoType

storeVar :: MonadInferPreprocessor m => Text -> TypeHandle -> m ()
storeVar name handle = variables %= storeVarImpl name handle

storeTVar :: MonadInferPreprocessor m => Text -> TypeHandle -> m ()
storeTVar name handle = typeVariables %= storeVarImpl name handle

storeVarImpl :: Ord k => k -> a -> [Map k a] -> [Map k a]
storeVarImpl name handle ~(vars : others) =
    Map.insert name handle vars : others

pushVars :: MonadInferPreprocessor m => m ()
pushVars = variables %= (mempty:)

pushTVars :: MonadInferPreprocessor m => m ()
pushTVars = typeVariables %= (mempty:)

-- TODO: Do not over-pop
popVars :: MonadInferPreprocessor m => m ()
popVars = variables %= tail

popTVars :: MonadInferPreprocessor m => m ()
popTVars = typeVariables %= tail

storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact = (facts %=) . (:)

freshTypeHandle :: MonadInferPreprocessor m => m TypeHandle
freshTypeHandle = do
    handleCounter += 1
    TypeVar <$> use handleCounter

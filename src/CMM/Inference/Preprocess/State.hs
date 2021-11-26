{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Inference.Preprocess.State where

import safe Control.Lens.Getter
import safe Control.Lens.Setter
import Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Function
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe Data.Text (Text)

import safe CMM.Lens
import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
import safe CMM.Inference.Type
import safe qualified CMM.Parser.HasPos as AST

class HasTypeHandle a where
  getTypeHandle :: a -> TypeHandle

instance HasTypeHandle (AST.SourcePos, TypeHandle) where
  getTypeHandle = snd

instance HasTypeHandle a => HasTypeHandle (AST.Annot n a) where
  getTypeHandle = getTypeHandle . AST.takeAnnot

class HasTypeHandle b =>
      WithTypeHandle a b
  | a -> b
  , b -> a
  where
  withTypeHandle :: TypeHandle -> a -> b

instance WithTypeHandle AST.SourcePos (AST.SourcePos, TypeHandle) where
  withTypeHandle = flip (,)

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor =
  InferPreprocessor
    { _variables :: [Map Text TypeHandle]
    , _procedures :: Map Text TypeHandle
    , _typeVariables :: [Map Text TypeHandle]
    , _facts :: Facts
    , _handleCounter :: Int
    , _currentReturn :: Maybe TypeHandle
    }

makeLenses ''InferPreprocessor

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor =
  InferPreprocessor
    { _variables = pure mempty
    , _procedures = mempty
    , _typeVariables = pure mempty
    , _facts = mempty
    , _handleCounter = 0
    , _currentReturn = Nothing
    }

-- returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupVar = uses variables . lookupVarImpl

lookupProc :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupProc = uses procedures . (fromMaybe NoType .) . Map.lookup

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupTVar = uses typeVariables . lookupVarImpl

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHandle
lookupVarImpl name = go
  where
    go (vars:others) =
      name `Map.lookup` vars & \case
        Just handle -> handle
        Nothing -> go others
    go [] = NoType

storeVar :: MonadInferPreprocessor m => Text -> TypeHandle -> m ()
storeVar name handle = variables %= storeVarImpl name handle

beginProc :: MonadInferPreprocessor m => m ()
beginProc = currentReturn <~ Just <$> freshTypeHandle

endProc :: MonadInferPreprocessor m => m TypeHandle
endProc = fromMaybe  NoType <$>
  (currentReturn `exchange` Nothing)

storeProc :: MonadInferPreprocessor m => Text -> TypeHandle -> m ()
storeProc name handle = procedures %= Map.insert name handle

storeReturn :: MonadInferPreprocessor m => TypeHandle -> m ()
storeReturn typeHandle = do
  ret <- fromMaybe NoType <$> use currentReturn
  storeFact $ unifyConstraint ret typeHandle

storeTVar :: MonadInferPreprocessor m => Text -> TypeHandle -> m ()
storeTVar name handle = typeVariables %= storeVarImpl name handle

storeVarImpl :: Ord k => k -> a -> [Map k a] -> [Map k a]
storeVarImpl name handle ~(vars:others) = Map.insert name handle vars : others

pushVars :: MonadInferPreprocessor m => m ()
pushVars = variables %= (mempty :)

pushTVars :: MonadInferPreprocessor m => m ()
pushTVars = typeVariables %= (mempty :)

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
  SimpleType . VarType <$> use handleCounter

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module CMM.Inference.Preprocess.State where

import safe Control.Applicative
import safe Control.Lens.Getter
import safe Control.Lens.Setter
import Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import safe CMM.Inference.Type
import safe CMM.Lens
import safe qualified CMM.Parser.HasPos as AST

class HasTypeHandle a where
  getTypeHandle :: a -> Type

instance HasTypeHandle (AST.SourcePos, Type) where
  getTypeHandle = snd

instance HasTypeHandle a => HasTypeHandle (AST.Annot n a) where
  getTypeHandle = getTypeHandle . AST.takeAnnot

class HasTypeHandle b =>
      WithTypeHandle a b
  | a -> b
  , b -> a
  where
  withTypeHandle :: Type -> a -> b

instance WithTypeHandle AST.SourcePos (AST.SourcePos, Type) where
  withTypeHandle = flip (,)

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor =
  InferPreprocessor
    { _variables :: Map Text Type
    , _procVariables :: Maybe (Map Text Type)
    , _typeVariables :: Map Text Type
    , _procTypeVariables :: Maybe (Map Text Type)
    , _facts :: Facts
    , _handleCounter :: Int
    , _currentReturn :: Type
    }

makeLenses ''InferPreprocessor

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor =
  InferPreprocessor
    { _variables = mempty
    , _procVariables = mempty
    , _typeVariables = mempty
    , _procTypeVariables = mempty
    , _facts = mempty
    , _handleCounter = 0
    , _currentReturn = noCurrentReturn
    }

beginTopLevel :: MonadInferPreprocessor m => Set Text -> Set Text -> m ()
beginTopLevel vars tVars = do
  variables <~ declVars vars
  typeVariables <~ declVars tVars

noCurrentReturn :: Type
noCurrentReturn = ErrorType "No function return registered"

-- returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m Type
lookupVar name = liftA2 (lookupVarImpl name) (use procVariables) (use variables)

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe Type)
lookupProc = uses variables . Map.lookup

lookupTVar :: MonadInferPreprocessor m => Text -> m Type
lookupTVar name =
  liftA2 (lookupVarImpl name) (use procTypeVariables) (use typeVariables)

lookupVarImpl :: Ord k => k -> Maybe (Map k Type) -> Map k Type -> Type
lookupVarImpl name procVars vars =
  fromMaybe NoType $
  (procVars >>= (name `Map.lookup`)) <|> name `Map.lookup` vars

storeVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeVar name handle = do
  vars <- use variables
  tVars <- use procVariables
  storeVarImpl name handle vars tVars

beginProc :: MonadInferPreprocessor m => Set Text -> Set Text -> m ()
beginProc vars tVars = do
  procVariables <~ Just <$> declVars vars
  procTypeVariables <~ Just <$> declVars tVars
  currentReturn <~ freshTypeHandle

declVars :: MonadInferPreprocessor m => Set Text -> m (Map Text Type)
declVars = Map.traverseWithKey (const id) . Map.fromSet (const freshTypeHandle)

endProc :: MonadInferPreprocessor m => m Type
endProc = do
  procVariables .= Nothing
  procTypeVariables .= Nothing
  currentReturn `exchange` noCurrentReturn

storeProc :: MonadInferPreprocessor m => Text -> Type -> m ()
storeProc name handle =
  use variables >>=
  (storeFact . unifyConstraint handle) . lookupVarImpl name Nothing

storeReturn :: MonadInferPreprocessor m => Type -> m ()
storeReturn typeHandle = do
  use currentReturn >>= \ret -> storeFact $ unifyConstraint ret typeHandle

storeTVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeTVar name handle = do
  vars <- use typeVariables
  tVars <- use procTypeVariables
  storeVarImpl name handle vars tVars

storeVarImpl ::
     (Ord k, MonadInferPreprocessor m)
  => k
  -> Type
  -> Map k Type
  -> Maybe (Map k Type)
  -> m ()
storeVarImpl name handle vars procVars =
  storeFact $ handle `unifyConstraint` lookupVarImpl name procVars vars

storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact = (facts %=) . (:)

freshTypeHandle :: MonadInferPreprocessor m => m Type
freshTypeHandle = do
  handleCounter += 1
  SimpleType . VarType . TypeVar <$> use handleCounter

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
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe Data.Function
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import safe CMM.Inference.Type
import safe CMM.Lens
import safe qualified CMM.Parser.HasPos as AST

class HasTypeHandle a where
  getTypeHandle :: a -> TypeVar

instance HasTypeHandle (AST.SourcePos, TypeVar) where
  getTypeHandle = snd

instance HasTypeHandle a => HasTypeHandle (AST.Annot n a) where
  getTypeHandle = getTypeHandle . AST.takeAnnot

class HasTypeHandle b =>
      WithTypeHandle a b
  | a -> b
  , b -> a
  where
  withTypeHandle :: TypeVar -> a -> b

instance WithTypeHandle AST.SourcePos (AST.SourcePos, TypeVar) where
  withTypeHandle = flip (,)

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor =
  InferPreprocessor
    { _variables :: Map Text TypeVar
    , _procVariables :: Maybe (Map Text TypeVar)
    , _typeVariables :: Map Text TypeVar
    , _procTypeVariables :: Maybe (Map Text TypeVar)
    , _facts :: Facts
    , _assumptions :: Facts
    , _handleCounter :: Int
    , _currentReturn :: TypeVar
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
    , _assumptions = mempty
    , _handleCounter = 0
    , _currentReturn = noCurrentReturn
    }

beginTopLevel :: MonadInferPreprocessor m => Map Text TypeKind -> Map Text TypeKind -> m ()
beginTopLevel vars tVars = do
  variables <~ declVars vars
  typeVariables <~ declVars tVars

noCurrentReturn :: TypeVar
noCurrentReturn = NoType

-- returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupVar name = liftA2 (lookupVarImpl name) (use procVariables) (use variables)

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe TypeVar)
lookupProc = uses variables . Map.lookup

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupTVar name =
  liftA2 (lookupVarImpl name) (use procTypeVariables) (use typeVariables)

lookupVarImpl :: Ord k => k -> Maybe (Map k TypeVar) -> Map k TypeVar -> TypeVar
lookupVarImpl name procVars vars =
  fromMaybe NoType $
  (procVars >>= (name `Map.lookup`)) <|> name `Map.lookup` vars

storeVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeVar name handle = do
  vars <- use variables
  tVars <- use procVariables
  storeVarImpl name handle vars tVars

beginProc :: MonadInferPreprocessor m => Map Text TypeKind -> Map Text TypeKind -> m ()
beginProc vars tVars = do
  procVariables <~ Just <$> declVars vars
  procTypeVariables <~ Just <$> declVars tVars
  currentReturn <~ freshTypeHandle Star

declVars :: MonadInferPreprocessor m => Map Text TypeKind -> m (Map Text TypeVar)
declVars = Map.traverseWithKey (&) . (freshNamedHandle <$>)

endProc :: MonadInferPreprocessor m => m TypeVar
endProc = do
  procVariables .= Nothing
  procTypeVariables .= Nothing
  exchange currentReturn noCurrentReturn

storeProc :: MonadInferPreprocessor m => Text -> Type -> m ()
storeProc name handle =
  use variables >>=
  (storeFact . flip typeUnion handle) . lookupVarImpl name Nothing

getCurrentReturn :: MonadInferPreprocessor m => m TypeVar
getCurrentReturn = use currentReturn

storeTVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeTVar name handle = do
  vars <- use typeVariables
  tVars <- use procTypeVariables
  storeVarImpl name handle vars tVars

storeVarImpl ::
     (Ord k, MonadInferPreprocessor m)
  => k
  -> Type
  -> Map k TypeVar
  -> Maybe (Map k TypeVar)
  -> m ()
storeVarImpl name handle vars procVars =
  storeFact $ lookupVarImpl name procVars vars `typeUnion` handle

storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact = (facts %=) . (:)

storeAssump :: MonadInferPreprocessor m => Fact -> m ()
storeAssump = (assumptions %=) . (:)

freshTypeHandle :: MonadInferPreprocessor m => TypeKind -> m TypeVar
freshTypeHandle tKind = do
  handleCounter += 1
  (Nothing &) . (tKind &) . TypeVar  <$> use handleCounter

freshNamedHandle :: MonadInferPreprocessor m => TypeKind -> Text -> m TypeVar
freshNamedHandle tKind name = do
  handleCounter += 1
  (Just name &) . (tKind &) . TypeVar  <$> use handleCounter

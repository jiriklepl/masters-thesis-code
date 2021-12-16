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
import safe Data.Function
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
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
    { _variables :: [Map Text TypeVar]
    , _funcVariables :: Map Text TypeVar
    , _typeVariables :: [Map Text TypeVar]
    , _facts :: [Facts]
    , _cSymbols :: [Text]
    , _handleCounter :: Int
    , _currentReturn :: TypeVar
    }

makeLenses ''InferPreprocessor

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor =
  InferPreprocessor
    { _variables = [mempty]
    , _funcVariables = mempty
    , _typeVariables = [mempty]
    , _facts = [mempty]
    , _cSymbols = mempty
    , _handleCounter = 0
    , _currentReturn = noCurrentReturn
    }

beginUnit ::
     MonadInferPreprocessor m
  => Map Text TypeKind
  -> Map Text TypeKind
  -> Map Text TypeKind
  -> m ()
beginUnit vars fVars tVars = do
  variables <~ pure <$> declVars vars
  funcVariables <~ declVars fVars
  typeVariables <~ pure <$> declVars tVars

noCurrentReturn :: TypeVar
noCurrentReturn = NoType

-- returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupVar name = lookupVarImpl name <$> use variables

lookupFVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupFVar name = lookupVarImpl name <$> uses funcVariables pure

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe TypeVar)
lookupProc = uses variables . (. last) . Map.lookup

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupTVar name = lookupVarImpl name <$> use typeVariables

lookupVarImpl :: Ord k => k -> [Map k TypeVar] -> TypeVar
lookupVarImpl name vars =
  fromMaybe NoType . foldr (<|>) Nothing $ (name `Map.lookup`) <$> vars

storeVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeVar name handle = do
  vars <- use variables
  storeVarImpl name handle vars

beginProc ::
     MonadInferPreprocessor m => Map Text TypeKind -> Map Text TypeKind -> m ()
beginProc vars tVars = do
  vars' <- declVars vars
  variables %= (vars' :)
  tVars' <- declVars tVars
  typeVariables %= (tVars' :)
  facts %= ([] :)
  currentReturn <~ freshTypeHandle Star

declVars ::
     MonadInferPreprocessor m => Map Text TypeKind -> m (Map Text TypeVar)
declVars = Map.traverseWithKey (&) . (freshNamedHandle <$>)

endProc :: MonadInferPreprocessor m => m (Facts, TypeVar)
endProc = do
  variables %= tail
  typeVariables %= tail
  ~(h:t) <- use facts
  facts .= t
  (h, ) <$> exchange currentReturn noCurrentReturn

storeProc :: MonadInferPreprocessor m => Text -> Facts -> Type -> m ()
storeProc name fs t = do
  handle <- uses funcVariables $ lookupVarImpl name . pure
  storeFact $ forall (freeTypeVars fs <> freeTypeVars t) fs handle t

getCurrentReturn :: MonadInferPreprocessor m => m TypeVar
getCurrentReturn = use currentReturn

storeTVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeTVar name handle = do
  vars <- use typeVariables
  storeVarImpl name handle vars

storeVarImpl ::
     (Ord k, MonadInferPreprocessor m) => k -> Type -> [Map k TypeVar] -> m ()
storeVarImpl name handle vars =
  storeFact $ lookupVarImpl name vars `typeUnion` handle

storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact f = do
  ~(h:t) <- use facts
  facts .= (f : h) : t

freshTypeHandle :: MonadInferPreprocessor m => TypeKind -> m TypeVar
freshTypeHandle tKind = do
  handleCounter += 1
  (Nothing &) . (tKind &) . TypeVar <$> use handleCounter

freshNamedHandle :: MonadInferPreprocessor m => TypeKind -> Text -> m TypeVar
freshNamedHandle tKind name = do
  handleCounter += 1
  (Just name &) . (tKind &) . TypeVar <$> use handleCounter

storeCSymbol :: MonadInferPreprocessor m => Text -> m ()
storeCSymbol = (cSymbols %=) . (:)

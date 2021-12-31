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

import safe Control.Applicative (Alternative((<|>)))
import safe Control.Lens.Getter (use, uses)
import safe Control.Lens.Setter ((%=), (+=), (.=), (<~))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Function ((&))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import safe CMM.Inference.Type
import safe CMM.Lens (exchange)
import safe CMM.Parser.HasPos
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
  => Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
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
     (MonadInferPreprocessor m)
  => Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> m ()
beginProc vars tVars = do
  vars' <- declVars vars
  variables %= (vars' :)
  tVars' <- declVars tVars
  typeVariables %= (tVars' :)
  facts %= ([] :)
  currentReturn <~ freshTypeHelper Star

declVars ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> m (Map Text TypeVar)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshTypeHandle kind name pos)

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

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeTVar name handle = do
  vars <- use typeVariables
  storeVarImpl name handle vars

storeVarImpl ::
     (Ord k, MonadInferPreprocessor m) => k -> Type -> [Map k TypeVar] -> m ()
storeVarImpl name handle vars =
  storeFact $ lookupVarImpl name vars `typeUnion` handle

-- | Stores the given `fact` to the state monad
storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact fact = do
  ~(h:t) <- use facts
  facts .= (fact : h) : t

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshTypeHandle ::
     (MonadInferPreprocessor m, HasPos n) => TypeKind -> Text -> n -> m TypeVar
freshTypeHandle tKind name node = do
  handleCounter += 1
  (TVarAST name (getPos node) &) . (tKind &) . TypeVar <$> use handleCounter

freshTypeHelper :: MonadInferPreprocessor m => TypeKind -> m TypeVar
freshTypeHelper tKind = do
  handleCounter += 1
  (NoTVarAnnot &) . (tKind &) . TypeVar <$> use handleCounter

storeCSymbol :: MonadInferPreprocessor m => Text -> m ()
storeCSymbol = (cSymbols %=) . (:)

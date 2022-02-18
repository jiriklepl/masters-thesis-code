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
import safe Control.Lens.Getter (use, uses, (^.))
import safe Control.Lens.Tuple
import safe Control.Lens.Setter ((%=), (+=), (.=), (<~))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Function ((&))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe Data.Maybe (fromMaybe)
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import safe CMM.Inference.Type
import safe CMM.Lens (exchange)
import safe CMM.Parser.HasPos
import safe qualified CMM.Parser.HasPos as AST
import CMM.Data.Tuple (complThd3)

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
    , _funcInstVariables :: Map Text TypeVar
    , _typeConstants :: [Map Text TypeVar]
    , _typeVariables :: [Map Text TypeVar]
    , _typeClasses :: Map Text ClassData
    , _structMembers :: Map Text TypeVar
    , _facts :: [Facts]
    , _cSymbols :: [Text]
    , _currentContext :: [Context]
    , _handleCounter :: Int
    , _currentReturn :: TypeVar
    }

initInferPreprocessor :: InferPreprocessor
initInferPreprocessor =
  InferPreprocessor
    { _variables = [mempty]
    , _funcVariables = mempty
    , _funcInstVariables = mempty
    , _typeConstants = [mempty]
    , _typeVariables = [mempty]
    , _typeClasses = mempty
    , _structMembers = mempty
    , _facts = [mempty]
    , _cSymbols = mempty
    , _currentContext = [GlobalCtx]
    , _handleCounter = 0
    , _currentReturn = noCurrentReturn
    }

data Context
  = GlobalCtx
  | ClassCtx (Text, TypeVar) [(Text, TypeVar)] -- className, classHandle, superClassHandles
  | InstanceCtx (Text, TypeVar) [(Text, TypeVar)] -- className, classHandle, superClassHandles
  -- | SectionCtx Text

data ClassData =
  ClassData
  { _classHandle :: TypeVar
  , _methodDecls :: Set Text
  }

initClassData :: TypeVar -> Set Text -> ClassData
initClassData handle decls =
  ClassData
  { _classHandle = handle
  , _methodDecls = decls
  }

noCurrentReturn :: TypeVar
noCurrentReturn = NoType

makeLenses ''InferPreprocessor
makeLenses ''ClassData

beginUnit ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind, Set Text)
  -> Map Text (SourcePos, TypeKind)
  -> m ()
beginUnit vars fVars fIVars tCons tClasses sMems = do
  variables <~ pure <$> declVars vars
  funcVariables <~ declVars fVars
  funcInstVariables <~ declVars fIVars
  typeConstants <~ pure <$> declVars tCons
  classHandles <- declVars $ complThd3 <$> tClasses
  typeClasses .= Map.intersectionWith ClassData classHandles ((^. _3) <$> tClasses)
  structMembers <~ declVars sMems

-- | returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupVar name = lookupVarImpl name <$> use variables

lookupFVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupFVar name = lookupVarImpl name <$> uses funcVariables pure

lookupFIVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupFIVar name = lookupVarImpl name <$> uses funcInstVariables pure

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe TypeVar)
lookupProc = uses variables . (. last) . Map.lookup

lookupTCon :: MonadInferPreprocessor m => Text -> m TypeVar
lookupTCon name = lookupVarImpl name <$> use typeConstants

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeVar
lookupTVar name = lookupVarImpl name <$> use typeVariables

lookupClass :: MonadInferPreprocessor m => Text -> m TypeVar
lookupClass name = uses typeClasses $ maybe NoType (^. classHandle) . (name `Map.lookup`)

lookupVarImpl :: Ord k => k -> [Map k TypeVar] -> TypeVar
lookupVarImpl name vars =
  fromMaybe NoType . foldr (<|>) Nothing $ (name `Map.lookup`) <$> vars

storeVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeVar name handle = do
  vars <- use variables
  storeVarImpl name handle vars

beginProc ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> m ()
beginProc vars tCons tVars = do
  vars' <- declVars vars
  variables %= (vars' :)
  tCons' <- declVars tCons
  typeConstants %= (tCons' :)
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse
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
  typeConstants %= tail
  typeVariables %= init
  ~(h:t) <- use facts
  facts .= t
  (h, ) <$> exchange currentReturn noCurrentReturn

storeProc :: MonadInferPreprocessor m => Text -> Facts -> Type -> m ()
storeProc name fs t = do
  uses currentContext head >>= \case
    GlobalCtx -> do
      handle <- lookupFVar name
      storeFact $ forall (freeTypeVars fs <> freeTypeVars t) fs handle t
    ClassCtx classHandle' superHandles -> do
      handle <- lookupFVar name
      iHandle <- lookupFIVar name
      iHandle' <- freshTypeHelper Star
      let fICall = iHandle `InstType` iHandle'
      let fIFact = iHandle' `typeUnion` makeFunction (VarType $ snd classHandle') t
      let fs' = fICall : fIFact : (uncurry ClassConstraint <$> (classHandle' : superHandles)) <> fs
      storeFact $ forall (freeTypeVars fs' <> freeTypeVars t) fs' handle t
    InstanceCtx classHandle' superHandles -> do
      handle <- lookupFIVar name
      let t' = makeFunction (VarType $ snd classHandle') t
      let fs' = (uncurry ClassConstraint <$> (classHandle' : superHandles)) <> fs
      storeFact $ forall (freeTypeVars fs' <> freeTypeVars t') fs' handle t'

getCurrentReturn :: MonadInferPreprocessor m => m TypeVar
getCurrentReturn = use currentReturn

pushTypeVariables :: MonadInferPreprocessor m => Map Text (SourcePos, TypeKind) -> m ()
pushTypeVariables tVars = do
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse

pullTypeVariables :: MonadInferPreprocessor m => m ()
pullTypeVariables = do
  typeVariables %= init

pushClass ::
     MonadInferPreprocessor m
  => (Text, TypeVar)
  -> [(Text, TypeVar)]
  -> m ()
pushClass handle supHandles = do
  currentContext %= (ClassCtx handle supHandles :)

pushInstance ::
     MonadInferPreprocessor m
  => (Text, TypeVar)
  -> [(Text, TypeVar)]
  -> m ()
pushInstance handle supHandles = do
  storeFact $ uncurry classFact handle
  currentContext %= (InstanceCtx handle supHandles :)

pullContext :: MonadInferPreprocessor m => m ()
pullContext = do
  currentContext %= tail

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTCon :: MonadInferPreprocessor m => Text -> Type -> m ()
storeTCon name handle = do
  vars <- use typeConstants
  storeVarImpl name handle vars

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

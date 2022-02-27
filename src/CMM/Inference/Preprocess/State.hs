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

-- TODO: reduce the number of "_2 %~ handleId"

import safe Control.Applicative (Alternative((<|>)))
import safe Control.Lens.Getter (use, uses, (^.))
import safe Control.Lens.Tuple
import safe Control.Lens.Setter ((%=), (+=), (.=), (<~), (%~))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Function ((&))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Maybe (fromMaybe)
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import safe CMM.Inference.Type
import safe CMM.Lens (exchange)
import safe CMM.Parser.HasPos
import safe qualified CMM.Parser.HasPos as AST
import CMM.Data.Tuple (complThd3)
import CMM.Inference.TypeHandle hiding (annot)

class HasTypeHandle a where
  getTypeHandle :: a -> TypeHandle

getTypeHandleId :: HasTypeHandle a => a -> TypeVar
getTypeHandleId = handleId . getTypeHandle

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
    , _funcVariables :: Map Text TypeHandle
    , _funcInstVariables :: Map Text TypeHandle
    , _typeConstants :: [Map Text TypeHandle]
    , _typeVariables :: [Map Text TypeHandle]
    , _typeClasses :: Map Text ClassData
    , _structMembers :: Map Text TypeHandle
    , _facts :: [Facts]
    , _cSymbols :: [Text]
    , _currentContext :: [Context]
    , _handleCounter :: Int
    , _currentReturn :: TypeHandle
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
  | ClassCtx (Text, TypeHandle) [(Text, TypeHandle)] -- className, classHandle, superClassHandles
  | InstanceCtx (Text, TypeHandle) [(Text, TypeHandle)] -- className, classHandle, superClassHandles
  -- | SectionCtx Text

data ClassData =
  ClassData
  { _classHandle :: TypeHandle
  , _methodDecls :: Set Text
  }

initClassData :: TypeHandle -> Set Text -> ClassData
initClassData handle decls =
  ClassData
  { _classHandle = handle
  , _methodDecls = decls
  }

noCurrentReturn :: TypeHandle
noCurrentReturn = emptyTypeHandle

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
lookupVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupVar name = lookupVarImpl name <$> use variables

lookupFVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupFVar name = lookupVarImpl name <$> uses funcVariables pure

lookupFIVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupFIVar name = lookupVarImpl name <$> uses funcInstVariables pure

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe TypeHandle)
lookupProc = uses variables . (. last) . Map.lookup

lookupTCon :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupTCon name = lookupVarImpl name <$> use typeConstants

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupTVar name = lookupVarImpl name <$> use typeVariables

lookupClass :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupClass name = uses typeClasses $ maybe emptyTypeHandle (^. classHandle) . (name `Map.lookup`)

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHandle
lookupVarImpl name vars =
  fromMaybe emptyTypeHandle . foldr (<|>) Nothing $ (name `Map.lookup`) <$> vars

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
  pushFacts
  currentReturn <~ freshTypeHelper Star

declVars ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> m (Map Text TypeHandle)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshASTTypeHandle name pos kind)

endProc :: MonadInferPreprocessor m => m (Facts, TypeHandle)
endProc = do
  variables %= tail
  typeConstants %= tail
  typeVariables %= init
  ~(h:t) <- use facts
  facts .= t
  (h, ) <$> exchange currentReturn noCurrentReturn

storeProc :: ToType a => MonadInferPreprocessor m => Text -> Facts -> a -> m ()
storeProc name fs x = do
  let t = toType x
  tVars <- collectTVars
  uses currentContext head >>= \case
    GlobalCtx -> do
      handle <- lookupFVar name
      storeFact $ forall tVars fs (handleId handle `typeUnion` t)
    ClassCtx classHandle' _ -> do
      handle <- lookupFVar name
      iHandle <- lookupFIVar name
      iHandle' <- freshTypeHelper Star
      let fICall = handleId iHandle `instType` handleId iHandle'
      let fIFact = handleId iHandle' `typeUnion` makeFunction [VarType . handleId $ snd classHandle'] t
      let fs' = fICall : fIFact : uncurry classConstraint (classHandle' & _2 %~ handleId) : fs
      storeFact $ forall tVars fs' (handleId handle `classUnion` t)
    InstanceCtx classHandle' superHandles -> do
      handle <- lookupFIVar name
      let t' = makeFunction [VarType . handleId $ snd classHandle'] t
      let fs' = {- TODO: maybe there has to be: (uncurry ClassConstraint <$> superHandles) <> -} fs
      let fs'' = (uncurry classFact . (_2 %~ handleId) <$> superHandles) <> fs
      storeFact $ forall tVars fs' (handleId handle `classUnion` t')
      storeFact $ forall tVars fs'' (handleId handle `instanceUnion` t')

getCurrentReturn :: MonadInferPreprocessor m => m TypeHandle
getCurrentReturn = use currentReturn

pushFacts :: MonadInferPreprocessor m => m ()
pushFacts = do
  facts %= ([] :)

pushTypeVariables :: MonadInferPreprocessor m => Map Text (SourcePos, TypeKind) -> m ()
pushTypeVariables tVars = do
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse

pullTypeVariables :: MonadInferPreprocessor m => m ()
pullTypeVariables = do
  typeVariables %= init

collectTVars :: MonadInferPreprocessor m => m (Set TypeVar)
collectTVars = uses typeVariables (Set.fromList . (handleId <$>) . Map.elems . Map.unions)

pushClass ::
     MonadInferPreprocessor m
  => (Text, TypeHandle)
  -> [(Text, TypeHandle)]
  -> m ()
pushClass handle supHandles = do -- TODO: solve type variables not in scope in superclasses (and in functions somehow)
  tVars <- collectTVars
  storeFact $ forall tVars (uncurry classConstraint . (_2 %~ handleId) <$> supHandles) (uncurry classFact $ handle & _2 %~ handleId)
  currentContext %= (ClassCtx handle supHandles :)

pushInstance ::
     MonadInferPreprocessor m
  => (Text, TypeHandle)
  -> [(Text, TypeHandle)]
  -> m ()
pushInstance handle supHandles = do -- TODO: solve type variables not in scope in superclasses
  ~(fs:facts') <- use facts
  facts .= facts'
  tVars <- collectTVars
  storeFact $ forall tVars ((uncurry classFact . (_2 %~ handleId) <$> supHandles) <> fs) (uncurry classConstraint $ handle & _2 %~ handleId)
  currentContext %= (InstanceCtx handle supHandles :)

pullContext :: MonadInferPreprocessor m => m ()
pullContext = do
  currentContext %= tail

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTCon :: (MonadInferPreprocessor m, ToType a) => Text -> a -> m ()
storeTCon name handle = do
  vars <- use typeConstants
  storeVarImpl name handle vars

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTVar :: (MonadInferPreprocessor m, ToType a) => Text -> a -> m ()
storeTVar name handle = do
  vars <- use typeVariables
  storeVarImpl name handle vars

storeVarImpl ::
     (ToType a, Ord k, MonadInferPreprocessor m) => k -> a -> [Map k TypeHandle] -> m ()
storeVarImpl name handle vars =
  storeFact $ handleId (lookupVarImpl name vars) `typeUnion` handle

-- | Stores the given `fact` to the state monad
storeFact :: MonadInferPreprocessor m => Fact -> m ()
storeFact fact = do
  ~(h:t) <- use facts
  facts .= (fact : h) : t

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshASTTypeHandle ::
     (MonadInferPreprocessor m, HasPos n) => Text -> n -> TypeKind -> m TypeHandle
freshASTTypeHandle name node =
  freshTypeHandle (TypeAST name (getPos node))

freshTypeHelper :: MonadInferPreprocessor m => TypeKind -> m TypeHandle
freshTypeHelper = freshTypeHandle NoTypeAnnot

freshTypeHandle :: MonadInferPreprocessor m => TypeAnnot -> TypeKind -> m TypeHandle
freshTypeHandle annot tKind = do
  handleCounter += 1
  initTypeHandle annot <$> uses handleCounter (`TypeVar` tKind)

storeCSymbol :: MonadInferPreprocessor m => Text -> m ()
storeCSymbol = (cSymbols %=) . (:)

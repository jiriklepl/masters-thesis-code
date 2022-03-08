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
import safe Control.Lens.Getter ((^.), use, uses)
import safe Control.Lens.Setter ((%=), (%~), (+=), (.=), (<~))
import safe Control.Lens.TH (makeLenses)
import safe Control.Lens.Tuple (Field2(_2), Field3(_3))
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Function ((&))
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe qualified CMM.AST.Annot as AST
import CMM.Data.Tuple (complThd3)
import safe CMM.Inference.Type
  ( Fact
  , Facts
  , FlatFact
  , NestedFact(Fact)
  , ToType(..)
  , Type(VarType)
  , TypeAnnot(NoTypeAnnot, TypeAST, TypeNamedAST)
  , TypeKind(Star)
  , TypeVar(TypeVar, tVarId)
  , classConstraint
  , classFact
  , classUnion
  , forall
  , instType
  , instanceUnion
  , makeFunction
  , typeUnion, tupleKind, constExprConstraint
  )
import CMM.Inference.TypeHandle
  ( TypeHandle
  , emptyTypeHandle
  , handleId
  , initTypeHandle
  )
import safe CMM.Parser.HasPos (HasPos(..), SourcePos)
import safe qualified CMM.Parser.HasPos as AST
import CMM.AST.HasName (HasName (getName))

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
    }

data Context
  = GlobalCtx
  | ClassCtx (Text, TypeHandle) [(Text, TypeHandle)] -- className, classHandle, superClassHandles
  | InstanceCtx (Text, TypeHandle) [(Text, TypeHandle)] -- className, classHandle, superClassHandles
  | FunctionCtx (Text, TypeHandle) TypeHandle
  -- | SectionCtx Text

instance HasName Context where
  getName GlobalCtx = undefined -- error
  getName (ClassCtx (name, _) _) = name
  getName (InstanceCtx (name, _) _) = name
  getName (FunctionCtx (name, _) _) = name

instance HasTypeHandle Context where
  getTypeHandle GlobalCtx = emptyTypeHandle
  getTypeHandle (ClassCtx (_, handle) _) = handle
  getTypeHandle (InstanceCtx (_, handle) _) = handle
  getTypeHandle (FunctionCtx (_, handle) _) = handle

data ClassData =
  ClassData
    { _classHandle :: TypeHandle
    , _methodDecls :: Set Text
    }

initClassData :: TypeHandle -> Set Text -> ClassData
initClassData handle decls =
  ClassData {_classHandle = handle, _methodDecls = decls}

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
  typeClasses .=
    Map.intersectionWith ClassData classHandles ((^. _3) <$> tClasses)
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
lookupClass name =
  uses typeClasses $ maybe emptyTypeHandle (^. classHandle) .
  (name `Map.lookup`)

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHandle
lookupVarImpl name vars =
  fromMaybe emptyTypeHandle . foldr (<|>) Nothing $ (name `Map.lookup`) <$> vars

storeVar :: MonadInferPreprocessor m => Text -> Type -> m ()
storeVar name handle = do
  vars <- use variables
  storeVarImpl name handle vars

beginProc ::
     MonadInferPreprocessor m
  => Text
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> m ()
beginProc name vars tCons tVars = do
  vars' <- declVars vars
  variables %= (vars' :)
  tCons' <- declVars tCons
  typeConstants %= (tCons' :)
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse
  pushFacts
  currentReturn <- freshTypeHelper Star
  let returnId = handleId currentReturn
  storeFact $ constExprConstraint returnId
  storeFact $ tVarId returnId `tupleKind` returnId
  handle <- lookupCtxFVar name
  currentContext %= (FunctionCtx (name, handle) currentReturn :)

declVars ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> m (Map Text TypeHandle)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshNamedTypeHandle name pos kind)

endProc :: MonadInferPreprocessor m => m (Facts, TypeHandle)
endProc = do
  variables %= tail
  typeConstants %= tail
  typeVariables %= init
  ~(h:t) <- use facts
  facts .= t
  currentReturn <- getCurrentReturn
  (h, currentReturn) <$ popContext

lookupCtxFVar :: MonadInferPreprocessor m => Text -> m TypeHandle
lookupCtxFVar name =
  uses currentContext head >>= \case
    GlobalCtx -> lookupFVar name
    ClassCtx{} -> lookupFVar name
    InstanceCtx{} -> lookupFIVar name
    FunctionCtx (name', handle) _
      | name == name' -> return handle
      | otherwise -> undefined -- error

getCurrentReturn :: MonadInferPreprocessor m => m TypeHandle
getCurrentReturn = uses currentContext (go . head)
  where
    go = \case
      FunctionCtx _ handle -> handle
      _ -> noCurrentReturn

getCtxName :: MonadInferPreprocessor m => m Text
getCtxName = uses currentContext (getName . head)

getCtxHandle :: MonadInferPreprocessor m => m TypeHandle
getCtxHandle = uses currentContext (getTypeHandle . head)

storeProc ::
     ToType a
  => MonadInferPreprocessor m =>
       Text -> Facts -> a -> m ()
storeProc name fs x = do
  let t = toType x
  tVars <- collectTVars
  uses currentContext head >>= \case
    GlobalCtx -> do
      handle <- lookupFVar name
      storeFact $ forall tVars [handleId handle `typeUnion` t] fs
    ClassCtx classHandle' _ -> do
      handle <- lookupFVar name
      iHandle <- lookupFIVar name
      iHandle' <- freshTypeHelper Star
      let fICall = handleId iHandle `instType` handleId iHandle'
      let fIFact =
            handleId iHandle' `typeUnion`
            makeFunction [VarType . handleId $ snd classHandle'] t
      let fs' =
            Fact fICall : Fact fIFact :
            (Fact . uncurry classConstraint $ classHandle' & _2 %~ handleId) :
            fs
      storeFact $ forall tVars [handleId handle `classUnion` t] fs'
    InstanceCtx classHandle' superHandles -> do
      handle <- lookupFIVar name
      let t' = makeFunction [VarType . handleId $ snd classHandle'] t
      let fs' = fs {- TODO: maybe there has to be: (uncurry ClassConstraint <$> superHandles) <> -}
      let fs'' =
            (Fact . uncurry classFact . (_2 %~ handleId) <$> superHandles) <> fs
      storeFact $ forall tVars [handleId handle `classUnion` t'] fs'
      storeFact $ forall tVars [handleId handle `instanceUnion` t'] fs''
    FunctionCtx {} -> undefined -- error

pushFacts :: MonadInferPreprocessor m => m ()
pushFacts = do
  facts %= ([] :)

pushTypeVariables ::
     MonadInferPreprocessor m => Map Text (SourcePos, TypeKind) -> m ()
pushTypeVariables tVars = do
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse

popTypeVariables :: MonadInferPreprocessor m => m ()
popTypeVariables = do
  typeVariables %= init

collectTVars :: MonadInferPreprocessor m => m (Set TypeVar)
collectTVars =
  uses typeVariables (Set.fromList . (handleId <$>) . Map.elems . Map.unions)

pushClass ::
     MonadInferPreprocessor m
  => (Text, TypeHandle)
  -> [(Text, TypeHandle)]
  -> m ()
pushClass handle supHandles -- TODO: solve type variables not in scope in superclasses (and in functions somehow)
 = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classFact $ handle & _2 %~ handleId]
      (Fact . uncurry classConstraint . (_2 %~ handleId) <$> supHandles)
  currentContext %= (ClassCtx handle supHandles :)

pushInstance ::
     MonadInferPreprocessor m
  => (Text, TypeHandle)
  -> [(Text, TypeHandle)]
  -> m ()
pushInstance handle supHandles -- TODO: solve type variables not in scope in superclasses
 = do
  ~(fs:facts') <- use facts
  facts .= facts'
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classConstraint $ handle & _2 %~ handleId]
      ((Fact . uncurry classFact . (_2 %~ handleId) <$> supHandles) <> fs)
  currentContext %= (InstanceCtx handle supHandles :)

popContext :: MonadInferPreprocessor m => m ()
popContext = do
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
     (ToType a, Ord k, MonadInferPreprocessor m)
  => k
  -> a
  -> [Map k TypeHandle]
  -> m ()
storeVarImpl name handle vars =
  storeFact $ handleId (lookupVarImpl name vars) `typeUnion` handle

-- | Stores the given `fact` to the state monad
class StoreFact a where
  storeFact :: MonadInferPreprocessor m => a -> m ()

instance StoreFact (FlatFact Type) where
  storeFact fact = do
    ~(h:t) <- use facts
    facts .= (Fact fact : h) : t

instance StoreFact Fact where
  storeFact fact = do
    ~(h:t) <- use facts
    facts .= (fact : h) : t

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshNamedTypeHandle ::
     (MonadInferPreprocessor m, HasPos n)
  => Text
  -> n
  -> TypeKind
  -> m TypeHandle
freshNamedTypeHandle name node = freshTypeHandle . TypeNamedAST name $ getPos node

freshASTTypeHandle ::
     (MonadInferPreprocessor m, HasPos n)
  => n
  -> TypeKind
  -> m TypeHandle
freshASTTypeHandle node = freshTypeHandle . TypeAST $ getPos node

freshTypeHelper :: MonadInferPreprocessor m => TypeKind -> m TypeHandle
freshTypeHelper = freshTypeHandle NoTypeAnnot

getHandleCounter :: MonadInferPreprocessor m => m Int
getHandleCounter = use handleCounter

nextHandleCounter :: MonadInferPreprocessor m => m Int
nextHandleCounter = handleCounter += 1 >> getHandleCounter

freshTypeHandle ::
     MonadInferPreprocessor m => TypeAnnot -> TypeKind -> m TypeHandle
freshTypeHandle annot tKind = do
  parent <- handleId <$> getCtxHandle
  initTypeHandle annot . (\int -> TypeVar int tKind parent) <$> nextHandleCounter

storeCSymbol :: MonadInferPreprocessor m => Text -> m ()
storeCSymbol = (cSymbols %=) . (:)

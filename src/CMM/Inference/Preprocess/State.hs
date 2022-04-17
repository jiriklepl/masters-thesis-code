{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Inference.Preprocess.State where

-- TODO: reduce the number of "_2 %~ handleId"
import safe Control.Applicative ((<|>))
import safe Control.Lens.Getter ((^.), use, uses)
import safe Control.Lens.Setter ((%=), (+=), (.=), (<~))
import safe Control.Lens.TH (makeLenses)
import safe Control.Lens.Tuple (_3)
import safe Control.Lens.Type (Lens')
import safe Control.Monad.State.Lazy (MonadState)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromMaybe)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe CMM.AST.HasName (HasName(getName))
import safe CMM.Data.Tuple (complThd3)
import safe CMM.Inference.BuiltIn (getConstType)
import safe CMM.Inference.Fact
  ( Fact
  , Facts
  , FlatFact
  , NestedFact(Fact)
  , classConstraint
  , classFact
  , constExprConstraint
  , forall
  , instType
  , makeApplication
  , makeFunction
  , tupleKind
  , typeUnion
  )
import safe CMM.Inference.Type (ToType(..), Type(ComplType))
import safe CMM.Inference.TypeAnnot
  ( TypeAnnot(NoTypeAnnot, TypeAST, TypeNamedAST)
  )
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , initTypeHandle, handleId
  )
import safe CMM.Inference.TypeKind (TypeKind(Star))
import safe CMM.Inference.TypeVar (TypeVar(TypeVar, tVarId), noType)
import safe CMM.Parser.HasPos (HasPos(..), SourcePos)
import safe CMM.Inference.Preprocess.HasTypeHandle ( getTypeHandleId )
import CMM.Inference.Preprocess.TypeHole
import CMM.Inference.Preprocess.HasTypeHole
import GHC.Stack

type MonadInferPreprocessor = MonadState InferPreprocessor

data InferPreprocessor =
  InferPreprocessor
    { _variables :: [Map Text TypeHandle]
    , _funcVariables :: Map Text TypeHandle
    , _funcInstVariables :: Map Text [TypeHandle]
    , _funcElabVariables :: Map Text TypeHandle
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
    , _funcElabVariables = mempty
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
  | ClassCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | InstanceCtx (Text, TypeHole) (Text, Type) [(Text, Type)] -- className, classHandle, superClassHandles
  | FunctionCtx (Text, TypeHole) TypeHandle
  -- | SectionCtx Text

instance HasName Context where
  getName GlobalCtx = undefined -- error
  getName (ClassCtx (name, _) _ _) = name
  getName (InstanceCtx (name, _) _ _) = name
  getName (FunctionCtx (name, _) _) = name

instance HasTypeHole Context where
  getTypeHole GlobalCtx = EmptyTypeHole
  getTypeHole (FunctionCtx (_, handle) _) = handle
  getTypeHole (ClassCtx (_, handle) _ _) = handle
  getTypeHole (InstanceCtx (_, handle) _ _) = handle

data ClassData =
  ClassData
    { _classHole :: TypeHole
    , _methodDecls :: Set Text
    }

initClassData :: TypeHole -> Set Text -> ClassData
initClassData handle decls =
  ClassData {_classHole = handle, _methodDecls = decls}

noCurrentReturn :: TypeHole
noCurrentReturn = EmptyTypeHole

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
  funcElabVariables <~ declVars fIVars
  typeConstants <~ pure <$> declVars tCons
  classHandles <- declVars $ complThd3 <$> tClasses
  typeClasses .=
    Map.intersectionWith (ClassData . SimpleTypeHole) classHandles ((^. _3) <$> tClasses)
  structMembers <~ declVars sMems

-- | returns `NoType` on failure
lookupVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupVar name = lookupVarImpl name <$> use variables

lookupFVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupFVar name = lookupVarImpl name <$> uses funcVariables pure

lookupFIVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupFIVar name = do
  handle <- freshTypeHelper Star
  funcInstVariables %= Map.adjust (handle :) name
  return $ SimpleTypeHole handle

lookupFEVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupFEVar name = lookupVarImpl name <$> uses funcElabVariables pure

lookupProc :: MonadInferPreprocessor m => Text -> m (Maybe TypeHole)
lookupProc = uses variables . fmap (fmap SimpleTypeHole) .  (. last) . Map.lookup

lookupTCon :: MonadInferPreprocessor m => Text -> m TypeHole
lookupTCon name = lookupVarImpl name <$> use typeConstants

lookupTVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupTVar name = lookupVarImpl name <$> use typeVariables

lookupClass :: MonadInferPreprocessor m => Text -> m TypeHole
lookupClass name =
  uses typeClasses $ maybe EmptyTypeHole (^. classHole) .
  (name `Map.lookup`)

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHole
lookupVarImpl name vars =
  maybe EmptyTypeHole SimpleTypeHole . foldr (<|>) Nothing $ (name `Map.lookup`) <$> vars

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
  let returnId = getTypeHandleId currentReturn
  storeFact $ constExprConstraint returnId
  storeFact $ tVarId returnId `tupleKind` returnId
  handle <- lookupCtxFVar name
  pushContext $ FunctionCtx (name, handle) currentReturn

openProc ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> Map Text (SourcePos, TypeKind)
  -> m ()
openProc vars tCons tVars = do
  vars' <- declVars vars
  modifyHead variables (vars' <>)
  tCons' <- declVars tCons
  modifyHead typeConstants (tCons' <>)
  tVars' <- declVars tVars
  ~(h:t) <- uses typeVariables reverse
  typeVariables .= reverse ((tVars' <> h) : t)

declVars ::
     MonadInferPreprocessor m
  => Map Text (SourcePos, TypeKind)
  -> m (Map Text TypeHandle)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshNamedTypeHandle name pos kind)

endProc :: MonadInferPreprocessor m => m (Facts, TypeHole)
endProc = do
  variables %= tail
  typeConstants %= tail
  typeVariables %= init
  ~(h:t) <- use facts
  facts .= t
  currentReturn <- getCurrentReturn
  (h, currentReturn) <$ popContext

poppedGlobalCtxError :: a
poppedGlobalCtxError =
  error
    "(internal) Inconsistent context; probable cause: popped global context."

lookupCtxFVar :: MonadInferPreprocessor m => Text -> m TypeHole
lookupCtxFVar name = use currentContext >>= go
  where
    go (GlobalCtx:_) = lookupFVar name
    go (ClassCtx {}:_) = lookupFVar name
    go (InstanceCtx {}:_) = lookupFIVar name
    go (FunctionCtx (name', handle) _:others)
      | name == name' = return handle
      | otherwise = go others
    go [] = poppedGlobalCtxError

getCurrentReturn :: MonadInferPreprocessor m => m TypeHole
getCurrentReturn = uses currentContext (go . head)
  where
    go (FunctionCtx _ handle) = SimpleTypeHole handle
    go _ = noCurrentReturn

getCtxName :: MonadInferPreprocessor m => m Text
getCtxName = uses currentContext (getName . head)

getCtxHandle :: (MonadInferPreprocessor m, HasCallStack ) => m (Maybe TypeHandle)
getCtxHandle = uses currentContext (safeHoleHandle . getTypeHole . head)

getCtxClassConstraint :: MonadInferPreprocessor m => m (Text, Type)
getCtxClassConstraint = uses currentContext go
  where
    go (GlobalCtx:_) = (mempty, noType)
    go (ClassCtx _ classConstraint' _:_) = classConstraint'
    go (InstanceCtx _ classConstraint' _:_) = classConstraint'
    go (FunctionCtx {}:others) = go others
    go [] = poppedGlobalCtxError

storeProc ::
     ToType a
  => MonadInferPreprocessor m =>
       Text -> Facts -> a -> m TypeHole
storeProc name fs x = do
  tVars <- collectTVars
  uses currentContext head >>= \case
    GlobalCtx -> do
      hole <- lookupFVar name
      storeFact $ forall tVars [holeId hole `typeUnion` t] fs
      return hole
    ClassCtx _ classConstraint' _ ->
      storeElaboratedProc tVars $
      Fact (uncurry classConstraint classConstraint') :
      fs
    InstanceCtx _ classConstraint' superConstraints ->
      storeElaboratedProc tVars $ Fact (uncurry classFact classConstraint') :
      (Fact . uncurry classFact <$> superConstraints) <>
      fs
    FunctionCtx {} -> error "(internal) Illegal local function encountered." -- TODO: reword this (consolidate)
  where
    t = toType x
    storeElaboratedProc tVars facts' = do
      handle <- holeHandle <$> lookupCtxFVar name
      eHandle <- holeHandle <$> lookupFEVar name
      iHandle <- getTypeHandleId <$> freshTypeHelper Star
      classConstraint' <- getCtxClassConstraint
      let eType =
            makeFunction
              [ toType $ ComplType (getConstType "constraintWitness") `makeApplication`
                snd classConstraint'
              ]
              t
          instFact = Fact $ handleId eHandle `instType` iHandle
          unionFact = Fact $ iHandle `typeUnion` eType
      uses currentContext head >>= \case
        ClassCtx {} -> do
          storeFact $ forall tVars [getTypeHandleId eHandle `typeUnion` eType] facts'
          storeFact $
            forall tVars [handleId handle `typeUnion` t] [instFact, unionFact]
        _ ->
          storeFact . forall tVars [handleId handle `typeUnion` t] $ instFact :
          unionFact :
          facts'
      return $ MethodTypeHole handle eHandle

pushFacts :: MonadInferPreprocessor m => m ()
pushFacts = facts %= ([] :)

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
  uses typeVariables (Set.fromList . (getTypeHandleId <$>) . Map.elems . Map.unions)

pushClass ::
     MonadInferPreprocessor m
  => (Text, TypeHole)
  -> (Text, Type)
  -> [(Text, Type)]
  -> m ()
pushClass handle mainHandle supHandles -- TODO: solve type variables not in scope in superclasses (and in functions somehow)
 = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classConstraint mainHandle]
      (Fact . uncurry classFact <$> supHandles)
  pushContext $ ClassCtx handle mainHandle supHandles

pushInstance ::
     MonadInferPreprocessor m
  => (Text, TypeHole)
  -> (Text, Type)
  -> [(Text, Type)]
  -> m ()
pushInstance handle mainHandle supHandles -- TODO: solve type variables not in scope in superclasses
 = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classFact mainHandle]
      (Fact . uncurry classFact <$> supHandles)
  pushContext $ InstanceCtx handle mainHandle supHandles

popContext :: MonadInferPreprocessor m => m ()
popContext = currentContext %= tail

pushContext :: MonadInferPreprocessor m => Context -> m ()
pushContext = (currentContext %=) . (:)

popTopContext :: MonadInferPreprocessor m => m Context
popTopContext = do
  ~(h:t) <- use currentContext
  currentContext .= t
  return h

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTCon :: (MonadInferPreprocessor m, ToType a) => Text -> a -> m ()
storeTCon name handle = use typeConstants >>= storeVarImpl name handle

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTVar :: (MonadInferPreprocessor m, ToType a) => Text -> a -> m ()
storeTVar name handle = use typeVariables >>= storeVarImpl name handle

storeVarImpl ::
     (ToType a, Ord k, MonadInferPreprocessor m)
  => k
  -> a
  -> [Map k TypeHandle]
  -> m ()
storeVarImpl name handle vars =
  storeFact $ holeId (lookupVarImpl name vars) `typeUnion` handle

-- | Stores the given `fact` to the state monad
class StoreFact a where
  storeFact :: MonadInferPreprocessor m => a -> m ()

instance StoreFact (FlatFact Type) where
  storeFact fact = pushToHead facts $ Fact fact

instance StoreFact Fact where
  storeFact = pushToHead facts

pushToHead :: MonadState s m => Lens' s [[a]] -> a -> m ()
pushToHead place = modifyHead place . (:)

modifyHead :: MonadState s m => Lens' s [a] -> (a -> a) -> m ()
modifyHead place action = do
  ~(h:t) <- use place
  place .= action h : t

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshNamedTypeHandle ::
     (MonadInferPreprocessor m, HasPos n)
  => Text
  -> n
  -> TypeKind
  -> m TypeHandle
freshNamedTypeHandle name node =
  freshTypeHandle . TypeNamedAST name $ getPos node

freshASTTypeHandle ::
     (MonadInferPreprocessor m, HasPos n) => n -> TypeKind -> m TypeHandle
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
  parent <- fmap handleId <$> getCtxHandle
  initTypeHandle annot . (\int -> TypeVar int tKind $ fromMaybe noType parent) <$>
    nextHandleCounter

storeCSymbol :: MonadInferPreprocessor m => Text -> m ()
storeCSymbol = (cSymbols %=) . (:)

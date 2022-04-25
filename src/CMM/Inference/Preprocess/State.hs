{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

-- TODO: reduce the number of "_2 %~ handleId"
module CMM.Inference.Preprocess.State
  ( module CMM.Inference.Preprocess.State
  , module CMM.Inference.Preprocess.State.Impl
  ) where

import safe Control.Applicative (Applicative(pure), (<|>))
import safe Control.Lens.Getter ((^.), use, uses)
import safe Control.Lens.Setter ((%=), (.=), (<~))
import safe Control.Lens.Tuple (_3)
import safe Control.Lens.Type (Lens')
import safe Control.Monad (Functor((<$), fmap), Monad((>>=), return))
import safe Control.Monad.State.Lazy (MonadState, State)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Function (($), (.))
import safe Data.Functor ((<$>))
import safe Data.List (foldr, head, init, last, reverse, tail)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import safe Data.Monoid (Monoid(mempty), (<>))
import safe Data.Ord (Ord)
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Tuple (snd, uncurry)
import safe GHC.Err (error)

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
import safe CMM.Inference.Preprocess.ClassData (ClassData(ClassData), classHole)
import safe CMM.Inference.Preprocess.Context
  ( Context(ClassCtx, FunctionCtx, GlobalCtx, InstanceCtx)
  )
import safe CMM.Inference.Preprocess.HasTypeHandle (getTypeHandleId)
import safe CMM.Inference.Preprocess.HasTypeHole (HasTypeHole(getTypeHole))
import safe CMM.Inference.Preprocess.TypeHole
  ( TypeHole(EmptyTypeHole, MethodTypeHole, SimpleTypeHole)
  , holeHandle
  , holeId
  , safeHoleHandle
  )
import safe CMM.Inference.Type (ToType(toType), Type(ComplType))
import safe CMM.Inference.TypeAnnot
  ( TypeAnnot(NoTypeAnnot, TypeAST, TypeNamedAST)
  )
import safe CMM.Inference.TypeHandle (TypeHandle, handleId, initTypeHandle)
import safe CMM.Inference.TypeKind (TypeKind(Star))
import safe CMM.Inference.TypeVar (TypeVar(TypeVar, tVarId), noType)
import safe CMM.Parser.HasPos (HasPos, SourcePos, getPos)

import safe CMM.AST.Variables.State (CollectorState)
import safe qualified CMM.AST.Variables.State as CS
import safe CMM.Inference.HandleCounter (nextHandleCounter)
import safe CMM.Inference.Preprocess.State.Impl
  ( PreprocessorState(PreprocessorState)
  , cSymbols
  , currentContext
  , facts
  , funcElabVariables
  , funcInstVariables
  , funcVariables
  , initPreprocessor
  , structMembers
  , typeClasses
  , typeConstants
  , typeVariables
  , variables
  )

type Preprocessor = State PreprocessorState

noCurrentReturn :: TypeHole
noCurrentReturn = EmptyTypeHole

beginUnit :: CollectorState -> Preprocessor ()
beginUnit collector = do
  variables <~ pure <$> declVars (collector ^. CS.variables)
  funcVariables <~ declVars (collector ^. CS.funcVariables)
  funcElabVariables <~ declVars (collector ^. CS.funcInstVariables)
  typeConstants <~ pure <$> declVars (collector ^. CS.typeConstants)
  classHandles <- declVars $ complThd3 <$> (collector ^. CS.typeClasses)
  typeClasses .=
    Map.intersectionWith
      (ClassData . SimpleTypeHole)
      classHandles
      ((^. _3) <$> (collector ^. CS.typeClasses))
  structMembers <~ declVars (collector ^. CS.structMembers)

-- | returns `NoType` on failure
lookupVar :: Text -> Preprocessor TypeHole
lookupVar name = lookupVarImpl name <$> use variables

lookupFVar :: Text -> Preprocessor TypeHole
lookupFVar name = lookupVarImpl name <$> uses funcVariables pure

lookupFIVar :: Text -> Preprocessor TypeHole
lookupFIVar name = do
  handle <- freshTypeHelper Star
  funcInstVariables %= Map.adjust (handle :) name
  return $ SimpleTypeHole handle

lookupFEVar :: Text -> Preprocessor TypeHole
lookupFEVar name = lookupVarImpl name <$> uses funcElabVariables pure

lookupProc :: Text -> Preprocessor (Maybe TypeHole)
lookupProc = uses variables . fmap (fmap SimpleTypeHole) . (. last) . Map.lookup

lookupTCon :: Text -> Preprocessor TypeHole
lookupTCon name = lookupVarImpl name <$> use typeConstants

lookupTVar :: Text -> Preprocessor TypeHole
lookupTVar name = lookupVarImpl name <$> use typeVariables

lookupClass :: Text -> Preprocessor TypeHole
lookupClass name =
  uses typeClasses $ maybe EmptyTypeHole (^. classHole) . (name `Map.lookup`)

lookupVarImpl :: Ord k => k -> [Map k TypeHandle] -> TypeHole
lookupVarImpl name vars =
  maybe EmptyTypeHole SimpleTypeHole . foldr (<|>) Nothing $ (name `Map.lookup`) <$>
  vars

storeVar :: Text -> Type -> Preprocessor ()
storeVar name handle = do
  vars <- use variables
  storeVarImpl name handle vars

beginProc :: Text -> CollectorState -> Preprocessor ()
beginProc name collector = do
  vars' <- declVars $ collector ^. CS.variables
  variables %= (vars' :)
  tCons' <- declVars $ collector ^. CS.typeConstants
  typeConstants %= (tCons' :)
  tVars' <- declVars $ collector ^. CS.typeVariables
  typeVariables %= reverse . (tVars' :) . reverse
  pushFacts
  currentReturn <- freshTypeHelper Star
  let returnId = getTypeHandleId currentReturn
  storeFact $ constExprConstraint returnId
  storeFact $ tVarId returnId `tupleKind` returnId
  handle <- lookupCtxFVar name
  pushContext $ FunctionCtx (name, handle) currentReturn

openProc :: CollectorState -> Preprocessor ()
openProc collector = do
  vars' <- declVars $ collector ^. CS.variables
  modifyHead variables (vars' <>)
  tCons' <- declVars $ collector ^. CS.typeConstants
  modifyHead typeConstants (tCons' <>)
  tVars' <- declVars $ collector ^. CS.typeVariables
  ~(h:t) <- uses typeVariables reverse
  typeVariables .= reverse ((tVars' <> h) : t)

declVars :: Map Text (SourcePos, TypeKind) -> Preprocessor (Map Text TypeHandle)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshNamedTypeHandle name pos kind)

endProc :: Preprocessor (Facts, TypeHole)
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

lookupCtxFVar :: Text -> Preprocessor TypeHole
lookupCtxFVar name = use currentContext >>= go
  where
    go (GlobalCtx:_) = lookupFVar name
    go (ClassCtx {}:_) = lookupFVar name
    go (InstanceCtx {}:_) = lookupFIVar name
    go (FunctionCtx (name', handle) _:others)
      | name == name' = return handle
      | otherwise = go others
    go [] = poppedGlobalCtxError

getCurrentReturn :: Preprocessor TypeHole
getCurrentReturn = uses currentContext (go . head)
  where
    go (FunctionCtx _ handle) = SimpleTypeHole handle
    go _ = noCurrentReturn

getCtxName :: Preprocessor Text
getCtxName = uses currentContext (getName . head)

getCtxHandle :: Preprocessor (Maybe TypeHandle)
getCtxHandle = uses currentContext (safeHoleHandle . getTypeHole . head)

getCtxClassConstraint :: Preprocessor (Text, Type)
getCtxClassConstraint = uses currentContext go
  where
    go (GlobalCtx:_) = (mempty, noType)
    go (ClassCtx _ classConstraint' _:_) = classConstraint'
    go (InstanceCtx _ classConstraint' _:_) = classConstraint'
    go (FunctionCtx {}:others) = go others
    go [] = poppedGlobalCtxError

storeProc :: ToType a => Text -> Facts -> a -> Preprocessor TypeHole
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
      fHandle <- holeHandle <$> lookupFVar name
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
          storeFact $
            forall tVars [getTypeHandleId eHandle `typeUnion` eType] facts'
          storeFact $
            forall tVars [handleId handle `typeUnion` t] [instFact, unionFact]
          return $ SimpleTypeHole handle
        _ -> do
          storeFact . forall tVars [handleId handle `typeUnion` t] $ instFact :
            unionFact :
            facts'
          return $ MethodTypeHole handle fHandle eHandle

pushFacts :: Preprocessor ()
pushFacts = facts %= ([] :)

pushTypeVariables :: Map Text (SourcePos, TypeKind) -> Preprocessor ()
pushTypeVariables tVars = do
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse

popTypeVariables :: Preprocessor ()
popTypeVariables = do
  typeVariables %= init

collectTVars :: Preprocessor (Set TypeVar)
collectTVars =
  uses
    typeVariables
    (Set.fromList . (getTypeHandleId <$>) . Map.elems . Map.unions)

pushClass ::
     (Text, TypeHole) -> (Text, Type) -> [(Text, Type)] -> Preprocessor ()
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
     (Text, TypeHole) -> (Text, Type) -> [(Text, Type)] -> Preprocessor ()
pushInstance handle mainHandle supHandles -- TODO: solve type variables not in scope in superclasses
 = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classFact mainHandle]
      (Fact . uncurry classFact <$> supHandles)
  pushContext $ InstanceCtx handle mainHandle supHandles

popContext :: Preprocessor ()
popContext = currentContext %= tail

pushContext :: Context -> Preprocessor ()
pushContext = (currentContext %=) . (:)

popTopContext :: Preprocessor Context
popTopContext = do
  ~(h:t) <- use currentContext
  currentContext .= t
  return h

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTCon :: ToType a => Text -> a -> Preprocessor ()
storeTCon name handle = use typeConstants >>= storeVarImpl name handle

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTVar :: ToType a => Text -> a -> Preprocessor ()
storeTVar name handle = use typeVariables >>= storeVarImpl name handle

storeVarImpl ::
     (ToType a, Ord k) => k -> a -> [Map k TypeHandle] -> Preprocessor ()
storeVarImpl name handle vars =
  storeFact $ holeId (lookupVarImpl name vars) `typeUnion` handle

-- | Stores the given `fact` to the state monad
class StoreFact a where
  storeFact :: a -> Preprocessor ()

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
     HasPos n => Text -> n -> TypeKind -> Preprocessor TypeHandle
freshNamedTypeHandle name node =
  freshTypeHandle . TypeNamedAST name $ getPos node

freshASTTypeHandle :: HasPos n => n -> TypeKind -> Preprocessor TypeHandle
freshASTTypeHandle node = freshTypeHandle . TypeAST $ getPos node

freshTypeHelper :: TypeKind -> Preprocessor TypeHandle
freshTypeHelper = freshTypeHandle NoTypeAnnot

freshTypeHandle :: TypeAnnot -> TypeKind -> Preprocessor TypeHandle
freshTypeHandle annot tKind = do
  parent <- fmap handleId <$> getCtxHandle
  initTypeHandle annot . (\int -> TypeVar int tKind $ fromMaybe noType parent) <$>
    nextHandleCounter

storeCSymbol :: Text -> Preprocessor ()
storeCSymbol = (cSymbols %=) . (:)

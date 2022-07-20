{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}

module CMM.Inference.Preprocess.State
  ( module CMM.Inference.Preprocess.State
  , module CMM.Inference.Preprocess.State.Impl
  ) where

import safe Control.Applicative ((<|>))
import safe Control.Lens (Lens', (%=), (.=), (<>=), (<~), (^.), _3, use, uses)
import safe Control.Monad.State (MonadState)
import safe Data.Bifunctor (Bifunctor(bimap, second))
import safe Data.Foldable (for_, traverse_)
import safe Data.Generics.Aliases (extT)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)

import safe CMM.AST (Conv)
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.AST.Variables.State (CollectorState)
import safe qualified CMM.AST.Variables.State as CS
import safe qualified CMM.Data.Trilean as T
import safe CMM.Data.Tuple (complThd3)
import safe CMM.Inference.BuiltIn
  ( addressKind
  , constraintWitnessName
  , getConstType
  , getDataKind
  )
import safe CMM.Inference.Fact
  ( Fact
  , Facts
  , FlatFact
  , NestedFact(Fact)
  , classConstraint
  , classFact
  , classFunDeps
  , constExprConstraint
  , forall
  , instType
  , kindConstraint
  , lockFact
  , regularExprConstraint
  , subConst
  , typeEquality
  , unstorableConstraint
  )
import safe CMM.Inference.GetParent (makeAdoption)
import safe CMM.Inference.Preprocess.ClassData (ClassData(ClassData), classHole)
import safe CMM.Inference.Preprocess.Context
  ( Context(ClassCtx, FunctionCtx, GlobalCtx, InstanceCtx, StructCtx,
        ctxConstraint, ctxFunctionHandle, ctxSupers)
  )
import safe CMM.Inference.Preprocess.Elaboration
  ( Elaboration(EmptyElaboration, MethodElaboration, SimpleElaboration)
  , eHandle
  )
import safe CMM.Inference.Properties (Properties, propsId)
import safe CMM.Inference.Refresh (Refresh(refresh))
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type (ToType(toType), Type(ComplType))
import safe CMM.Inference.TypeCompl
  ( TypeCompl(ConstType)
  , makeApplication
  , makeFunction
  )
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind)
  , TypeKind((:->), Constraint, Star)
  )
import safe CMM.Inference.TypeVar
  ( ToTypeVar(toTypeVar)
  , TypeVar(NoType)
  , noType
  )
import safe CMM.Inference.Utils (fieldClassHelper)
import safe CMM.Parser.GetPos (SourcePos)

import CMM.Err.State (HasErrorState(errorState))
import safe CMM.Inference.Preprocess.State.Impl
  ( Preprocessor
  , PreprocessorState(PreprocessorState)
  , cSymbols
  , currentContext
  , currentParent
  , facts
  , freshGeneric
  , freshStar
  , freshTypeHelper
  , funcElabVariables
  , funcInstVariables
  , funcVariables
  , initPreprocessor
  , structInstMembers
  , structMembers
  , typeAliases
  , typeClasses
  , typeConstants
  , typeVariables
  , variables
  )

noCurrentReturn :: Elaboration
noCurrentReturn = EmptyElaboration

beginUnit :: CollectorState -> Preprocessor ()
beginUnit collector = do
  variables <~ pure <$> declVars (collector ^. CS.variables)
  funcVariables <~ declVars (collector ^. CS.funcVariables)
  funcElabVariables <~ declVars (collector ^. CS.funcInstVariables)
  tCons <- declVars $ collector ^. CS.typeConstants
  untrivialize tCons
  typeConstants .= pure tCons
  classHandles <- declVars $ complThd3 <$> (collector ^. CS.typeClasses)
  untrivialize classHandles
  typeClasses .=
    Map.intersectionWith
      (ClassData . SimpleElaboration)
      classHandles
      ((^. _3) <$> (collector ^. CS.typeClasses))
  memberClasses <-
    declVars . Map.fromAscList $
    bimap fieldClassHelper (second $ \_ -> Star :-> Star :-> Constraint) <$>
    Map.toAscList members
  let memberClassData =
        (`ClassData` mempty) . SimpleElaboration <$> memberClasses
  typeClasses %= (`Map.union` memberClassData)
  Map.toList memberClasses `for_` \(name, props) ->
    storeFacts [classFunDeps name [[T.False, T.True]], lockFact $ toType props]
  mems <- declVars members
  structMembers .= mems
  mems `for_` \mem -> do
    a <- freshTypeHelper Star
    b <- freshTypeHelper Star
    let t = makeFunction [a] b
    storeFact $
      forall
        (Set.fromList $ toTypeVar <$> [a, b])
        [mem `typeEquality` t]
        [ Fact $ addressKind `kindConstraint` a
        , Fact $ addressKind `kindConstraint` b
        , Fact $ b `subConst` a
        ]
  where
    members = collector ^. CS.structMembers

-- | returns `NoType` on failure
lookupVar :: GetName n => n -> Preprocessor Elaboration
lookupVar named = lookupVarImpl named <$> use variables

lookupFVar :: GetName n => n -> Preprocessor Elaboration
lookupFVar named = lookupVarImpl named <$> uses funcVariables pure

lookupSMem :: GetName n => n -> Preprocessor Elaboration
lookupSMem named = lookupVarImpl named <$> uses structMembers pure

lookupSIMem :: GetName n => n -> Preprocessor Elaboration
lookupSIMem named = do
  props <- freshTypeHelper Star
  structInstMembers %= Map.adjust (props :) (getName named)
  return $ SimpleElaboration props

lookupFIVar :: GetName n => n -> Preprocessor Elaboration
lookupFIVar named = do
  props <- freshTypeHelper Star
  funcInstVariables %= Map.adjust (props :) (getName named)
  return $ SimpleElaboration props

lookupFEVar :: GetName n => n -> Preprocessor Elaboration
lookupFEVar named = lookupVarImpl named <$> uses funcElabVariables pure

lookupProc :: GetName n => n -> Preprocessor (Maybe Elaboration)
lookupProc =
  uses variables . fmap (fmap SimpleElaboration) . (. last) . Map.lookup .
  getName

lookupTCon :: GetName n => n -> Preprocessor Elaboration
lookupTCon named = lookupVarImpl named <$> use typeConstants

lookupTVar :: GetName n => n -> Preprocessor Elaboration
lookupTVar named = lookupVarImpl named <$> use typeVariables

lookupClass :: GetName n => n -> Preprocessor Elaboration
lookupClass named =
  uses typeClasses $ maybe EmptyElaboration (^. classHole) .
  (getName named `Map.lookup`)

lookupVarImpl :: GetName n => n -> [Map Text Properties] -> Elaboration
lookupVarImpl named vars =
  maybe EmptyElaboration SimpleElaboration . foldr (<|>) Nothing $
  (getName named `Map.lookup`) <$>
  vars

storeVar :: Text -> Type -> Preprocessor ()
storeVar name props = do
  vars <- use variables
  storeVarImpl name props vars

untrivialize :: Map Text Properties -> Preprocessor ()
untrivialize tCons =
  Map.toList tCons `for_` \(name', props) ->
    storeFacts
      [ props `typeEquality`
        ComplType (ConstType name' (getTypeKind $ propsId props) NoType)
      , getDataKind "" `kindConstraint` props
      , lockFact props
      ]

beginProc ::
     Text -> Elaboration -> CollectorState -> Maybe Conv -> Preprocessor ()
beginProc name hole collector mConv = do
  vars <- declVars $ collector ^. CS.variables
  variables %= (vars :)
  tAliases <- declVars $ collector ^. CS.typeAliases
  typeAliases %= (tAliases :)
  tVars <- declVars $ collector ^. CS.typeVariables
  errorState <>= collector ^. errorState
  typeVariables %= reverse . (tVars :) . reverse
  pushFacts
  currentReturn <- freshTypeHelper Star
  storeFacts
    [constExprConstraint currentReturn, unstorableConstraint currentReturn]
  pushContext $ FunctionCtx name hole currentReturn mConv

openProc :: CollectorState -> Preprocessor ()
openProc collector = do
  vars' <- declVars $ collector ^. CS.variables
  modifyHead variables (vars' <>)
  tVars' <- declVars $ collector ^. CS.typeVariables
  errorState <>= collector ^. errorState
  ~(h:t) <- uses typeVariables reverse
  typeVariables .= reverse ((tVars' <> h) : t)

declVars :: Map Text (SourcePos, TypeKind) -> Preprocessor (Map Text Properties)
declVars = traverse $ freshTypeHelper . snd

endProc :: Preprocessor (Facts, Elaboration)
endProc = do
  variables %= tail
  typeVariables %= init
  h <- popTopFacts
  currentReturn <- getCurrentReturn
  (h, currentReturn) <$ popContext

poppedGlobalCtxError :: a
poppedGlobalCtxError =
  error
    "(internal) Inconsistent context; probable cause: popped global context."

lookupCtxFVar :: Text -> Preprocessor Elaboration
lookupCtxFVar name = use currentContext >>= go
  where
    go =
      \case
        ClassCtx {}:_ -> lookupFVar name
        FunctionCtx name' props _ _:others
          | name == name' -> return props
          | otherwise -> go others
        GlobalCtx:_ -> lookupFVar name
        InstanceCtx {}:_ -> lookupFIVar name
        StructCtx {}:_ -> lookupFVar name
        [] -> poppedGlobalCtxError

getCurrentReturn :: Preprocessor Elaboration
getCurrentReturn = uses currentContext $ go . head
  where
    go =
      \case
        FunctionCtx {ctxFunctionHandle} -> SimpleElaboration ctxFunctionHandle
        _ -> noCurrentReturn

getCtxName :: Preprocessor Text
getCtxName = uses currentContext $ getName . head

getCtxMConv :: Preprocessor (Maybe Conv)
getCtxMConv = uses currentContext $ getConv . head

getCtxClassConstraint :: Preprocessor (Text, Type)
getCtxClassConstraint = uses currentContext go
  where
    go =
      \case
        GlobalCtx:_ -> (mempty, noType)
        ClassCtx {ctxConstraint}:_ -> ctxConstraint
        InstanceCtx {ctxConstraint}:_ -> ctxConstraint
        FunctionCtx {}:others -> go others
        StructCtx {}:others -> go others
        [] -> poppedGlobalCtxError

storeProc :: ToType a => Text -> Facts -> a -> Preprocessor Elaboration
storeProc name fs x = do
  tVars <- collectTVars
  subst <- refresh tVars
  let tVars' = apply subst `Set.map` tVars
  uses currentContext head >>= \case
    GlobalCtx -> do
      hole <- lookupFVar name
      storeFact . apply subst $ forall tVars' [hole `typeEquality` t] fs
      return hole
    ClassCtx {ctxConstraint} ->
      storeElaboratedProc subst tVars' $
      Fact (uncurry classConstraint ctxConstraint) :
      fs
    InstanceCtx {ctxConstraint, ctxSupers} ->
      storeElaboratedProc subst tVars' $ Fact (uncurry classFact ctxConstraint) :
      (Fact . uncurry classFact <$> ctxSupers) <>
      fs
    FunctionCtx {} -> goIllegal
    StructCtx {} -> goIllegal
  where
    goIllegal = error "(internal) Illegal local function encountered." -- NOTE: not possible within the bounds of the syntax
    t = toType x
    storeElaboratedProc subst tVars facts' = do
      props <- eHandle <$> lookupCtxFVar name
      fHandle <- eHandle <$> lookupFVar name
      eHandle <- eHandle <$> lookupFEVar name
      iHandle <- freshTypeHelper Star
      classConstraint' <- getCtxClassConstraint
      let eType =
            makeFunction
              [ toType $ ComplType (getConstType constraintWitnessName) `makeApplication`
                snd classConstraint'
              ]
              t
          instFact = Fact $ eHandle `instType` iHandle
          unionFact = Fact $ iHandle `typeEquality` eType
      uses currentContext head >>= \case
        ClassCtx {} -> do
          adoption :: TypeVar -> TypeVar <- makeAdoption eHandle
          storeFact . extT id adoption . apply subst $
            forall tVars [eHandle `typeEquality` eType] facts'
          storeFact . extT id adoption . apply subst $
            forall tVars [props `typeEquality` t] [instFact, unionFact]
          return . extT id adoption $ SimpleElaboration props
        _ -> do
          storeFact . apply subst . forall tVars [props `typeEquality` t] $
            instFact :
            unionFact :
            facts'
          return $ MethodElaboration props fHandle eHandle

pushParent :: TypeVar -> Preprocessor ()
pushParent parent = currentParent %= (parent :)

popParent :: Preprocessor ()
popParent = currentParent %= tail

pushFacts :: Preprocessor ()
pushFacts = facts %= ([] :)

pushTypeVariables :: CollectorState -> Preprocessor ()
pushTypeVariables collector = do
  tVars' <- declVars $ collector ^. CS.typeVariables
  errorState <>= collector ^. errorState
  typeVariables %= reverse . (tVars' :) . reverse

popTypeVariables :: Preprocessor ()
popTypeVariables = do
  typeVariables %= init

collectTVars :: Preprocessor (Set TypeVar)
collectTVars =
  uses typeVariables (Set.fromList . (propsId <$>) . Map.elems . Map.unions)

pushStruct :: (Text, Elaboration) -> (Text, Type) -> Preprocessor ()
pushStruct (name, hole) constraint@(_, t) = do
  tVars <- collectTVars
  pushContext $ StructCtx name hole constraint
  storeFact $
    forall tVars [hole `typeEquality` t] [Fact $ regularExprConstraint t]

pushClass ::
     Text -> Elaboration -> (Text, Type) -> [(Text, Type)] -> Preprocessor ()
pushClass name hole constraint supers = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classConstraint constraint]
      (Fact . uncurry classFact <$> supers)
  pushContext $ ClassCtx name hole constraint supers

pushInstance ::
     Text -> Elaboration -> (Text, Type) -> [(Text, Type)] -> Preprocessor ()
pushInstance name hole constraint supers = do
  tVars <- collectTVars
  storeFact $
    forall
      tVars
      [uncurry classFact constraint]
      (Fact . uncurry classFact <$> supers)
  pushContext $ InstanceCtx name hole constraint supers

popContext :: Preprocessor ()
popContext = currentContext %= tail

pushContext :: Context -> Preprocessor ()
pushContext = (currentContext %=) . (:)

popTopContext :: Preprocessor Context
popTopContext = popTop currentContext

popTopFacts :: Preprocessor Facts
popTopFacts = popTop facts

popTop :: Lens' PreprocessorState [t] -> Preprocessor t
popTop from = do
  ~(h:t) <- use from
  from .= t
  return h

-- | Stores the given type variable properties `props` under the given `name` to the state monad
storeTCon :: ToType a => Text -> a -> Preprocessor ()
storeTCon name props = use typeConstants >>= storeVarImpl name props

-- | Stores the given type variable properties `props` under the given `name` to the state monad
storeTVar :: ToType a => Text -> a -> Preprocessor ()
storeTVar name props = use typeVariables >>= storeVarImpl name props

storeVarImpl ::
     (ToType a, GetName n) => n -> a -> [Map Text Properties] -> Preprocessor ()
storeVarImpl named props vars =
  storeFact $ lookupVarImpl named vars `typeEquality` props

-- | Stores the given `fact` to the state monad
class StoreFact a where
  storeFact :: a -> Preprocessor ()

instance ToType a => StoreFact (FlatFact a) where
  storeFact fact = pushToHead facts . Fact $ toType <$> fact

instance StoreFact Fact where
  storeFact = pushToHead facts

storeFacts :: (Foldable f, StoreFact t) => f t -> Preprocessor ()
storeFacts = traverse_ storeFact

pushToHead :: MonadState s m => Lens' s [[a]] -> a -> m ()
pushToHead place = modifyHead place . (:)

modifyHead :: MonadState s m => Lens' s [a] -> (a -> a) -> m ()
modifyHead place action = do
  ~(h:t) <- use place
  place .= action h : t

storeCSymbol :: Text -> Preprocessor ()
storeCSymbol = (cSymbols %=) . (:)

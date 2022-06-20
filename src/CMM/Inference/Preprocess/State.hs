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
import safe Control.Monad.State (MonadState)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Foldable (Foldable(foldr), for_, traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor ((<$>))
import safe Data.List (head, init, last, reverse, tail)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Nothing), maybe)
import safe Data.Monoid (Monoid(mempty), (<>))
import safe Data.Set (Set)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Tuple (snd, uncurry)
import safe Data.Bifunctor ( Bifunctor(bimap, second) )

import safe GHC.Err (error)

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Data.Tuple (complThd3)
import safe CMM.Inference.BuiltIn (constraintWitness, getConstType, getDataKind, addressKind)
import safe CMM.Inference.Fact
  ( Fact
  , Facts
  , FlatFact (ClassFunDeps)
  , NestedFact(Fact)
  , constExprConstraint
  , forall
  , instType
  , tupleKind
  , typeUnion, kindConstraint, subConst, classFact, classConstraint, minKindConstraint, maxKindConstraint, regularExprConstraint
  )
import safe CMM.Inference.Preprocess.ClassData (ClassData(ClassData), classHole)
import safe CMM.Inference.Preprocess.Context
  ( Context(ClassCtx, FunctionCtx, GlobalCtx, InstanceCtx, StructCtx)
  )
import safe CMM.Inference.Preprocess.TypeHole
  ( TypeHole(EmptyTypeHole, MethodTypeHole, SimpleTypeHole)
  , holeHandle

  )
import safe CMM.Inference.Type (ToType(toType), Type(ComplType))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeKind (TypeKind(Star, (:->), Constraint), HasTypeKind (getTypeKind))
import safe CMM.Inference.TypeVar
  ( ToTypeVar(toTypeVar)

  , noType, TypeVar (NoType, tVarId)
  )
import safe CMM.Parser.HasPos (SourcePos)
import safe CMM.AST.Variables.State (CollectorState)
import safe qualified CMM.AST.Variables.State as CS
import safe CMM.Inference.TypeCompl (TypeCompl(ConstType), makeFunction, makeApplication)
import safe qualified CMM.Data.Trilean as T
import safe CMM.Inference.State (fieldClassHelper)
import safe CMM.AST (Conv)
import safe CMM.AST.GetConv ( GetConv(getConv) )

import safe CMM.Inference.Preprocess.State.Impl
  ( PreprocessorState(PreprocessorState)
  , cSymbols
  , currentContext
  , facts
  , freshStar
  , freshASTStar
  , freshASTGeneric
  , freshNamedASTStar
  , freshGeneric
  , funcElabVariables
  , funcInstVariables
  , funcVariables
  , initPreprocessor
  , freshASTTypeHandle
  , structInstMembers
  , structMembers
  , typeAliases
  , typeClasses
  , typeConstants
  , typeVariables
  , variables, Preprocessor, freshTypeHelper, freshNamedASTTypeHandle, currentParent
  )

noCurrentReturn :: TypeHole
noCurrentReturn = EmptyTypeHole

beginUnit :: CollectorState -> Preprocessor ()
beginUnit collector = do
  variables <~ pure <$> declVars (collector ^. CS.variables)
  funcVariables <~ declVars (collector ^. CS.funcVariables)
  funcElabVariables <~ declVars (collector ^. CS.funcInstVariables)
  tCons <- declVars $ collector ^. CS.typeConstants
  untrivialize tCons
  typeConstants .= pure tCons
  classHandles <- declVars $ complThd3 <$> (collector ^. CS.typeClasses)
  typeClasses .=
    Map.intersectionWith
      (ClassData . SimpleTypeHole)
      classHandles
      ((^. _3) <$> (collector ^. CS.typeClasses))
  memberClasses <-
    declVars . Map.fromAscList $ bimap fieldClassHelper (second $ \_ -> Star :-> Star :-> Constraint) <$> Map.toAscList members
  let memberClassData = (`ClassData` mempty) . SimpleTypeHole <$> memberClasses
  typeClasses %= (`Map.union` memberClassData)
  Map.keys memberClasses `for_` \name ->
    storeFact (ClassFunDeps name [[T.False, T.True]] :: FlatFact Type)
  mems <- declVars members
  structMembers .= mems
  mems `for_` \mem -> do
    a <- freshTypeHelper Star
    b <- freshTypeHelper Star
    let t = makeFunction [a] b
    storeFact $
      forall (Set.fromList $ toTypeVar <$> [a, b]) [mem `typeUnion` t]
        [ Fact $ addressKind `kindConstraint` a
        , Fact $ addressKind `kindConstraint` b
        , Fact $ b `subConst` a
        ]
  where
    members = collector ^. CS.structMembers

-- | returns `NoType` on failure
lookupVar :: GetName n => n -> Preprocessor TypeHole
lookupVar named = lookupVarImpl named <$> use variables

lookupFVar :: GetName n => n -> Preprocessor TypeHole
lookupFVar named = lookupVarImpl named <$> uses funcVariables pure

lookupSMem :: GetName n => n -> Preprocessor TypeHole
lookupSMem named = lookupVarImpl named <$> uses structMembers pure

lookupSIMem :: GetName n => n -> Preprocessor TypeHole
lookupSIMem named = do
  handle <- freshTypeHelper Star
  structInstMembers %= Map.adjust (handle :) (getName named)
  return $ SimpleTypeHole handle

lookupFIVar :: GetName n => n -> Preprocessor TypeHole
lookupFIVar named = do
  handle <- freshTypeHelper Star
  funcInstVariables %= Map.adjust (handle :) (getName named)
  return $ SimpleTypeHole handle

lookupFEVar :: GetName n => n -> Preprocessor TypeHole
lookupFEVar named = lookupVarImpl named <$> uses funcElabVariables pure

lookupProc :: GetName n => n -> Preprocessor (Maybe TypeHole)
lookupProc = uses variables . fmap (fmap SimpleTypeHole) . (. last) . Map.lookup . getName

lookupTCon :: GetName n => n -> Preprocessor TypeHole
lookupTCon named = lookupVarImpl named <$> use typeConstants

lookupTVar :: GetName n => n -> Preprocessor TypeHole
lookupTVar named = lookupVarImpl named <$> use typeVariables

lookupClass :: GetName n => n -> Preprocessor TypeHole
lookupClass named =
  uses typeClasses $ maybe EmptyTypeHole (^. classHole) . (getName named `Map.lookup`)

lookupVarImpl :: GetName n => n -> [Map Text TypeHandle] -> TypeHole
lookupVarImpl named vars =
  maybe EmptyTypeHole SimpleTypeHole . foldr (<|>) Nothing $ (getName named `Map.lookup`) <$>
  vars

storeVar :: Text -> Type -> Preprocessor ()
storeVar name handle = do
  vars <- use variables
  storeVarImpl name handle vars

untrivialize :: Map Text TypeHandle -> Preprocessor ()
untrivialize tCons =
  Map.toList tCons `for_` \(name', handle) ->
    storeFacts
      [ handle `typeUnion`
        ComplType (ConstType name' (getTypeKind $ handleId handle) NoType)
      , getDataKind "" `kindConstraint` handle
      ]

beginProc :: Text -> TypeHole -> CollectorState -> Maybe Conv -> Preprocessor ()
beginProc name hole collector mConv = do
  vars <- declVars $ collector ^. CS.variables
  variables %= (vars :)
  tAliases <- declVars $ collector ^. CS.typeAliases
  typeAliases %= (tAliases :)
  tVars <- declVars $ collector ^. CS.typeVariables
  typeVariables %= reverse . (tVars :) . reverse
  pushFacts
  currentReturn <- freshTypeHelper Star
  storeFacts
    [ constExprConstraint currentReturn
    , tVarId (handleId currentReturn) `tupleKind` currentReturn
    ]
  pushContext $ FunctionCtx (name, hole) currentReturn mConv

openProc :: CollectorState -> Preprocessor ()
openProc collector = do
  vars' <- declVars $ collector ^. CS.variables
  modifyHead variables (vars' <>)
  tVars' <- declVars $ collector ^. CS.typeVariables
  ~(h:t) <- uses typeVariables reverse
  typeVariables .= reverse ((tVars' <> h) : t)

declVars :: Map Text (SourcePos, TypeKind) -> Preprocessor (Map Text TypeHandle)
declVars =
  Map.traverseWithKey (\name (pos, kind) -> freshNamedASTTypeHandle name pos kind)

endProc :: Preprocessor (Facts, TypeHole)
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

lookupCtxFVar :: Text -> Preprocessor TypeHole
lookupCtxFVar name = use currentContext >>= go
  where
    go =
      \case
        ClassCtx {}:_ -> lookupFVar name
        FunctionCtx (name', handle) _ _:others
          | name == name' -> return handle
          | otherwise -> go others
        GlobalCtx:_ -> lookupFVar name
        InstanceCtx {}:_ -> lookupFIVar name
        StructCtx {}:_ -> lookupFVar name
        [] -> poppedGlobalCtxError

getCurrentReturn :: Preprocessor TypeHole
getCurrentReturn = uses currentContext $ go . head
  where
    go =
      \case
        FunctionCtx _ handle _ -> SimpleTypeHole handle
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
        ClassCtx _ classConstraint' _:_ -> classConstraint'
        InstanceCtx _ classConstraint' _:_ -> classConstraint'
        FunctionCtx {}:others -> go others
        StructCtx {}:others -> go others
        [] -> poppedGlobalCtxError

storeProc :: ToType a => Text -> Facts -> a -> Preprocessor TypeHole
storeProc name fs x = do
  tVars <- collectTVars
  uses currentContext head >>= \case
    GlobalCtx -> do
      hole <- lookupFVar name
      storeFact $ forall tVars [hole `typeUnion` t] fs
      return hole
    ClassCtx _ classConstraint' _ ->
      storeElaboratedProc tVars $
      Fact (uncurry classConstraint classConstraint') :
      fs
    InstanceCtx _ classConstraint' superConstraints ->
      storeElaboratedProc tVars $ Fact (uncurry classFact classConstraint') :
      (Fact . uncurry classFact <$> superConstraints) <>
      fs
    FunctionCtx {} -> goIllegal
    StructCtx {} -> goIllegal
  where
    goIllegal = error "(internal) Illegal local function encountered." -- TODO: reword this (consolidate)
    t = toType x
    storeElaboratedProc tVars facts' = do
      handle <- holeHandle <$> lookupCtxFVar name
      fHandle <- holeHandle <$> lookupFVar name
      eHandle <- holeHandle <$> lookupFEVar name
      iHandle <- freshTypeHelper Star
      classConstraint' <- getCtxClassConstraint
      let eType =
            makeFunction
              [ toType $ ComplType (getConstType constraintWitness) `makeApplication`
                snd classConstraint'
              ]
              t
          instFact = Fact $ eHandle `instType` iHandle
          unionFact = Fact $ iHandle `typeUnion` eType
      uses currentContext head >>= \case
        ClassCtx {} -> do
          storeFact $ forall tVars [eHandle `typeUnion` eType] facts'
          storeFact $ forall tVars [handle `typeUnion` t] [instFact, unionFact]
          return $ SimpleTypeHole handle
        _ -> do
          storeFact . forall tVars [handle `typeUnion` t] $ instFact : unionFact :
            facts'
          return $ MethodTypeHole handle fHandle eHandle

pushParent :: TypeVar -> Preprocessor ()
pushParent parent =
  currentParent %= (parent :)

popParent :: Preprocessor ()
popParent =
  currentParent %= tail

pushFacts :: Preprocessor ()
pushFacts = facts %= ([] :)

pushTypeVariables :: Map Text (SourcePos, TypeKind) -> Preprocessor ()
pushTypeVariables tVars = do
  tVars' <- declVars tVars
  typeVariables %= reverse . (tVars' :) . reverse

popTypeVariables :: Preprocessor ()
popTypeVariables = do
  typeVariables %= init

-- TODO: renew in all context where used
collectTVars :: Preprocessor (Set TypeVar)
collectTVars =
  uses typeVariables (Set.fromList . (handleId <$>) . Map.elems . Map.unions)

-- TODO
pushStruct :: (Text, TypeHole) -> (Text, Type) -> Preprocessor ()
pushStruct handle mainHandle = do
  tVars <- collectTVars
  pushContext $ StructCtx handle mainHandle
  storeFact $ forall tVars [snd handle `typeUnion` snd mainHandle] [Fact . regularExprConstraint $ snd mainHandle]

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
popTopContext = popTop currentContext

popTopFacts :: Preprocessor Facts
popTopFacts = popTop facts

popTop :: Lens' PreprocessorState [t] -> Preprocessor t
popTop from = do
  ~(h:t) <- use from
  from .= t
  return h

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTCon :: ToType a => Text -> a -> Preprocessor ()
storeTCon name handle = use typeConstants >>= storeVarImpl name handle

-- | Stores the given type variable `handle` under the given `name` to the state monad
storeTVar :: ToType a => Text -> a -> Preprocessor ()
storeTVar name handle = use typeVariables >>= storeVarImpl name handle

storeVarImpl ::
     (ToType a, GetName n) => n -> a -> [Map Text TypeHandle] -> Preprocessor ()
storeVarImpl named handle vars =
  storeFact $ lookupVarImpl named vars `typeUnion` handle

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

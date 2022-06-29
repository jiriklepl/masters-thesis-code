{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.PreprocessClass where

import Prelude
import Control.Lens
import CMM.AST.Annot
import CMM.Parser.HasPos
import Data.Graph
import qualified CMM.AST as A
import qualified CMM.Inference.Type as Ty
import qualified CMM.Inference.TypeVar as Ty

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified CMM.Inference.Fact as Ty
import Data.Set (Set)
import qualified Data.Map as Map
import CMM.AST.GetName
import Control.Applicative
import Data.Data
import Data.Generics
import CMM.Data.Generics
import Data.Maybe
import Data.Foldable
import CMM.Utils
import Data.Traversable
import CMM.Inference.DataKind
import CMM.Inference.TypeKind
import CMM.Inference.Constness
import CMM.Inference.BuiltIn
import CMM.AST.Variables
import CMM.AST.Variables.State.Impl hiding (_variables, _typeVariables)


data Witness
  = InstanceWitness { getWitTypes :: [Type], getWitInstance :: A.Instance (SourcePos, Elaboration) }
  | StructWitness { getWitTypes :: [Type],  getWitStruct :: A.Struct (SourcePos, Elaboration) }
  | War { getWitTypes :: [Type] }
  deriving (Show, Data)

data Elaboration
  = EmptyElab
  | TypeElab Type
  | QualElab (Qual Type)
  | WitElab Witness
  deriving (Show, Data)

class ElabType a where
  elabType :: a -> Type

instance ElabType Elaboration where
  elabType = \case
    EmptyElab -> undefined
    WitElab {} -> undefined
    TypeElab t -> t
    QualElab (_ :=> t) -> t

instance ElabType a => ElabType (n, a) where
  elabType = elabType . snd

instance ElabType a => ElabType (n `Annot` a) where
  elabType = elabType . takeAnnot

-- data TypeKind
--   = Generic
--   | Star
--   | Constraint
--   | TypeKind :-> TypeKind
--   deriving (Eq, Ord, Show, Data)

class GetKind a where
  getKind :: a -> TypeKind

instance GetKind TypeKind where
  getKind = id

instance GetKind Elaboration where
  getKind = getKind . elabType

instance GetKind a => GetKind (n `Annot` a) where
  getKind = getKind . takeAnnot

instance GetKind a => GetKind (n, a) where
  getKind = getKind . snd

data Skolem
  = Skolem { skolId :: Int, skolKind :: TypeKind, skolLevel :: Int }
  deriving (Eq, Show, Data)

compareBy :: Ord a => (t -> a) -> t -> t -> Ordering
compareBy f a b = f a `compare` f b

instance Ord Skolem where
  compare = compareBy skolId

instance GetKind Skolem where
  getKind  = skolKind

data KindVar
  = KVar { kVarId :: Int, kVarLevel :: Int }
  deriving (Eq, Show, Data)

instance Ord KindVar where
  compare = compareBy kVarId

data KindType
  = KindVar { getKVar :: KindVar }
  | KindSpec DataKind
  deriving (Eq, Show, Data)

data ConstVar
  = CVar { cVarId :: Int, cVarLevel :: Int }
  deriving (Eq, Show, Data)

instance Ord ConstVar where
  compare = compareBy cVarId

data ConstType
  =  ConstVar { getCVar :: ConstVar }
  |  ConstSpec Constness
  deriving (Eq, Show, Data)

data TypeVar
  = TVar { tVarId :: Int, tVarKind :: TypeKind, tVarLevel :: Int }
  deriving (Eq, Show, Data)

instance Ord TypeVar where
  compare = compareBy tVarId

instance GetKind TypeVar where
  getKind  = tVarKind

data Type
  = Func [Type] [Type]
  | Var TypeVar
  | Bits Int
  | Bool
  | Void
  | App Type Type
  | TConst Text TypeKind
  | Skol Skolem
  | Label
  | Addr Type
  deriving (Eq, Ord, Data, Show)

makeApp :: Foldable f => Type -> f Type -> Type
makeApp = foldl App

instance GetKind Type where
  getKind = \case
    Func {} -> Star
    Var tVar -> getKind tVar
    Bits {} -> Star
    Bool -> Star
    Void -> Star
    App app _ -> case getKind app of
      _ :-> right -> right
      GenericType -> GenericType
      _ -> undefined
    TConst _ kind -> kind
    Skol skol -> getKind skol
    Label -> Star
    Addr {} -> Star

infixr 3 :=>

data Qual a =
  [FlatFact] :=> a
  deriving (Show, Data)

infixr 2 :.

data Scheme a =
  [Skolem] :. Qual a

class PreprocessMentionGraph where
  preprocessMentionGraph ::
    SCC (Annot A.TopLevel SourcePos, Mention, [Mention])
    -> Preprocessor [Annot A.TopLevel (SourcePos, Elaboration)]

data FlatFact
  = Equality Type Type
  | TEquality Type Type
  | KEquality Type Type
  | CEquality Type Type
  | Typing Type Type
  | Kinding Type KindType
  | Consting Type ConstType
  | SubType Type Type
  | SubKind Type Type
  | SubConst Type Type
  | EqKinding KindType KindType
  | EqConsting ConstType ConstType
  | EqTyping Type Type
  | SubKinding KindType KindType
  | SubConsting ConstType ConstType
  | ClassWitness Text Witness
  deriving (Data, Show)

data Fact
  = FlatFact FlatFact
  | NestedFact Mention (Scheme [Fact])

data PreprocessState = PreprocessState
  { _classSchemes :: Map Text (Scheme Witness)
  , _instanceSchemes :: Map Text [Scheme Witness]
  , _typeSchemes :: Map Text Type
  , _varCounter :: Int
  , _currentLevel :: Int
  , _currentFacts :: [[FlatFact]]
  , _endedFacts :: [[Fact]]
  , _typeVariables :: [Map Text Skolem]
  , _variables :: [Map Text TypeVar]
  , _tAliases :: [Map Text TypeVar]
  }

initPreprocessState :: PreprocessState
initPreprocessState = PreprocessState
  { _classSchemes = Map.empty
  , _instanceSchemes = Map.empty
  , _typeSchemes = Map.empty
  , _varCounter = 0
  , _currentLevel = 0
  , _currentFacts = []
  , _endedFacts = []
  , _typeVariables = []
  , _variables = []
  , _tAliases = []
  }

type Preprocessor = State PreprocessState

makeFieldsNoPrefix ''PreprocessState

pushTypeVars :: Preprocessor ()
pushTypeVars =
  typeVariables %= (Map.empty:)

popTypeVars :: Preprocessor ()
popTypeVars =
  typeVariables %= tail

pushVars :: Preprocessor ()
pushVars =
  typeVariables %= (Map.empty:)

popVars :: Preprocessor ()
popVars =
  typeVariables %= tail

pushTAliases :: Preprocessor ()
pushTAliases =
  typeVariables %= (Map.empty:)

popTAliases :: Preprocessor ()
popTAliases =
  typeVariables %= tail

storeTypeVar :: Text -> Skolem -> Preprocessor ()
storeTypeVar name t = do
  ~(h:rest) <- use typeVariables
  typeVariables .= Map.insert name t h : rest

storeVar :: Text -> TypeVar -> Preprocessor ()
storeVar name t = do
  ~(h:rest) <- use variables
  variables .= Map.insert name t h : rest

storeTAlias :: Text -> TypeVar -> Preprocessor ()
storeTAlias name t = do
  ~(h:rest) <- use tAliases
  tAliases .= Map.insert name t h : rest

lookupTypeVar :: Text -> Preprocessor (Maybe Skolem)
lookupTypeVar name = uses typeVariables go
  where
    go [] = Nothing
    go (one:more) = case Map.lookup name one of
      Nothing -> go more
      Just t -> Just t

lookupVar :: Text -> Preprocessor (Maybe TypeVar)
lookupVar name = uses variables go
  where
    go [] = Nothing
    go (one:more) = case Map.lookup name one of
      Nothing -> go more
      Just t -> Just t

lookupTAlias :: Text -> Preprocessor (Maybe TypeVar)
lookupTAlias name = uses tAliases go
  where
    go [] = Nothing
    go (one:more) = case Map.lookup name one of
      Nothing -> go more
      Just t -> Just t


pushLevel :: Preprocessor ()
pushLevel =
  currentLevel += 1

popLevel :: Preprocessor ()
popLevel =
  currentLevel -= 1

getLevel :: Preprocessor Int
getLevel =
  use currentLevel

pushFacts :: Preprocessor ()
pushFacts = do
  currentFacts %= ([]:)
  endedFacts %= ([]:)

popFacts :: Preprocessor ()
popFacts = do
  currentFacts %= ([]:)
  endedFacts %= ([]:)

storeAssume :: FlatFact -> Preprocessor ()
storeAssume fact = do
  ~(h:rest) <- use currentFacts
  currentFacts .= (fact : h) : rest

storeFact :: Fact -> Preprocessor ()
storeFact fact = do
  ~(h:rest) <- use endedFacts
  endedFacts .= (fact : h) : rest

pushContext :: Preprocessor ()
pushContext = do
  pushLevel
  pushFacts
  pushTypeVars
  pushVars
  pushTAliases

popContext :: Mention -> Preprocessor ()
popContext name = do
  popLevel
  facts <- uses currentFacts head
  nesteds <- uses endedFacts head
  popFacts
  skolems <- uses typeVariables $ Map.elems . head
  popTypeVars
  popVars
  popTAliases
  storeFact . NestedFact name $
    skolems :. facts :=> nesteds

nextVar :: Preprocessor Int
nextVar = do
  varCounter += 1
  use varCounter

freshVar :: TypeKind -> Preprocessor TypeVar
freshVar kind =
  liftA2 (`TVar` kind) nextVar getLevel

freshKind :: Preprocessor KindVar
freshKind = liftA2 KVar nextVar getLevel

freshConst :: Preprocessor ConstVar
freshConst = liftA2 CVar nextVar getLevel

freshSkol :: TypeKind -> Preprocessor Skolem
freshSkol kind =
  liftA2 (`Skolem` kind) nextVar getLevel

class Apply a b where
  apply :: Data c => Map a b -> c -> c

instance Apply Skolem Type where
  apply subst = go
    where
      go :: Data d => d -> d
      go = mapCase *|* gmapT go
      mapCase (Skol x) = fromMaybe undefined $ x `Map.lookup` subst
      mapCase t = gmapT go t

instance Apply TypeVar Type where
  apply subst = go
    where
      go :: Data d => d -> d
      go = mapCase *|* gmapT go
      mapCase (Var x) = fromMaybe undefined $ x `Map.lookup` subst
      mapCase t = gmapT go t

instance Apply TypeVar TypeVar where
  apply subst = go
    where
      go :: Data d => d -> d
      go = mapCase *|* gmapT go
      mapCase x = fromMaybe undefined $ x `Map.lookup` subst

unSkolem :: Data a => Scheme a -> Preprocessor (Qual a)
unSkolem (skolems :. preds :=> t) = do
  newVars <- traverse (freshVar . getKind) skolems
  let subst = Map.fromList . zip skolems $ fmap Var newVars
  return $ apply subst preds :=> apply subst t

class PreprocessClass a where
  preprocessClass :: Annot a SourcePos -> Preprocessor (Annot a (SourcePos, Elaboration))

instance PreprocessClass A.ParaType where
  preprocessClass (paraType `Annot` pos) = case paraType of
    A.ParaType app args -> do
      app' <- preprocessClass app
      args' <- traverse preprocessClass args
      let t = makeApp (elabType app') (elabType <$> args')
      return . withAnnot (pos, TypeElab t) $ A.ParaType app' args'

instance PreprocessClass A.Type where
  preprocessClass annotated@(type' `Annot` pos) = case type' of
    A.TBits n -> return $ (, TypeElab $ Bits n) <$> annotated
    A.TName name -> do
      uses typeSchemes (Map.lookup $ getName name) >>= \case
        Nothing -> undefined -- report
        Just t ->  return $ (, TypeElab t) <$> annotated
    A.TAuto Nothing -> do
      tVar <- freshVar GenericType
      return $ (, TypeElab $ Var tVar) <$> annotated
    A.TAuto (Just name) ->
      lookupTypeVar (getName name) >>= \case
        Nothing -> do
          skol <- freshSkol GenericType
          storeTypeVar (getName name) skol
          return $ (, TypeElab $ Skol skol) <$> annotated
        Just skol ->
          return $ (, TypeElab $ Skol skol) <$> annotated
    A.TPar paraType -> do
      paraType' <- preprocessClass paraType
      return $ withAnnot (pos, takeElab paraType') $ A.TPar paraType'

takeElab :: Annotation node (SourcePos, Elaboration) -> Elaboration
takeElab = snd . takeAnnot

instance PreprocessClass (A.ParaName A.Type) where
  preprocessClass (A.ParaName name args `Annot` pos) = do
    args' <- traverse preprocessClass args
    let witness = War (elabType <$> args')
    storeAssume . ClassWitness (getName name ) $ witness
    return . withAnnot (pos, WitElab witness) $ A.ParaName (fmapTrivial name) args'  -- TODO

instance PreprocessClass A.Class where
  preprocessClass (A.Class paraNames paraName methods `Annot` pos) =
    case paraName of
      A.ParaName name tVars `Annot` pos' -> do
        tVars' <- tVars `for` \case
          name' `Annot` pos'' -> do
            skol <- freshSkol GenericType
            storeTypeVar (getName name') skol
            return $ withAnnot (pos'', TypeElab $ Skol skol) (fmapTrivial name')
        let paraName' =
              withAnnot (pos', EmptyElab) $ A.ParaName (fmapTrivial name) tVars'-- TODO
        paraNames' <- traverse preprocessClass paraNames
        methods' <- traverse preprocessClass methods
        let witness = War (elabType <$> tVars')
        storeFact . FlatFact . ClassWitness (getName name) $ witness
        popContext (MentionClass $ getName name)
        return . withAnnot (pos, WitElab witness) $ A.Class paraNames' paraName' methods'  -- TODO

mKindUnif ::
  Maybe A.Kind -> Type -> Type -> Preprocessor ()
mKindUnif mKind derived base =
  traverse_ storeFact . fmap FlatFact $
    case mKind of
      Nothing ->
        [ derived `KEquality` base
        , base `TEquality` derived
        ]
      Just kind ->
        [ derived `TEquality` base
        , derived `Kinding` KindSpec (getDataKind $ getName kind)
        ]

instance PreprocessClass A.Formal where
  preprocessClass (A.Formal mKind invar type' name `Annot` pos) = do
    type'' <- preprocessClass type'
    lookupVar (getName name) >>= \case
       Nothing -> undefined
       Just tVar -> do
        let t = Var tVar
        mKindUnif mKind t (elabType type'')
        return . withAnnot (pos, TypeElab t) $ A.Formal mKind invar type'' (fmapTrivial name)

instance PreprocessClass A.SemiFormal where
  preprocessClass (A.SemiFormal mKind type' `Annot` pos) = do
    type'' <- preprocessClass type'
    t <- fmap Var . freshVar $ getKind type''
    mKindUnif mKind t (elabType type'')
    return . withAnnot (pos, TypeElab t) $ A.SemiFormal mKind type''

instance PreprocessClass A.ProcedureDecl where
  preprocessClass procedure@(A.ProcedureDecl decl `Annot` pos) = case decl of
    A.ProcedureHeader mConv name formals mSemis `Annot` pos' -> do
      let cs = localVariables procedure
      pushContext
      Map.toList (view variables cs) `for_` \(name', (_, kind)) ->
        freshVar kind >>= storeVar name'
      formals' <- traverse preprocessClass formals
      mSemis' <- traverse (traverse preprocessClass) mSemis
      case mConv of
        Nothing -> return ()
        Just (A.Foreign (A.StrLit "C")) -> do
          traverse_ setConst formals'
          traverse_ (traverse_ setConst) mSemis'
        Just (A.Foreign _) -> undefined
      let type' = Func (fmap elabType formals') (maybe [] (fmap elabType) mSemis')
      popContext (MentionMethod $ getName name)
      return .
        withAnnot (pos, TypeElab type') .
        A.ProcedureDecl. withAnnot (pos', TypeElab type') $
        A.ProcedureHeader mConv (fmapTrivial name) formals' mSemis'
    where setConst t =
            storeFact . FlatFact $ elabType t `Consting` ConstSpec Regular

instance PreprocessMentionGraph where
  preprocessMentionGraph = \case
    AcyclicSCC (topLevel `Annot` annot , _, _) -> case topLevel of
      A.TopSection sl ans -> undefined
      A.TopDecl an -> undefined
      A.TopProcedure an -> undefined
      A.TopClass class' -> do
        class'' <- preprocessClass class'
        return [withAnnot (annot, takeElab class'') . A.TopClass $ class'']
      A.TopInstance an -> undefined
      A.TopStruct an -> undefined
    CyclicSCC x0 -> undefined

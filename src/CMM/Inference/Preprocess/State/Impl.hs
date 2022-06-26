{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.State.Impl where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Control.Lens.Getter (uses)
import safe Control.Monad (Monad((>>=)), sequence)
import safe Control.Monad.State (State)
import safe Data.Data (Data)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap))
import safe Data.List (head)
import safe Data.Map (Map)
import safe Data.Maybe (Maybe)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)
import safe qualified Data.Map as Map

import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Inference.Fact (Facts)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter
  ( HandleCounter
  , HasHandleCounter(handleCounter)
  , freshAnnotatedTypeHelperWithParent
  )
import safe CMM.Inference.Preprocess.ClassData (ClassData)
import safe CMM.Inference.Preprocess.Context (Context(GlobalCtx))
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , safeHoleHandle
  )
import safe CMM.Inference.Refresh (Refresher(refresher))
import safe CMM.Inference.TypeAnnot
  ( TypeAnnot(NoTypeAnnot, TypeAST, TypeInst, TypeNamed, TypeNamedAST)
  )
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind)
  , TypeKind(GenericType, Star)
  )
import safe CMM.Inference.TypeVar (TypeVar(NoType))
import safe CMM.Parser.HasPos (HasPos(getPos))
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.Preprocess.Settings
    ( PreprocessorSettings(PreprocessorSettings) )

data PreprocessorState =
  PreprocessorState
    { _variables :: [Map Text TypeHandle]
    , _funcVariables :: Map Text TypeHandle
    , _funcInstVariables :: Map Text [TypeHandle]
    , _funcElabVariables :: Map Text TypeHandle
    , _typeAliases :: [Map Text TypeHandle]
    , _typeConstants :: [Map Text TypeHandle]
    , _typeVariables :: [Map Text TypeHandle]
    , _typeClasses :: Map Text ClassData
    , _structMembers :: Map Text TypeHandle
    , _structInstMembers :: Map Text [TypeHandle]
    , _facts :: [Facts]
    , _cSymbols :: [Text]
    , _currentContext :: [Context]
    , _handleCounter :: HandleCounter
    , _errorState :: ErrorState
    , _currentParent :: [TypeVar]
    }
    deriving (Data)

initPreprocessor :: PreprocessorSettings -> PreprocessorState
initPreprocessor PreprocessorSettings {} =
  PreprocessorState
    { _variables = [mempty]
    , _funcVariables = mempty
    , _funcInstVariables = mempty
    , _funcElabVariables = mempty
    , _typeAliases = [mempty]
    , _typeConstants = [mempty]
    , _typeVariables = [mempty]
    , _typeClasses = mempty
    , _structMembers = mempty
    , _structInstMembers = mempty
    , _facts = [mempty]
    , _cSymbols = mempty
    , _currentContext = [GlobalCtx]
    , _handleCounter = 0
    , _errorState = mempty
    , _currentParent = [NoType]
    }

makeFieldsNoPrefix ''PreprocessorState

type Preprocessor = State PreprocessorState

instance GetParent Preprocessor TypeVar where
  getParent = uses currentParent head

instance Refresher Preprocessor where
  refresher tVars =
    sequence $
    Map.fromSet
      (\tVar ->
         fmap handleId . freshAnnotatedTypeHelper (TypeInst tVar) $
         getTypeKind tVar)
      tVars

getCtxHandle :: Preprocessor (Maybe TypeHandle)
getCtxHandle = uses currentContext (safeHoleHandle . getTypeHole . head)

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshNamedASTTypeHandle ::
     HasPos n => Text -> n -> TypeKind -> Preprocessor TypeHandle
freshNamedASTTypeHandle name node =
  freshAnnotatedTypeHelper . TypeNamedAST name $ getPos node

freshNamedTypeHandle :: Text -> TypeKind -> Preprocessor TypeHandle
freshNamedTypeHandle name = freshAnnotatedTypeHelper $ TypeNamed name

freshNamedNodeTypeHandle ::
     (HasPos n, GetName n) => n -> TypeKind -> Preprocessor TypeHandle
freshNamedNodeTypeHandle node =
  freshAnnotatedTypeHelper . TypeNamedAST (getName node) $ getPos node

freshASTTypeHandle :: HasPos n => n -> TypeKind -> Preprocessor TypeHandle
freshASTTypeHandle node = freshAnnotatedTypeHelper . TypeAST $ getPos node

freshTypeHelper :: TypeKind -> Preprocessor TypeHandle
freshTypeHelper = freshAnnotatedTypeHelper NoTypeAnnot

freshNamedASTStar :: HasPos n => Text -> n -> Preprocessor TypeHandle
freshNamedASTStar name node = freshNamedASTTypeHandle name node Star

freshASTStar :: HasPos n => n -> Preprocessor TypeHandle
freshASTStar = (`freshASTTypeHandle` Star)

freshASTGeneric :: HasPos n => n -> Preprocessor TypeHandle
freshASTGeneric = (`freshASTTypeHandle` GenericType)

freshStar :: Preprocessor TypeHandle
freshStar = freshTypeHelper Star

freshGeneric :: Preprocessor TypeHandle
freshGeneric = freshTypeHelper GenericType

freshAnnotatedTypeHelper :: TypeAnnot -> TypeKind -> Preprocessor TypeHandle
freshAnnotatedTypeHelper annot tKind = do
  getParent >>= freshAnnotatedTypeHelperWithParent annot tKind

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.State.Impl where

import safe Control.Lens ( makeFieldsNoPrefix, uses )
import safe Control.Monad.State (State)
import safe Data.Data (Data)
import safe Data.Map (Map)
import safe Data.Text (Text)
import safe qualified Data.Map as Map

import safe CMM.Inference.Fact (Facts)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter
  ( HasHandleCounter(handleCounter)
  , freshAnnotatedTypeHelperWithParent
  )
import safe CMM.Inference.Preprocess.ClassData (ClassData)
import safe CMM.Inference.Preprocess.Context (Context(GlobalCtx))
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration)
  , safeElabHandle
  )
import safe CMM.Inference.Refresh (Refresh(refresh))
import safe CMM.Inference.TypeAnnot
  ( TypeAnnot(NoTypeAnnot, TypeAST, TypeInst, TypeNamedAST)
  )
import safe CMM.Inference.Properties (Properties, propsId)
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind)
  , TypeKind(GenericType, Star)
  )
import safe CMM.Inference.TypeVar (TypeVar(NoType))
import safe CMM.Parser.GetPos (GetPos(getPos))
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import CMM.Options (Options(Options))

-- | Contains the state of the preprocessor
data PreprocessorState =
  PreprocessorState
    { _variables :: [Map Text Properties] -- ^ For each nested context, maps variable names to their type properties
    , _funcVariables :: Map Text Properties -- ^ Maps function names to their type properties
    , _funcInstVariables :: Map Text [Properties] -- ^ Maps method names to the list of type properties of their instances
    , _funcElabVariables :: Map Text Properties -- ^ Maps function names to the type properties of their elaborated forms (taking a witness of the class)
    , _typeAliases :: [Map Text Properties] -- ^ For each nested context, maps type alias names to their type properties
    , _typeConstants :: [Map Text Properties] -- ^ For each nested context, maps constant names to their type properties
    , _typeVariables :: [Map Text Properties] -- ^ For each nested context, maps type variable names to their type properties
    , _typeClasses :: Map Text ClassData -- ^ Maps each class name to its type hole and a list of its methods
    , _structMembers :: Map Text Properties -- ^ Maps field names to their type properties
    , _structInstMembers :: Map Text [Properties] -- ^ Maps field name instances to their type properties
    , _facts :: [Facts] -- ^ The head refers to the Constraints in the current context
    , _cSymbols :: [Text] -- ^ list of the functions with the 'foreign "C"' specifier
    , _currentContext :: [Context] -- ^ The head refers to the `Context` object representing the given context
    , _handleCounter :: Int -- ^ Counter for fresh variable names when generating type properties
    , _errorState :: ErrorState -- ^ the error state of the preprocessor
    , _currentParent :: [TypeVar] -- ^ The head refers to the parent of the current context
    }
    deriving (Data)

-- | Initiates a preprocessor state with initial values dictated by the given `Options` object
initPreprocessor :: Options -> PreprocessorState
initPreprocessor Options {} =
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

-- | Monadic type for the preprocessor state
type Preprocessor = State PreprocessorState

instance GetParent Preprocessor TypeVar where
  getParent = uses currentParent head

instance Refresh Preprocessor where
  refresh tVars =
    sequence $
    Map.fromSet
      (\tVar ->
         fmap propsId . freshAnnotatedTypeHelper (TypeInst tVar) $
         getTypeKind tVar)
      tVars

-- | Gets the properties of the current context
getCtxHandle :: Preprocessor (Maybe Properties)
getCtxHandle = uses currentContext (safeElabHandle . getElaboration . head)

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshNamedASTProperties ::
     GetPos n => Text -> n -> TypeKind -> Preprocessor Properties
freshNamedASTProperties name node =
  freshAnnotatedTypeHelper . TypeNamedAST name $ getPos node

-- | Generates a new type properties with AST annotation and the given type kind
freshASTProperties :: GetPos n => n -> TypeKind -> Preprocessor Properties
freshASTProperties node = freshAnnotatedTypeHelper . TypeAST $ getPos node

-- | Generates a new type properties with no annotation
freshTypeHelper :: TypeKind -> Preprocessor Properties
freshTypeHelper = freshAnnotatedTypeHelper NoTypeAnnot

-- | Generates a new type properties type-kinded `Star` with AST annotation and name annotation
freshNamedASTStar :: GetPos n => Text -> n -> Preprocessor Properties
freshNamedASTStar name node = freshNamedASTProperties name node Star

-- | Generates a new type properties type-kinded `Star` with AST annotation
freshASTStar :: GetPos n => n -> Preprocessor Properties
freshASTStar = (`freshASTProperties` Star)

-- | Generates a new type properties type-kinded `GenericType` with AST annotation
freshASTGeneric :: GetPos n => n -> Preprocessor Properties
freshASTGeneric = (`freshASTProperties` GenericType)

-- | Generates a new type properties type-kinded `Star` with no annotation
freshStar :: Preprocessor Properties
freshStar = freshTypeHelper Star

-- | Generates a new type properties type-kinded `GenericType` with no annotation
freshGeneric :: Preprocessor Properties
freshGeneric = freshTypeHelper GenericType

-- | Generates a new type properties with the given annotation and the given type kind
freshAnnotatedTypeHelper :: TypeAnnot -> TypeKind -> Preprocessor Properties
freshAnnotatedTypeHelper annot tKind = do
  getParent >>= freshAnnotatedTypeHelperWithParent annot tKind

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.State.Impl where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Control.Lens.Getter (uses)
import safe Control.Monad.State (State)
import safe Data.Data (Data)
import safe Data.Map (Map)
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
import safe CMM.Parser.GetPos (GetPos(getPos))
import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import CMM.Options (Options(Options))

-- | Contains the state of the preprocessor
data PreprocessorState =
  PreprocessorState
    { _variables :: [Map Text TypeHandle] -- ^ For each nested context, maps variable names to their type handles
    , _funcVariables :: Map Text TypeHandle -- ^ Maps function names to their type handles
    , _funcInstVariables :: Map Text [TypeHandle] -- ^ Maps method names to the list of type handles of their instances
    , _funcElabVariables :: Map Text TypeHandle -- ^ Maps function names to the type handles of their elaborated forms (taking a witness of the class)
    , _typeAliases :: [Map Text TypeHandle] -- ^ For each nested context, maps type alias names to their type handles
    , _typeConstants :: [Map Text TypeHandle] -- ^ For each nested context, maps constant names to their type handles
    , _typeVariables :: [Map Text TypeHandle] -- ^ For each nested context, maps type variable names to their type handles
    , _typeClasses :: Map Text ClassData -- ^ Maps each class name to its type hole and a list of its methods
    , _structMembers :: Map Text TypeHandle -- ^ Maps field names to their type handles
    , _structInstMembers :: Map Text [TypeHandle] -- ^ Maps field name instances to their type handles
    , _facts :: [Facts] -- ^ The head refers to the Constraints in the current context
    , _cSymbols :: [Text] -- ^ list of the functions with the 'foreign "C"' specifier
    , _currentContext :: [Context] -- ^ The head refers to the `Context` object representing the given context
    , _handleCounter :: HandleCounter -- ^ Counter for fresh variable names when generating type handles
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

instance Refresher Preprocessor where
  refresher tVars =
    sequence $
    Map.fromSet
      (\tVar ->
         fmap handleId . freshAnnotatedTypeHelper (TypeInst tVar) $
         getTypeKind tVar)
      tVars

-- | Gets the handle of the current context
getCtxHandle :: Preprocessor (Maybe TypeHandle)
getCtxHandle = uses currentContext (safeHoleHandle . getTypeHole . head)

-- | Creates a fresh type variable of the kind `tKind` annotated with the given `name` and the source position of the given `node`
freshNamedASTTypeHandle ::
     GetPos n => Text -> n -> TypeKind -> Preprocessor TypeHandle
freshNamedASTTypeHandle name node =
  freshAnnotatedTypeHelper . TypeNamedAST name $ getPos node

-- | Generates a new type handle with the given kind and annotation specifying its name
freshNamedTypeHandle :: Text -> TypeKind -> Preprocessor TypeHandle
freshNamedTypeHandle name = freshAnnotatedTypeHelper $ TypeNamed name

-- | Generates a new type handle with AST annotation and name annotation and the given type kind
freshNamedNodeTypeHandle ::
     (GetPos n, GetName n) => n -> TypeKind -> Preprocessor TypeHandle
freshNamedNodeTypeHandle node =
  freshAnnotatedTypeHelper . TypeNamedAST (getName node) $ getPos node

-- | Generates a new type handle with AST annotation and the given type kind
freshASTTypeHandle :: GetPos n => n -> TypeKind -> Preprocessor TypeHandle
freshASTTypeHandle node = freshAnnotatedTypeHelper . TypeAST $ getPos node

-- | Generates a new type handle with no annotation
freshTypeHelper :: TypeKind -> Preprocessor TypeHandle
freshTypeHelper = freshAnnotatedTypeHelper NoTypeAnnot

-- | Generates a new type handle type-kinded `Star` with AST annotation and name annotation
freshNamedASTStar :: GetPos n => Text -> n -> Preprocessor TypeHandle
freshNamedASTStar name node = freshNamedASTTypeHandle name node Star

-- | Generates a new type handle type-kinded `Star` with AST annotation
freshASTStar :: GetPos n => n -> Preprocessor TypeHandle
freshASTStar = (`freshASTTypeHandle` Star)

-- | Generates a new type handle type-kinded `GenericType` with AST annotation
freshASTGeneric :: GetPos n => n -> Preprocessor TypeHandle
freshASTGeneric = (`freshASTTypeHandle` GenericType)

-- | Generates a new type handle type-kinded `Star` with no annotation
freshStar :: Preprocessor TypeHandle
freshStar = freshTypeHelper Star

-- | Generates a new type handle type-kinded `GenericType` with no annotation
freshGeneric :: Preprocessor TypeHandle
freshGeneric = freshTypeHelper GenericType

-- | Generates a new type handle with the given annotation and the given type kind
freshAnnotatedTypeHelper :: TypeAnnot -> TypeKind -> Preprocessor TypeHandle
freshAnnotatedTypeHelper annot tKind = do
  getParent >>= freshAnnotatedTypeHelperWithParent annot tKind

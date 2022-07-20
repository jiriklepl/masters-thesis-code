{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.State.Impl where

import safe Control.Lens (makeFieldsNoPrefix, uses)
import safe Control.Monad.State (State)
import safe Data.Data (Data)
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Text (Text)

import safe CMM.Err.State (ErrorState, HasErrorState(errorState))
import safe CMM.Inference.Fact (Facts)
import safe CMM.Inference.GetParent (GetParent(getParent))
import safe CMM.Inference.HandleCounter
  ( HasHandleCounter(handleCounter)
  , freshTypeHelperWithParent
  )
import safe CMM.Inference.Preprocess.ClassData (ClassData)
import safe CMM.Inference.Preprocess.Context (Context(GlobalCtx))
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration)
  , safeElabHandle
  )
import safe CMM.Inference.Properties (Properties, propsId)
import safe CMM.Inference.Refresh (Refresh(refresh))
import safe CMM.Inference.TypeKind
  ( HasTypeKind(getTypeKind)
  , TypeKind(GenericType, Star)
  )
import safe CMM.Inference.TypeVar (TypeVar(NoType))
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
    sequence $ Map.fromSet (fmap propsId . freshTypeHelper . getTypeKind) tVars

-- | Gets the properties of the current context
getCtxHandle :: Preprocessor (Maybe Properties)
getCtxHandle = uses currentContext (safeElabHandle . getElaboration . head)

-- | Generates a new type properties type-kinded `Star` with no annotation
freshStar :: Preprocessor Properties
freshStar = freshTypeHelper Star

-- | Generates a new type properties type-kinded `GenericType` with no annotation
freshGeneric :: Preprocessor Properties
freshGeneric = freshTypeHelper GenericType

-- | Generates a new type properties with the given type kind
freshTypeHelper :: TypeKind -> Preprocessor Properties
freshTypeHelper tKind = do
  getParent >>= freshTypeHelperWithParent tKind

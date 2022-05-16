{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Preprocess.State.Impl where

import safe Control.Lens.TH (makeFieldsNoPrefix)

import safe Data.Map (Map)
import safe Data.Monoid (Monoid(mempty))
import safe Data.Text (Text)
import safe Control.Monad.State ( State )
import safe Data.Maybe ( Maybe, maybe )
import safe Data.List ( head )
import safe Data.Function ( (.) )
import safe Data.Functor ( (<$>) )
import safe Control.Lens.Getter ( uses )

import safe CMM.Inference.Fact (Facts)
import safe CMM.Inference.HandleCounter
  ( HandleCounter
  , HasHandleCounter(handleCounter)
  )
import safe CMM.Inference.Preprocess.ClassData (ClassData)
import safe CMM.Inference.Preprocess.Context (Context(GlobalCtx))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar ( noType )
import safe CMM.Inference.Preprocess.TypeHole ( safeHoleHandle )
import safe CMM.Inference.Preprocess.HasTypeHole
    ( HasTypeHole(getTypeHole) )
import safe CMM.Inference.GetParent ( GetParent(getParent) )

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
    , _memberClasses :: Map Text TypeHandle
    , _facts :: [Facts]
    , _cSymbols :: [Text]
    , _currentContext :: [Context]
    , _handleCounter :: HandleCounter
    }

initPreprocessor :: PreprocessorState
initPreprocessor =
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
    , _memberClasses = mempty
    , _facts = [mempty]
    , _cSymbols = mempty
    , _currentContext = [GlobalCtx]
    , _handleCounter = 0
    }

makeFieldsNoPrefix ''PreprocessorState

type Preprocessor = State PreprocessorState

instance GetParent Preprocessor where
  getParent = maybe noType handleId <$> getCtxHandle

getCtxHandle :: Preprocessor (Maybe TypeHandle)
getCtxHandle = uses currentContext (safeHoleHandle . getTypeHole . head)

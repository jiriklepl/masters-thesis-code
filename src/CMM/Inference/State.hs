{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CMM.Inference.State where


import safe Control.Monad.State.Lazy
import safe Control.Lens.Setter
import safe Control.Lens.TH
import safe Data.Text (Text)
import safe qualified Data.Text as Text
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Prettyprinter

import safe CMM.Inference.Type
import safe CMM.Parser.HasPos
import safe CMM.Warnings

type Subst = Map TypeVar Type

data Inferencer =
  Inferencer
    { _currentSubst :: Subst
    , _handleCounter :: Int
    , _errors :: Int
    , _warnings :: Int
    }
    deriving (Show)

initInferencer :: Int -> Inferencer
initInferencer handleCounter =
  Inferencer
    { _currentSubst = mempty
    , _handleCounter = handleCounter
    , _errors = 0
    , _warnings = 0
    }

type MonadInferencer m = (MonadState Inferencer m, MonadIO m)

makeLenses ''Inferencer

registerError ::
     (HasPos n, Pretty n, MonadInferencer m) => n -> Text -> m ()
registerError node message = do
  errors += 1
  makeMessage mkError node message

registerWarning ::
     (HasPos n, Pretty n, MonadInferencer m) => n -> Text -> m ()
registerWarning node message = do
  warnings += 1
  makeMessage mkWarning node message

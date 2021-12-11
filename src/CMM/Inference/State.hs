{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CMM.Inference.State where


import safe Control.Monad.State.Lazy
import safe Control.Lens.Setter
import safe Control.Lens.Getter
import safe Control.Lens.TH
import safe Data.Text (Text)
import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Function

import safe CMM.Inference.Type
import safe CMM.Inference.BuiltIn

type Subst = Map TypeVar Type


data Inferencer =
  Inferencer
    { _typing :: Subst -- contains types of type variables
    , _kinding :: Map TypeVar Text -- contains least kinds of type variables
    , _subKinding :: Map TypeVar (Set TypeVar) -- maps variables to their superKinds
    , _consting :: Map TypeVar (Constness, Constness) -- contains constness limits of type variables
    , _subConsting :: Map TypeVar (Set TypeVar) -- maps variables to their subConsts
    , _handleCounter :: Int
    , _errors :: [UnificationError]
    , _facts :: Facts
    , _assumps :: Facts
    , _context :: Facts
    , _schemes :: Map TypeVar (Scheme Type)
    }
    deriving (Show)

initInferencer :: Int -> Inferencer
initInferencer handleCounter =
  Inferencer
    { _typing = mempty
    , _kinding = mempty
    , _subKinding = mempty
    , _consting = mempty
    , _subConsting = mempty
    , _handleCounter = handleCounter
    , _facts = mempty
    , _assumps = mempty
    , _errors = mempty
    , _context = builtInContext
    , _schemes = mempty
    }

data UnificationError
  = Occurs TypeVar Type
  | Mismatch Type Type
  | NoSubType Type Type -- supertype; subtype
  | NoConstness Constness Type
  | NoKind Text Type
  | NoRegister Text Type
  | TupleMismatch [Type] [Type]
  | GotErrorType Text
  | IllegalPolytype Type
  | BadKind Type Type
  | FalseKind
  | FalseConst
  deriving (Show)

type MonadInferencer m = (MonadState Inferencer m, MonadIO m)

makeLenses ''Inferencer

freshTypeHandle :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHandle tKind = do
  handleCounter += 1
  (Nothing &) . (tKind &) . TypeVar  <$> use handleCounter

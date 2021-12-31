{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module CMM.Inference.State where

import safe Control.Lens.Getter (use)
import safe Control.Lens.Setter ((+=))
import safe Control.Lens.TH (makeLenses)
import safe Control.Monad.State.Lazy (MonadIO, MonadState)
import safe Data.Function ((&))
import safe Data.Map (Map)
import safe Data.Set (Set)
import safe Data.Text (Text)

import safe CMM.Data.Bounds
import safe CMM.Data.Lattice
import safe CMM.Inference.BuiltIn
import safe CMM.Inference.Type

type Subst = Map TypeVar Type

data Inferencer =
  Inferencer
    {
    -- | Contains types of type variables
    _typing :: Subst
    ,
    -- | Contains kind limits of type variables
    _kinding :: Map TypeVar (Bounds DataKind Lattice)
    ,
    -- | Maps variables to their respective superKinding variables (forms a graph)
    _subKinding :: Map TypeVar (Set TypeVar)
    ,
    -- | Contains constness limits of type variables
    _consting :: Map TypeVar (Bounds Constness Ord)
    ,
    -- |
    _unifying :: Map TypeVar (Set Type)
    ,
    -- | Maps variables to their respective subConsting variables (forms a graph - should overlap with transposed `_kinding` graph)
    _subConsting :: Map TypeVar (Set TypeVar)
    ,
    -- | TODO
    _handleCounter :: Int
    ,
    -- | TODO
    _errors :: [UnificationError]
    ,
    -- | TODO
    _facts :: Facts
    ,
    -- | TODO
    _assumps :: Facts
    ,
    -- | TODO
    _context :: Facts
    ,
    -- | TODO
    _schemes :: Map TypeVar (Set (Scheme Type))
    }
  deriving (Show)

initInferencer :: Int -> Inferencer
initInferencer handleCounter =
  Inferencer
    { _typing = mempty
    , _kinding = mempty
    , _subKinding = mempty
    , _consting = mempty
    , _unifying = mempty
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

freshTypeHelper :: MonadInferencer m => TypeKind -> m TypeVar
freshTypeHelper tKind = do
  handleCounter += 1
  (NoTVarAnnot &) . (tKind &) . TypeVar <$> use handleCounter

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.TypeHole where

import safe Data.Function ((.))
import safe Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import safe GHC.Err (error)
import safe Text.Show (Show)
import safe Data.Functor ( Functor(fmap) )
import Data.Text (Text)

import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)

data TypeHole
  = EmptyTypeHole
  | SimpleTypeHole !TypeHandle
  | NamedTypeHole  !TypeHandle !Text
  | LVInstTypeHole !TypeHandle !TypeHole
  | MethodTypeHole !TypeHandle !TypeHandle !TypeHandle
  | MemberTypeHole !TypeHandle ![TypeHole] ![TypeHandle] ![TypeHandle]
  deriving (Show)

instance ToType TypeHole where
  toType = toType . holeHandle

instance ToTypeVar TypeHole where
  toTypeVar = toTypeVar . holeHandle

holeHandle :: TypeHole -> TypeHandle
holeHandle = fromMaybe err . safeHoleHandle
  where
    err = error "(Internal) implementation error" -- TODO

safeHoleHandle :: TypeHole -> Maybe TypeHandle
safeHoleHandle =
  \case
    EmptyTypeHole -> Nothing
    SimpleTypeHole handle -> Just handle
    NamedTypeHole  handle _ -> Just handle
    LVInstTypeHole handle _ -> Just handle
    MethodTypeHole handle _ _ -> Just handle
    MemberTypeHole handle _ _ _ -> Just handle

holeName :: TypeHole -> Text
holeName = \case
  NamedTypeHole _ name -> name
  _ -> error "logic error" -- TODO: logic error

holeId :: TypeHole -> TypeVar
holeId = handleId . holeHandle

safeHoleId :: TypeHole -> Maybe TypeVar
safeHoleId = fmap handleId . safeHoleHandle

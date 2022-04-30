{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.TypeHole where

import safe Data.Function ((.))
import safe Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import safe GHC.Err (error)
import safe Text.Show (Show)

import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (TypeVar)

data TypeHole
  = EmptyTypeHole
  | SimpleTypeHole !TypeHandle
  | LVInstTypeHole !TypeHandle !TypeHole
  | MethodTypeHole !TypeHandle !TypeHandle !TypeHandle
  | MemberTypeHole !TypeHandle ![TypeHandle] ![TypeHandle]
  deriving (Show)

holeHandle :: TypeHole -> TypeHandle
holeHandle = fromMaybe err . safeHoleHandle
  where
    err = error "(Internal) implementation error" -- TODO

safeHoleHandle :: TypeHole -> Maybe TypeHandle
safeHoleHandle =
  \case
    EmptyTypeHole -> Nothing
    SimpleTypeHole handle -> Just handle
    LVInstTypeHole handle _ -> Just handle
    MethodTypeHole handle _ _ -> Just handle
    MemberTypeHole handle _ _ -> Just handle

holeId :: TypeHole -> TypeVar
holeId = handleId . holeHandle

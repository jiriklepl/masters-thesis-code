{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.TypeHole where

import safe Prelude

import safe Data.Maybe (fromMaybe)

import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (TypeVar)

data TypeHole
  = EmptyTypeHole
  | SimpleTypeHole !TypeHandle
  | LVInstTypeHole !TypeHandle !TypeHole
  | MethodTypeHole !TypeHandle !TypeHandle !TypeHandle

holeHandle :: TypeHole -> TypeHandle
holeHandle = fromMaybe err . safeHoleHandle
  where
    err = error "(Internal) implementations error" -- TODO

safeHoleHandle :: TypeHole -> Maybe TypeHandle
safeHoleHandle EmptyTypeHole = Nothing
safeHoleHandle (SimpleTypeHole handle) = Just handle
safeHoleHandle (LVInstTypeHole handle _) = Just handle
safeHoleHandle (MethodTypeHole handle _ _) = Just handle

holeId :: TypeHole -> TypeVar
holeId = handleId . holeHandle

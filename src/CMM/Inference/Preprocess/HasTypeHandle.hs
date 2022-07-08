{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.HasTypeHandle where

import safe CMM.AST.Annot (Annot, takeAnnot)
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (TypeVar)

-- | A class for objects that have a type handle
class HasTypeHandle a where
  getTypeHandle :: a -> TypeHandle

-- | retrieves a variable identifier from the type handle of the given object
getTypeHandleId :: HasTypeHandle a => a -> TypeVar
getTypeHandleId = handleId . getTypeHandle

instance HasTypeHandle TypeHandle where
  getTypeHandle = id

instance HasTypeHandle (a, TypeHandle) where
  getTypeHandle = snd

instance HasTypeHandle a => HasTypeHandle (Annot n a) where
  getTypeHandle = getTypeHandle . takeAnnot

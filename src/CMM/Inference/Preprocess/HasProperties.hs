{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.HasProperties where

import safe CMM.AST.Annot (Annot, takeAnnot)
import safe CMM.Inference.Properties (Properties, propsId)
import safe CMM.Inference.TypeVar (TypeVar)

-- | A class for objects that have a type properties
class HasProperties a where
  getProperties :: a -> Properties

-- | retrieves a variable identifier from the type properties of the given object
getPropertiesId :: HasProperties a => a -> TypeVar
getPropertiesId = propsId . getProperties

instance HasProperties Properties where
  getProperties = id

instance HasProperties (a, Properties) where
  getProperties = snd

instance HasProperties a => HasProperties (Annot n a) where
  getProperties = getProperties . takeAnnot

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.TypeHandle.Impl where

import safe Prelude

import safe Control.Lens.TH (makeLenses)
import safe Data.Data ( Data )

import safe CMM.Inference.TypeVar ( TypeVar )
import safe CMM.Inference.Type ( Type )
import safe CMM.Inference.TypeAnnot ( TypeAnnot )

data TypeHandle =
  TypeHandle
    { _identifier :: TypeVar
    , _typing :: Type
    , _consting :: TypeVar
    , _kinding :: TypeVar
    , _annot :: TypeAnnot
    }
  deriving (Show, Data)

makeLenses ''TypeHandle

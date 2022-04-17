{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.TypeHandle where

import safe Control.Lens.TH (makeLenses)
import safe Data.Data (Data)

import safe CMM.Inference.Type (Type(VarType))
import safe CMM.Inference.TypeAnnot (TypeAnnot)
import safe CMM.Inference.TypeVar (TypeVar)

data TypeHandle =
  TypeHandle
    { _identifier :: TypeVar
    , _typing :: Type
    , _consting :: TypeVar
    , _kinding :: TypeVar
    , _annot :: TypeAnnot
    }
  deriving (Show, Data)

instance Eq TypeHandle where
  TypeHandle {_typing = typing, _consting = consting, _kinding = kinding} == TypeHandle { _typing = typing'
                                                                                        , _consting = consting'
                                                                                        , _kinding = kinding'
                                                                                        } =
    typing == typing' && consting == consting' && kinding == kinding'

instance Ord TypeHandle where
  TypeHandle {_typing = typing, _consting = consting, _kinding = kinding} `compare` TypeHandle { _typing = typing'
                                                                                               , _consting = consting'
                                                                                               , _kinding = kinding'
                                                                                               } =
    compare typing typing' <>
    compare consting consting' <> compare kinding kinding'

initTypeHandle :: TypeAnnot -> TypeVar -> TypeHandle
initTypeHandle annot tVar =
  TypeHandle
    { _identifier = tVar
    , _typing = VarType tVar
    , _consting = tVar
    , _kinding = tVar
    , _annot = annot
    }

handleId :: TypeHandle -> TypeVar
handleId = _identifier

$(makeLenses ''TypeHandle)

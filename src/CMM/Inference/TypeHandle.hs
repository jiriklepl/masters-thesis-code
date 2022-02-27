{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CMM.Inference.TypeHandle where

import safe Data.Data (Data)
import safe Control.Lens.TH (makeLenses)

import safe CMM.Inference.Type ( Type (VarType), TypeAnnot (NoTypeAnnot), TypeVar (NoType) )

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
  TypeHandle{_typing=typing, _consting=consting, _kinding=kinding}
    == TypeHandle{_typing=typing', _consting=consting', _kinding=kinding'} =
      typing == typing' && consting == consting' && kinding == kinding'

instance Ord TypeHandle where
  TypeHandle{_typing=typing, _consting=consting, _kinding=kinding}
    `compare` TypeHandle{_typing=typing', _consting=consting', _kinding=kinding'} =
      compare typing typing' <> compare consting consting' <> compare kinding kinding'

initTypeHandle :: TypeAnnot -> TypeVar -> TypeHandle
initTypeHandle annot tVar =
  TypeHandle
  { _identifier = tVar
  , _typing = VarType tVar
  , _consting = tVar
  , _kinding = tVar
  , _annot = annot
  }

emptyTypeHandle :: TypeHandle
emptyTypeHandle = initTypeHandle NoTypeAnnot NoType

handleId :: TypeHandle -> TypeVar
handleId = _identifier

$(makeLenses ''TypeHandle)

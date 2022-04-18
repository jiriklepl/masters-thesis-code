{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Inference.TypeHandle (module CMM.Inference.TypeHandle, module CMM.Inference.TypeHandle.Impl) where

import safe Prelude

import safe CMM.Inference.Type (Type(VarType))
import safe CMM.Inference.TypeAnnot (TypeAnnot)
import safe CMM.Inference.TypeVar (TypeVar)

import safe CMM.Inference.TypeHandle.Impl

instance Eq TypeHandle where
  TypeHandle {_typing = t, _consting = c, _kinding = k} == TypeHandle { _typing = t'
                                                                                        , _consting = c'
                                                                                        , _kinding = k'
                                                                                        } =
    t == t' && c == c' && k == k'

instance Ord TypeHandle where
  TypeHandle {_typing = t, _consting = c, _kinding = k} `compare` TypeHandle { _typing = t'
                                                                                               , _consting = c'
                                                                                               , _kinding = k'
                                                                                               } =
    compare t t' <>
    compare c c' <> compare k k'

initTypeHandle :: TypeAnnot -> TypeVar -> TypeHandle
initTypeHandle annotation tVar =
  TypeHandle
    { _identifier = tVar
    , _typing = VarType tVar
    , _consting = tVar
    , _kinding = tVar
    , _annot = annotation
    }

handleId :: TypeHandle -> TypeVar
handleId = _identifier

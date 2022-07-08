{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.TypeHandle where

import safe Control.Lens.TH (makeLenses)

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), (<+>), braces, parens)

import safe CMM.Inference.Type (ToType(toType), Type(VarType))
import safe CMM.Inference.TypeAnnot (TypeAnnot)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (constingSymbol, kindingSymbol, typingSymbol)

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
  TypeHandle {_identifier} == TypeHandle { _identifier = i'} =
    _identifier == i'

instance Ord TypeHandle where
  TypeHandle {_identifier} `compare` TypeHandle { _identifier = i'} =
    compare _identifier i'

instance ToType TypeHandle where
  toType = toType . handleId

instance ToTypeVar TypeHandle where
  toTypeVar = toTypeVar . handleId

instance Pretty TypeHandle where
  pretty =
    \case
      TypeHandle { _identifier = tId
                 , _typing = typing
                 , _consting = consting
                 , _kinding = kinding
                 , _annot = annot
                 } ->
        braces $
        pretty tId <+>
        parens (typingSymbol <+> "~" <+> pretty typing) <+>
        parens (constingSymbol <+> "~" <+> pretty consting) <+>
        parens (kindingSymbol <+> "~" <+> pretty kinding) <+> pretty annot

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

makeLenses ''TypeHandle

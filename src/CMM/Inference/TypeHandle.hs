{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.TypeHandle where

import safe Control.Lens.TH (makeLenses)

import safe Data.Bool ((&&))
import safe Data.Data (Data)
import safe Data.Eq (Eq((==)))
import safe Data.Function ((.), ($))
import safe Data.Monoid ((<>))
import safe Data.Ord (Ord(compare))
import safe Text.Show (Show)

import safe CMM.Inference.Type (ToType(toType), Type(VarType))
import safe CMM.Inference.TypeAnnot (TypeAnnot)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import Prettyprinter
import CMM.Pretty (typingSymbol, constingSymbol, kindingSymbol)

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

instance Eq TypeHandle where
  TypeHandle {_typing = t, _consting = c, _kinding = k} == TypeHandle { _typing = t'
                                                                      , _consting = c'
                                                                      , _kinding = k'
                                                                      } =
    t == t' && c == c' && k == k'

instance Ord TypeHandle where
  TypeHandle {_identifier = i, _typing = t, _consting = c, _kinding = k} `compare` TypeHandle { _identifier = i'
                                                                             , _typing = t'
                                                                             , _consting = c'
                                                                             , _kinding = k'
                                                                             } =
    compare i i' <> compare t t' <> compare c c' <> compare k k'

instance ToType TypeHandle where
  toType = toType . handleId

instance ToTypeVar TypeHandle where
  toTypeVar = toTypeVar . handleId

initTypeHandle :: TypeAnnot -> TypeVar -> TypeHandle
initTypeHandle annotation tVar =
  TypeHandle
    { _identifier = tVar
    , _typing = VarType tVar
    , _consting = tVar
    , _kinding = tVar
    , _annot = annotation
    }

instance Pretty TypeHandle where
  pretty = \case
    TypeHandle { _identifier = tId, _typing = typing, _consting = consting, _kinding = kinding, _annot = annot} ->
      braces $ pretty tId <+>
        parens (typingSymbol <+> "~" <+> pretty typing) <+>
        parens (constingSymbol <+> "~" <+> pretty consting) <+>
        parens (kindingSymbol <+> "~" <+> pretty kinding) <+> pretty annot

handleId :: TypeHandle -> TypeVar
handleId = _identifier

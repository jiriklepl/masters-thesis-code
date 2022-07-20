{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}

module CMM.Inference.Properties where

import safe Control.Lens (makeLenses)

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty), (<+>), braces, parens)

import safe CMM.Inference.Type (ToType(toType), Type(VarType))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (constingSymbol, kindingSymbol, typingSymbol)

-- | contains the typing, constness and data kind of a type (identifier is the same as the type variable's it has been initialized from)
data Properties =
  Properties
    { _identifier :: TypeVar
    , _typing :: Type
    , _consting :: TypeVar
    , _kinding :: TypeVar
    }
  deriving (Show, Data)

instance Eq Properties where
  Properties {_identifier} == Properties { _identifier = i'} =
    _identifier == i'

instance Ord Properties where
  Properties {_identifier} `compare` Properties { _identifier = i'} =
    compare _identifier i'

instance ToType Properties where
  toType = toType . propsId

instance ToTypeVar Properties where
  toTypeVar = toTypeVar . propsId

instance Pretty Properties where
  pretty =
    \case
      Properties { _identifier = tId
                 , _typing = typing
                 , _consting = consting
                 , _kinding = kinding
                 } ->
        braces $
        pretty tId <+>
        parens (typingSymbol <+> "~" <+> pretty typing) <+>
        parens (constingSymbol <+> "~" <+> pretty consting) <+>
        parens (kindingSymbol <+> "~" <+> pretty kinding)

-- | initializes a type properties from the given type variable and the given annotation
initProperties :: TypeVar -> Properties
initProperties tVar =
  Properties
    { _identifier = tVar
    , _typing = VarType tVar
    , _consting = tVar
    , _kinding = tVar
    }

-- | returns the identifier of the given type properties
propsId :: Properties -> TypeVar
propsId = _identifier

makeLenses ''Properties

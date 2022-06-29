{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.TypeHole where

import safe Data.Data (Data)
import Data.Text (Text)
import safe Control.Lens.Getter (view)
import safe Control.Lens.Tuple (_2)

import safe Prettyprinter ( Pretty(pretty), (<+>), parens )

import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (emptySet, instSymbol, darrowNice, commaSep)

data TypeHole
  = EmptyTypeHole
  | SimpleTypeHole { holeHandle :: !TypeHandle }
  | NamedTypeHole { holeHandle :: !TypeHandle, holeName :: !Text }
  | LVInstTypeHole { holeHandle :: !TypeHandle,  schemeHandle :: !TypeHandle }
  | MethodTypeHole { holeHandle :: !TypeHandle,  schemeHandle :: !TypeHandle, elabHandle :: !TypeHandle }
  | MemberTypeHole { holeHandle :: !TypeHandle,  classHandles :: ![(Text, TypeHandle)],  instHandles :: ![TypeHandle], schemeHandles :: ![TypeHandle] }
  deriving (Show, Data)

instance Eq TypeHole where
  EmptyTypeHole {} == EmptyTypeHole {} = True
  EmptyTypeHole {} == _ = False
  _ == EmptyTypeHole {} = False
  hole == hole' = holeHandle hole == holeHandle hole'

instance Ord TypeHole where
  EmptyTypeHole {} `compare` EmptyTypeHole {} = EQ
  EmptyTypeHole {} `compare` _ = LT
  _ `compare` EmptyTypeHole {} = GT
  hole `compare` hole' = holeHandle hole `compare` holeHandle hole'

instance Pretty TypeHole where
  pretty = \case
    EmptyTypeHole -> emptySet
    SimpleTypeHole handle ->
      pretty handle
    NamedTypeHole handle name ->
      parens $ pretty name <> "@" <> pretty handle
    LVInstTypeHole inst scheme ->
      parens $ pretty scheme <+> instSymbol <+> pretty inst
    MethodTypeHole inst scheme elab ->
      parens $ pretty scheme <+> instSymbol <+> pretty inst <> ";" <+> pretty elab
    MemberTypeHole sHandle cHandles insts schemes ->
      parens $ pretty sHandle <+> darrowNice <+> commaSep members
      where
        members = zipWith3 goInst cHandles insts schemes
        goInst cHandle inst scheme =
          pretty cHandle <> ":" <+> pretty scheme <+> instSymbol <+> pretty inst

class HasTypeHole a where
  getTypeHole :: a -> TypeHole

instance HasTypeHole TypeHole where
  getTypeHole = id

instance HasTypeHole (a, TypeHole) where
  getTypeHole = view _2

instance HasTypeHole (a, TypeHole, b) where
  getTypeHole = view _2

instance ToType TypeHole where
  toType = toType . holeHandle

instance ToTypeVar TypeHole where
  toTypeVar = toTypeVar . holeHandle

getTypeHoleId :: HasTypeHole a => a -> TypeVar
getTypeHoleId = holeId . getTypeHole

getSafeTypeHoleId :: HasTypeHole a => a -> Maybe TypeVar
getSafeTypeHoleId = safeHoleId . getTypeHole

safeHoleHandle :: TypeHole -> Maybe TypeHandle
safeHoleHandle =
  \case
    EmptyTypeHole -> Nothing
    hole -> Just $ holeHandle hole

holeId :: TypeHole -> TypeVar
holeId = handleId . holeHandle

safeHoleId :: TypeHole -> Maybe TypeVar
safeHoleId = fmap handleId . safeHoleHandle

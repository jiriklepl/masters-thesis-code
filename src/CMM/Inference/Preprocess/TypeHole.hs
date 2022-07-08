{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.TypeHole where

import safe Data.Data (Data)
import safe Control.Lens ( view, set, Field2(_2) )
import safe Data.Text (Text)

import safe Prettyprinter ( Pretty(pretty), (<+>), parens )

import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (emptySet, instSymbol, darrowNice, commaSep)

-- | A type hole object containing the type handle for the type of the corresponding node and some extra data depending on the hole
data TypeHole
  = EmptyTypeHole
  -- ^ type hole for objects with no type
  | SimpleTypeHole { holeHandle :: !TypeHandle }
  -- ^ type hole for most objects
  | LVInstTypeHole { holeHandle :: !TypeHandle,  schemeHandle :: !TypeHandle }
  -- ^ type hole for references to (possibly) polymorphic objects
  | MethodTypeHole { holeHandle :: !TypeHandle,  schemeHandle :: !TypeHandle, elabHandle :: !TypeHandle }
  -- ^ type hole for method instance definitions
  | MemberTypeHole { holeHandle :: !TypeHandle,  classHandles :: ![(Text, TypeHandle)],  instHandles :: ![TypeHandle], schemeHandles :: ![TypeHandle] }
  -- ^ type hole for datums that represent a struct field, it contains a list with names and handles to the classes corresponding to each accessor,
  --   handle to each such accessor instance, and a handle to the corresponding accessor methods
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

-- | updates the type handle in the given type hole
setHoleHandle :: TypeHandle -> TypeHole -> TypeHole
setHoleHandle handle = \case
  EmptyTypeHole -> EmptyTypeHole
  hole' -> hole' {holeHandle= handle}

-- | Class for objects which have a type hole
class HasTypeHole a where
  getTypeHole :: a -> TypeHole -- ^ Retrieves the type hole of the given object
  setTypeHole :: TypeHole -> a -> a -- ^ Sets the type hole of the given object to a new value

instance HasTypeHole TypeHole where
  getTypeHole = id
  setTypeHole = const

instance HasTypeHole (a, TypeHole) where
  getTypeHole = view _2
  setTypeHole = set _2

instance HasTypeHole (a, TypeHole, b) where
  getTypeHole = view _2
  setTypeHole = set _2


instance ToType TypeHole where
  toType = toType . holeHandle

instance ToTypeVar TypeHole where
  toTypeVar = toTypeVar . holeHandle

-- | Extends `holeId` to any object that has a type hole
getTypeHoleId :: HasTypeHole a => a -> TypeVar
getTypeHoleId = holeId . getTypeHole

-- | Extends `safeHoleId` to any object that has a type hole
getSafeTypeHoleId :: HasTypeHole a => a -> Maybe TypeVar
getSafeTypeHoleId = safeHoleId . getTypeHole

-- Returns `Just` the type handle of the given type hole if it has any, otherwise, Nothing
safeHoleHandle :: TypeHole -> Maybe TypeHandle
safeHoleHandle =
  \case
    EmptyTypeHole -> Nothing
    hole -> Just $ holeHandle hole

-- | Gets the variable identifier from the type variable of the given type hole
holeId :: TypeHole -> TypeVar
holeId = handleId . holeHandle

-- | Gets `Just` the variable identifier from the type variable of the given type hole if it has any, otherwise, Nothing
safeHoleId :: TypeHole -> Maybe TypeVar
safeHoleId = fmap handleId . safeHoleHandle

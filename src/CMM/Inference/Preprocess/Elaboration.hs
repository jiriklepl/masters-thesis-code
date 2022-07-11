{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Elaboration where

import safe Data.Data (Data)
import safe Control.Lens ( view, set, Field2(_2) )
import safe Data.Text (Text)

import safe Prettyprinter ( Pretty(pretty), (<+>), parens )

import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.TypeHandle (TypeHandle, handleId)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (emptySet, instSymbol, darrowNice, commaSep)

-- | A elaboration object containing the type handle for the type of the corresponding node and some extra data depending on the hole
data Elaboration
  = EmptyElaboration
  -- ^ elaboration for objects with no type
  | SimpleElaboration { eHandle :: !TypeHandle }
  -- ^ elaboration for most objects
  | LVInstElaboration { eHandle :: !TypeHandle,  schemeHandle :: !TypeHandle }
  -- ^ elaboration for references to (possibly) polymorphic objects
  | MethodElaboration { eHandle :: !TypeHandle,  schemeHandle :: !TypeHandle, elabHandle :: !TypeHandle }
  -- ^ elaboration for method instance definitions
  | MemberElaboration { eHandle :: !TypeHandle,  classHandles :: ![(Text, TypeHandle)],  instHandles :: ![TypeHandle], schemeHandles :: ![TypeHandle] }
  -- ^ elaboration for datums that represent a struct field, it contains a list with names and handles to the classes corresponding to each accessor,
  --   handle to each such accessor instance, and a handle to the corresponding accessor methods
  deriving (Show, Data)

instance Eq Elaboration where
  EmptyElaboration {} == EmptyElaboration {} = True
  EmptyElaboration {} == _ = False
  _ == EmptyElaboration {} = False
  hole == hole' = eHandle hole == eHandle hole'

instance Ord Elaboration where
  EmptyElaboration {} `compare` EmptyElaboration {} = EQ
  EmptyElaboration {} `compare` _ = LT
  _ `compare` EmptyElaboration {} = GT
  hole `compare` hole' = eHandle hole `compare` eHandle hole'

instance Pretty Elaboration where
  pretty = \case
    EmptyElaboration -> emptySet
    SimpleElaboration handle ->
      pretty handle
    LVInstElaboration inst scheme ->
      parens $ pretty scheme <+> instSymbol <+> pretty inst
    MethodElaboration inst scheme elab ->
      parens $ pretty scheme <+> instSymbol <+> pretty inst <> ";" <+> pretty elab
    MemberElaboration sHandle cHandles insts schemes ->
      parens $ pretty sHandle <+> darrowNice <+> commaSep members
      where
        members = zipWith3 goInst cHandles insts schemes
        goInst cHandle inst scheme =
          pretty cHandle <> ":" <+> pretty scheme <+> instSymbol <+> pretty inst

-- | updates the type handle in the given elaboration
setHoleHandle :: TypeHandle -> Elaboration -> Elaboration
setHoleHandle handle = \case
  EmptyElaboration -> EmptyElaboration
  hole' -> hole' {eHandle= handle}

-- | Class for objects which have an elaboration
class HasElaboration a where
  getElaboration :: a -> Elaboration -- ^ Retrieves the elaboration of the given object
  setElaboration :: Elaboration -> a -> a -- ^ Sets the elaboration of the given object to a new value

instance HasElaboration Elaboration where
  getElaboration = id
  setElaboration = const

instance HasElaboration (a, Elaboration) where
  getElaboration = view _2
  setElaboration = set _2

instance HasElaboration (a, Elaboration, b) where
  getElaboration = view _2
  setElaboration = set _2


instance ToType Elaboration where
  toType = toType . eHandle

instance ToTypeVar Elaboration where
  toTypeVar = toTypeVar . eHandle

-- | Extends `elabId` to any object that has an elaboration
getElaborationId :: HasElaboration a => a -> TypeVar
getElaborationId = elabId . getElaboration

-- | Extends `safeElabId` to any object that has an elaboration
getSafeElaborationId :: HasElaboration a => a -> Maybe TypeVar
getSafeElaborationId = safeElabId . getElaboration

-- Returns `Just` the type handle of the given elaboration if it has any, otherwise, Nothing
safeElabHandle :: Elaboration -> Maybe TypeHandle
safeElabHandle =
  \case
    EmptyElaboration -> Nothing
    hole -> Just $ eHandle hole

-- | Gets the variable identifier from the type variable of the given type hole
elabId :: Elaboration -> TypeVar
elabId = handleId . eHandle

-- | Gets `Just` the variable identifier from the type variable of the given type hole if it has any, otherwise, Nothing
safeElabId :: Elaboration -> Maybe TypeVar
safeElabId = fmap handleId . safeElabHandle

{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Elaboration where

import safe Data.Data (Data)
import safe Control.Lens ( view, set, Field2(_2) )
import safe Data.Text (Text)

import safe Prettyprinter ( Pretty(pretty), (<+>), parens )

import safe CMM.Inference.Type (ToType(toType))
import safe CMM.Inference.Properties (Properties, propsId)
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Pretty (emptySet, instSymbol, darrowNice, commaSep)

-- | A elaboration object containing the type properties for the type of the corresponding node and some extra data depending on the hole
data Elaboration
  = EmptyElaboration
  -- ^ elaboration for objects with no type
  | SimpleElaboration { eHandle :: !Properties }
  -- ^ elaboration for most objects
  | LVInstElaboration { eHandle :: !Properties,  schemeHandle :: !Properties }
  -- ^ elaboration for references to (possibly) polymorphic objects
  | MethodElaboration { eHandle :: !Properties,  schemeHandle :: !Properties, elabHandle :: !Properties }
  -- ^ elaboration for method instance definitions
  | MemberElaboration { eHandle :: !Properties,  classHandles :: ![(Text, Properties)],  instHandles :: ![Properties], schemeHandles :: ![Properties] }
  -- ^ elaboration for datums that represent a struct field, it contains a list with names and properties to the classes corresponding to each accessor,
  --   properties to each such accessor instance, and a properties to the corresponding accessor methods
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
    SimpleElaboration props ->
      pretty props
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

-- | updates the type properties in the given elaboration
setElabHandle :: Properties -> Elaboration -> Elaboration
setElabHandle props = \case
  EmptyElaboration -> EmptyElaboration
  hole' -> hole' {eHandle= props}

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

-- Returns `Just` the type properties of the given elaboration if it has any, otherwise, Nothing
safeElabHandle :: Elaboration -> Maybe Properties
safeElabHandle =
  \case
    EmptyElaboration -> Nothing
    hole -> Just $ eHandle hole

-- | Gets the variable identifier from the type variable of the given type hole
elabId :: Elaboration -> TypeVar
elabId = propsId . eHandle

-- | Gets `Just` the variable identifier from the type variable of the given type hole if it has any, otherwise, Nothing
safeElabId :: Elaboration -> Maybe TypeVar
safeElabId = fmap propsId . safeElabHandle

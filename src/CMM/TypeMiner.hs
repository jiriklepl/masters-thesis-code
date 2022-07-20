{-# LANGUAGE Safe #-}

{-|
Module      : CMM.TypeMiner
Description : Type-translation to LLVM
Maintainer  : jiriklepl@seznam.cz

This module translates types from the type inference to LLVM
-}
module CMM.TypeMiner where

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Data.Function ((.>))
import safe CMM.Inference.Preprocess.Elaboration
  ( Elaboration(EmptyElaboration, eHandle)
  , HasElaboration(getElaboration)
  )
import safe CMM.Inference.State (Inferencer)
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.Type (Type(ComplType, VarType))
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, ConstType, FunctionType,
          LabelType, String16Type, StringType, TBitsType, TupleType,
          VoidType)
  )
import safe CMM.Inference.TypeKind (HasTypeKind(getTypeKind), TypeKind(Star))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Utils (HasCallStack, logicError, notYetImplemented)
import safe Control.Monad ((>=>))
import safe qualified Data.Map as Map
import safe Data.Map (Map)
import safe Data.Text (Text)
import safe Data.Traversable (for)
import safe Data.Tuple.Extra (second)
import qualified LLVM.AST.Type as L

type StructData = Map Text ([(Text, Int)], [L.Type])

mineElaborated ::
     (HasCallStack, HasElaboration a) => StructData -> a -> Inferencer L.Type
mineElaborated structs =
  getElaboration .> \case
    EmptyElaboration -> logicError
    elab -> mineTypeHandle structs $ eHandle elab

mineElaboration ::
     HasCallStack => StructData -> Elaboration -> Inferencer L.Type
mineElaboration structs =
  \case
    EmptyElaboration -> logicError
    elab -> mineTypeHandle structs $ eHandle elab

mineTypeHandle ::
     (ToTypeVar a, HasCallStack) => StructData -> a -> Inferencer L.Type
mineTypeHandle structs = State.getTyping . toTypeVar >=> mineType structs

mineStructName :: HasElaboration a => a -> Inferencer Text
mineStructName = fmap go . State.getTyping . toTypeVar . getElaboration
  where
    go (t :: Type) =
      case t of
        VarType {} -> logicError
        ComplType tc ->
          case tc of
            ConstType name _ _ -> name
            AddrType t' -> go t'
            _ -> logicError

collectStructs ::
     HasElaboration annot
  => StructData
  -> Annotation AST.Unit annot
  -> Inferencer StructData
collectStructs structs (AST.Unit topLevels `Annot` _) =
  Map.fromList <$> do
    collected <-
      reverse topLevels `for` \(topLevel `Annot` _) ->
        case topLevel of
          AST.TopSection {} -> notYetImplemented
          AST.TopDecl {} -> return []
          AST.TopProcedure {} -> return []
          AST.TopClass {} -> return []
          AST.TopInstance {} -> return []
          AST.TopStruct struct -> pure <$> mineStruct structs struct
    return $ concat collected

mineStruct ::
     (HasCallStack, HasElaboration annot)
  => StructData
  -> Annot AST.Struct annot
  -> Inferencer (Text, ([(Text, Int)], [L.Type]))
mineStruct structs (AST.Struct name datums `Annot` _) =
  (getName name, ) <$> mineDatums structs datums

mineDatums ::
     (HasCallStack, HasElaboration annot)
  => StructData
  -> [Annot AST.Datum annot]
  -> Inferencer ([(Text, Int)], [L.Type])
mineDatums structs = fmap fix . go
  where
    fix (ids, fs) = (second (length fs -) <$> ids, fs)
    go [] = return ([], [])
    go ((datum `Annot` _):others) = do
      (points, data') <- go others
      case datum of
        AST.DatumLabel (AST.Name name) ->
          return ((name, length data') : points, data')
        AST.DatumAlign {} -> notYetImplemented
        AST.Datum _ t Nothing _ -> do
          t' <- mineElaboration structs $ getElaboration t
          return (points, t' : data')
        AST.Datum {} -> notYetImplemented

makePacked :: [L.Type] -> L.Type
makePacked = L.StructureType True

makeUnpacked :: [L.Type] -> L.Type
makeUnpacked = L.StructureType False

mineType :: HasCallStack => StructData -> Type -> Inferencer L.Type
mineType structs t =
  case t of
    VarType {} -> error "-"
    ComplType tc ->
      case tc of
        TupleType args -> makePacked <$> traverse (mineType structs) args
        FunctionType args ret -> do
          args' <- traverse (mineType structs) args
          ret' <- mineType structs ret
          return $ L.FunctionType ret' args' False
        AppType {} -> notYetImplemented
        AddrType arg -> L.ptr <$> mineType structs arg
        ConstType name _ _ ->
          case getTypeKind t of
            Star ->
              case name `Map.lookup` structs of
                Nothing -> logicError
                Just (_, datums) -> do
                  return $ makeUnpacked datums
            _ -> notYetImplemented
        StringType -> notYetImplemented
        String16Type -> notYetImplemented
        LabelType -> return L.LabelType
        TBitsType n -> return . L.IntegerType . fromInteger $ toInteger n
        BoolType -> return L.i1
        VoidType -> return L.void

{-# LANGUAGE Safe #-}

module CMM.TypeMiner where

import safe qualified CMM.AST as AST
import qualified LLVM.AST.Type as L
import safe CMM.AST.Annot ( Annot, Annotation(Annot) )
import safe CMM.Inference.State ( Inferencer )
import safe CMM.Inference.Type ( Type (VarType, ComplType) )
import safe CMM.Inference.TypeCompl
    ( TypeCompl(VoidType, TupleType, FunctionType, AppType, AddrType,
                ConstType, StringType, String16Type, LabelType, TBitsType,
                BoolType) )
import safe CMM.Inference.Preprocess.Elaboration
    ( HasElaboration(getElaboration), Elaboration(EmptyElaboration, eHandle) )
import safe Data.Text (Text)
import safe CMM.AST.GetName ( GetName(getName) )
import safe Data.Traversable ( for )
import safe qualified Data.Map as Map
import safe Data.Map ( Map )
import safe CMM.Inference.TypeKind
    ( HasTypeKind(getTypeKind), TypeKind(Star) )
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.TypeVar ( ToTypeVar(toTypeVar) )
import safe Control.Monad ( (>=>) )
import safe CMM.Data.Function ((.>))
import safe Data.Tuple.Extra (second)
import safe CMM.Utils ( HasCallStack )

type StructData = Map Text ([(Text, Int)], [L.Type])

mineElaborated :: (HasCallStack, HasElaboration a) => StructData -> a -> Inferencer L.Type
mineElaborated structs = getElaboration .> \case
  EmptyElaboration -> undefined
  elab -> mineTypeHandle structs $ eHandle elab

mineElaboration :: HasCallStack => StructData -> Elaboration -> Inferencer L.Type
mineElaboration structs = \case
  EmptyElaboration -> undefined
  elab -> mineTypeHandle structs $ eHandle elab

mineTypeHandle :: (ToTypeVar a, HasCallStack) => StructData
  -> a
  -> Inferencer L.Type
mineTypeHandle structs = State.getTyping . toTypeVar >=> mineType structs

mineStructName :: HasElaboration a => a
  -> Inferencer Text
mineStructName = fmap go . State.getTyping . toTypeVar . getElaboration
  where
    go (t :: Type) = case t of
      VarType {} -> undefined
      ComplType tc -> case tc of
        ConstType name _ _ -> name
        AddrType t' -> go t'
        _ -> undefined

collectStructs :: HasElaboration annot =>
  StructData
  -> Annotation AST.Unit annot
  -> Inferencer StructData
collectStructs structs (AST.Unit topLevels `Annot` _) = Map.fromList <$> do
  collected <- reverse topLevels `for` \(topLevel `Annot` _) -> case topLevel of
    AST.TopSection {} -> undefined
    AST.TopDecl {} -> return []
    AST.TopProcedure {} -> return []
    AST.TopClass {} -> return []
    AST.TopInstance {} -> return []
    AST.TopStruct struct -> pure <$> mineStruct structs struct
  return $ concat collected

mineStruct :: (HasCallStack, HasElaboration annot) => StructData -> Annot AST.Struct annot -> Inferencer (Text, ([(Text, Int)], [L.Type]))
mineStruct structs (AST.Struct name datums `Annot` _) = (getName name,) <$> mineDatums structs datums

mineDatums :: (HasCallStack, HasElaboration annot) => StructData -> [Annot AST.Datum annot] -> Inferencer ([(Text, Int)], [L.Type])
mineDatums structs = fmap fix . go
  where
    fix (ids, fs) = (second (length fs -) <$> ids, fs)
    go [] = return ([], [])
    go  ((datum `Annot` _) : others) = do
      (points, data') <- go others
      case datum of
        AST.DatumLabel (AST.Name name) -> return ((name,length data') : points, data')
        AST.DatumAlign {} -> undefined
        AST.Datum _ t Nothing _ -> do
          t' <- mineElaboration structs $ getElaboration t
          return (points, t':data')
        AST.Datum {} -> undefined

makePacked :: [L.Type] -> L.Type
makePacked = L.StructureType True

makeUnpacked :: [L.Type] -> L.Type
makeUnpacked = L.StructureType False

mineType :: HasCallStack => StructData -> Type -> Inferencer L.Type
mineType structs t = case t of
  VarType {} -> error "-"
  ComplType tc -> case tc of
    TupleType args -> makePacked <$> traverse (mineType structs) args
    FunctionType args ret -> do
      args' <- traverse (mineType structs) args
      ret' <- mineType structs ret
      return $ L.FunctionType ret' args' False
    AppType {} -> undefined
    AddrType arg -> L.ptr <$> mineType structs arg
    ConstType name _ _  -> case getTypeKind t of
      Star -> case name `Map.lookup` structs of
        Nothing -> undefined
        Just (_, datums) -> do
          return $ makeUnpacked datums
      _ -> undefined
    StringType -> undefined
    String16Type -> undefined
    LabelType -> return L.LabelType
    TBitsType n -> return . L.IntegerType . fromInteger $ toInteger n
    BoolType -> return L.i1
    VoidType -> return L.void

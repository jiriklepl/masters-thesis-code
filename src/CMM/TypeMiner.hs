{-# LANGUAGE Trustworthy #-}

module CMM.TypeMiner where

import safe qualified CMM.AST as AST
import safe qualified LLVM.AST.Type as L
import safe CMM.AST.Annot
import safe CMM.Inference.State
import safe CMM.Inference.Type
import safe CMM.Inference.TypeCompl
import safe CMM.Inference.Preprocess.TypeHole
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Data
import safe Data.Generics.Aliases
import CMM.AST.GetName
import Data.Traversable
import qualified Data.Map as Map
import Data.Map
import CMM.Inference.TypeKind
import CMM.Inference.TypeHandle
import safe qualified CMM.Inference.State as State
import CMM.Inference.TypeVar
import Control.Monad
import CMM.Data.Function ((.>))

mineTypeHoled :: HasTypeHole a => Map Text ([(Text, Int)], [Type]) -> a -> Inferencer L.Type
mineTypeHoled structs = getTypeHole .> \case
  EmptyTypeHole -> undefined
  hole -> mineTypeHandle structs $ holeHandle hole

mineTypeHole :: Map Text ([(Text, Int)], [Type]) ->TypeHole -> Inferencer L.Type
mineTypeHole structs = \case
  EmptyTypeHole -> undefined
  hole -> mineTypeHandle structs $ holeHandle hole

mineTypeHandle :: ToTypeVar a => Map Text ([(Text, Int)], [Type])
  -> a
  -> Inferencer L.Type
mineTypeHandle structs = State.getTyping . toTypeVar >=> mineType structs

collectStructs structs (AST.Unit topLevels `Annot` _) = Map.fromList <$> do
  collected <- reverse topLevels `for` \(topLevel `Annot` _) -> case topLevel of
    AST.TopSection {} -> undefined
    AST.TopDecl {} -> return []
    AST.TopProcedure {} -> return []
    AST.TopClass {} -> return []
    AST.TopInstance {} -> return []
    AST.TopStruct struct -> pure <$> mineStruct structs struct
  return $ concat collected

mineStruct :: HasTypeHole annot => Map Text ([(Text, Int)], [Type]) -> Annot AST.Struct annot -> Inferencer (Text, ([(Text, Int)], [L.Type]))
mineStruct structs (AST.Struct name datums `Annot` _) = (getName name,) <$> mineDatums structs datums

mineDatums :: HasTypeHole annot => Map Text ([(Text, Int)], [Type]) -> [Annot AST.Datum annot] -> Inferencer ([(Text, Int)], [L.Type])
mineDatums _ [] = return ([], [])
mineDatums structs ((datum `Annot` _) : others) = do
  (points, data') <- mineDatums structs others
  case datum of
    AST.DatumLabel (AST.Name name) -> return ((name,length data') : points, data')
    AST.DatumAlign {} -> undefined
    AST.Datum _ t Nothing _ -> do
      t' <- mineTypeHole structs $ getTypeHole t
      return (points, t':data')
    AST.Datum {} -> undefined

makePacked :: [L.Type] -> L.Type
makePacked = L.StructureType True

makeUnpacked :: [L.Type] -> L.Type
makeUnpacked = L.StructureType False


mineType :: Map Text ([(Text, Int)], [Type]) -> Type -> Inferencer L.Type
mineType structs t = case t of
  ErrorType {} -> error "-"
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
          datums' <- traverse (mineType structs) datums
          return $ makeUnpacked datums'
      _ -> undefined
    StringType -> undefined
    String16Type -> undefined
    LabelType -> return L.LabelType
    TBitsType n -> return . L.IntegerType . fromInteger $ toInteger n
    BoolType -> return L.i1
    VoidType -> return L.void

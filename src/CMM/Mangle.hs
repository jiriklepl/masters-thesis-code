{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Mangle where

import safe qualified CMM.Inference.State as State
import safe qualified CMM.Inference
import safe qualified CMM.AST as AST

import safe CMM.Inference.State (Inferencer)
import CMM.AST.Annot
import CMM.Inference.Preprocess.TypeHole
import Data.Data
import Data.Generics.Aliases
import CMM.Parser.HasPos
import CMM.AST.BlockAnnot
import CMM.Inference.TypeVar
import CMM.Inference.Type
import CMM.Inference.TypeCompl
import Control.Lens
import CMM.FillHoles
import Data.Text (Text)
import qualified Data.Text as T
import CMM.AST.GetName
import Data.Foldable
import Data.String (IsString)
import CMM.Err.IsError
import Prettyprinter
import CMM.Parser.ASTError
import Data.Maybe

data MangleError
  = NonConcreteType Type
  | IllegalTypeInformation TypeHole
  deriving (Show, Eq, IsError, Data)

instance Pretty MangleError where
  pretty = \case
    NonConcreteType type' ->
      "Type" <+> pretty type' <+> "is not possible to transform to name"
    IllegalTypeInformation hole ->
      "Cannot get the necessary type information from the intermediate data" <+> pretty hole

class Mangle n where
  mangle :: (Data (n a), Typeable n, HasTypeHole a, HasPos a, Data a) => Annot n a -> Inferencer (Annot n a)

mangledFromHoled :: (HasTypeHole a, HasPos a) => Bool -> a -> Inferencer (Maybe Text)
mangledFromHoled skipAddr holed = do
  typing <- State.getTyping (toTypeVar $ getTypeHole holed)
  case mangleType skipAddr typing of
    Nothing -> do
      registerASTError holed $ NonConcreteType typing
      return Nothing
    Just mangled -> return $ Just mangled

enclosed :: (Semigroup a, IsString a) => a -> a
enclosed txt = "$L" <> txt <> "R$"

mangleType :: Bool -> Type -> Maybe Text
mangleType skipAddr = \case
    ErrorType {} -> undefined
    VarType {} -> undefined
    ComplType tc -> case tc of
      TupleType types -> do
        types' <- traverse (mangleType False) types
        return $ "t" <> enclosed (T.concat types')
      FunctionType args ret -> do
        args' <- traverse (mangleType False) args
        ret' <- mangleType False ret
        return $ "f" <> enclosed (T.concat args') <> enclosed ret'
      AppType app arg -> do
        app' <- mangleType False app
        arg' <- mangleType False arg
        return $ "a" <> enclosed (app' <> arg')
      AddrType t -> if skipAddr
        then mangleType False t
        else do
          t' <- mangleType False t
          return $ "p" <> enclosed t'
      ConstType name _ _ -> return $ "n" <> enclosed name
      StringType -> return "str"
      String16Type -> return "str16"
      LabelType -> return "label"
      TBitsType n -> return $ T.pack $ 'b' : show n
      BoolType -> return "bool"
      VoidType -> return "void"

addName :: GetName n => n -> Text -> Text
addName name = mappend ("_M" <> getName name <> "_$")

instance Mangle n where
  mangle n@(_ `Annot` (_ :: annot)) = go n
    where
      go :: Data d => d -> Inferencer d
      go = gmapM go `extM` lvNameCase `extM` procedureHeaderCase
      lvNameCase (AST.LVName name `Annot` (a :: annot)) = do
        name' <- case getTypeHole a of
          SimpleTypeHole {} -> return name
          LVInstTypeHole {} ->
            AST.Name . addName name . fromMaybe "!error" <$> mangledFromHoled True a
          _ -> do
            registerASTError a . IllegalTypeInformation $ getTypeHole a
            return name
        return $ AST.LVName name' `Annot` a
      lvNameCase lvRef = gmapM go lvRef
      procedureHeaderCase (AST.ProcedureHeader mConv name formals semis `Annot` (a :: annot)) = do
        name' <- case getTypeHole a of
          SimpleTypeHole {} -> case mConv of
            Just conv -> case conv of
              AST.Foreign (AST.StrLit "C") -> return name
              _ -> undefined
            Nothing -> AST.Name . addName name . fromMaybe "!error" <$> mangledFromHoled True a
          MethodTypeHole {} -> AST.Name . addName name . fromMaybe "!error" <$> mangledFromHoled False a
          _ -> do
            registerASTError a . IllegalTypeInformation $ getTypeHole a
            return name
        return $ AST.ProcedureHeader mConv name' formals semis `Annot` a

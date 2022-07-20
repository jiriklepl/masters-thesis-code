{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : CMM.Mangle
Description : Mangling layer
Maintainer  : jiriklepl@seznam.cz

This module defines the mangling postprocessing layer.
-}
module CMM.Mangle where

import safe Data.Data (Data(gmapM), Typeable)
import safe Data.Generics.Aliases (extM)
import safe Data.Maybe (fromMaybe)
import safe Data.String (IsString)
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Prettyprinter (Pretty(pretty), (<+>))

import safe qualified CMM.AST as AST
import safe qualified CMM.Inference.State as State

import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.AST.GetName (GetName(getName))
import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Preprocess.Elaboration
  ( Elaboration(LVInstElaboration, MethodElaboration,
            SimpleElaboration)
  , HasElaboration(getElaboration)
  )
import safe CMM.Inference.State.Impl (Inferencer)
import safe qualified CMM.Inference.Type as Ty
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, ConstType, FunctionType,
          LabelType, String16Type, StringType, TBitsType, TupleType,
          VoidType)
  )
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Parser.ASTError (registerASTError)
import safe CMM.Parser.GetPos (GetPos)
import safe CMM.Utils (HasCallStack, logicError, notYetImplemented)

data MangleError
  = NonConcreteType Ty.Type
  | IllegalTypeInformation Elaboration
  deriving (Show, Eq, IsError, Data)

instance Pretty MangleError where
  pretty =
    \case
      NonConcreteType type' ->
        "Type" <+> pretty type' <+> "is not possible to transform to name"
      IllegalTypeInformation hole ->
        "Cannot get the necessary type information from the intermediate data" <+>
        pretty hole

class Mangle n where
  mangle ::
       (Data (n a), Typeable n, HasElaboration a, GetPos a, Data a)
    => Annot n a
    -> Inferencer (Annot n a)
  -- ^ mangles the name in the given AST node and its subterms, changing procedure names according to their types

mangledFromHoled ::
     (HasCallStack, HasElaboration a, GetPos a)
  => Bool
  -> a
  -> Inferencer (Maybe Text)
mangledFromHoled skipAddr holed = do
  typing <- State.getTyping (toTypeVar $ getElaboration holed)
  case mangleType skipAddr typing of
    Nothing -> do
      registerASTError holed $ NonConcreteType typing
      return Nothing
    Just mangled -> return $ Just mangled

enclosed :: (Semigroup a, IsString a) => a -> a
enclosed txt = "$L" <> txt <> "R$"

mangleType :: Bool -> Ty.Type -> Maybe Text
mangleType skipAddr =
  \case
    Ty.VarType {} -> logicError
    Ty.ComplType tc ->
      case tc of
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
        AddrType t ->
          if skipAddr
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
      go = gmapM go `extM` lValueCase `extM` procedureHeaderCase
      lValueCase (AST.LVName name `Annot` (a :: annot)) = do
        name' <-
          case getElaboration a of
            SimpleElaboration {} -> return name
            LVInstElaboration {} ->
              AST.Name . addName name . fromMaybe "!error" <$>
              mangledFromHoled True a
            _ -> do
              registerASTError a . IllegalTypeInformation $ getElaboration a
              return name
        return $ AST.LVName name' `Annot` a
      lValueCase lvRef = gmapM go lvRef
      procedureHeaderCase (AST.ProcedureHeader mConv name formals semis `Annot` (a :: annot)) = do
        name' <-
          case getElaboration a of
            SimpleElaboration {} ->
              case mConv of
                Just conv ->
                  case conv of
                    AST.Foreign (AST.StrLit "C") -> return name
                    _ -> notYetImplemented
                Nothing ->
                  AST.Name . addName name . fromMaybe "!error" <$>
                  mangledFromHoled True a
            MethodElaboration {} ->
              AST.Name . addName name . fromMaybe "!error" <$>
              mangledFromHoled False a
            _ -> do
              registerASTError a . IllegalTypeInformation $ getElaboration a
              return name
        return $ AST.ProcedureHeader mConv name' formals semis `Annot` a

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : CMM.FillElabs
Description : Elaboration filling
Maintainer  : jiriklepl@seznam.cz

This module defines filling of elaborations in the monomorphized input AST.
-}
module CMM.FillElabs where

import safe Data.Data (Data(gmapM), Typeable)
import safe Data.Generics.Aliases (extM)
import safe Data.Maybe (fromMaybe)

import safe Prettyprinter (Pretty(pretty), (<+>))

import safe qualified CMM.AST as AST
import safe qualified CMM.Inference.State as State

import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration)
  )
import safe CMM.Inference.State.Impl (Inferencer)
import safe qualified CMM.Inference.Type as T
import safe CMM.Inference.Type (unfoldApp)
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, ConstType, FunctionType,
          LabelType, String16Type, StringType, TBitsType, TupleType,
          VoidType)
  )
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Parser.ASTError (registerASTError)
import safe CMM.Parser.GetPos (GetPos)

newtype FillAnnotError =
  UnbackedType T.Type
  deriving (Show, Eq, IsError, Data)

instance Pretty FillAnnotError where
  pretty (UnbackedType t) =
    "The intermediate form type" <+>
    pretty t <+> "is currently not reflected in the syntax of C--"

class FillElabs n where
  fillHoles ::
       (Data (n a), Typeable n, Data a, HasElaboration a, GetPos a)
    => Annot n a
    -> Inferencer (Annot n a)
  -- ^ fills elaborations in the given AST node (all types are replaced with AST representations of types stored in their elaborations)

typeFromHoled ::
     (HasElaboration a, GetPos a) => a -> Inferencer (Maybe (AST.Type a))
typeFromHoled holed = do
  typing <- State.getTyping . toTypeVar $ getElaboration holed
  case translType holed typing of
    Nothing -> do
      registerASTError holed $ UnbackedType typing
      return Nothing
    Just type' -> return $ Just type'

translType :: a -> T.Type -> Maybe (AST.Type a)
translType holed t =
  case t of
    T.VarType {} -> Nothing
    T.ComplType tc ->
      case tc of
        TupleType {} -> Nothing
        FunctionType {} -> Nothing
        AppType {} -> do
          app' <- withAnnot holed <$> translType holed app
          args' <- traverse (fmap (withAnnot holed) . translType holed) args
          return . AST.TPar . withAnnot holed $ AST.ParaType app' args'
          where (app:args) = unfoldApp t
        AddrType t' -> AST.TPtr . withAnnot holed <$> translType holed t'
        ConstType name _ _ -> Just . AST.TName $ AST.Name name
        StringType -> Nothing
        String16Type -> Nothing
        LabelType -> Just AST.TLabel
        TBitsType n -> Just $ AST.TBits n
        BoolType -> Just AST.TBool
        VoidType -> Just AST.TVoid

instance FillElabs n where
  fillHoles n@(_ `Annot` (_ :: annot)) = go n
    where
      go :: Data d => d -> Inferencer d
      go =
        gmapM go `extM` registersCase `extM` formalCase `extM` semiFormalCase `extM`
        datumCase
      registersCase (AST.Registers mKind (t `Annot` b) nameStrLits `Annot` (a :: annot)) = do
        t' <- fromMaybe t <$> typeFromHoled b
        return (AST.Registers mKind (withAnnot b t') nameStrLits `Annot` a)
      datumCase (AST.Datum new (t `Annot` b) size init' `Annot` (a :: annot)) = do
        t' <- fromMaybe t <$> typeFromHoled b
        size' <- gmapM go size
        init'' <- gmapM go init'
        return (AST.Datum new (withAnnot b t') size' init'' `Annot` a)
      datumCase datum = gmapM go datum
      formalCase (AST.Formal mKind bool (t `Annot` b) name `Annot` (a :: annot)) = do
        t' <- fromMaybe t <$> typeFromHoled b
        return (AST.Formal mKind bool (withAnnot b t') name `Annot` a)
      semiFormalCase (AST.SemiFormal mKind (t `Annot` b) `Annot` (a :: annot)) = do
        t' <- fromMaybe t <$> typeFromHoled b
        return (AST.SemiFormal mKind (withAnnot b t') `Annot` a)

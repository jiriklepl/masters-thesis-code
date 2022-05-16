{-# LANGUAGE Safe #-}

module CMM.AST.GetName where

import safe Data.Function (($), id)
import safe Data.List ((++))
import safe Data.Maybe (Maybe(Just))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe GHC.Err (error)
import safe Text.Show (Show(show))

import safe CMM.AST
  ( Class(Class)
  , Conv(Foreign)
  , Datum(DatumLabel)
  , Decl(ConstDecl, PragmaDecl)
  , Export(Export)
  , Formal(Formal)
  , Import(Import)
  , Instance(Instance)
  , Kind(Kind)
  , KindName(KindName)
  , LValue(LVName)
  , Name(Name)
  , ParaName(ParaName)
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Stmt(ContStmt, LabelStmt)
  , StrLit(StrLit)
  , Struct(Struct)
  , TopLevel(TopClass, TopDecl, TopInstance, TopProcedure, TopSection,
         TopStruct)
  , Type(TAuto, TBits, TName)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot))

class GetName n where
  getName :: n -> Text

instance GetName Text where
  getName = id

instance GetName (Name a) where
  getName =
    \case
      Name n -> n

instance GetName (n a) => GetName (Annot n a) where
  getName =
    \case
      Annot n _ -> getName n

instance GetName (TopLevel a) where
  getName =
    \case
      TopProcedure p -> getName p
      TopDecl d -> getName d
      TopSection (StrLit n) _ -> n
      TopClass c -> getName c
      TopInstance i -> getName i
      TopStruct s -> getName s

instance GetName (Decl a) where
  getName =
    \case
      ConstDecl _ n _ -> getName n
      PragmaDecl n _ -> getName n
      _ -> error "This declaration does not have a name"

instance GetName (Class a) where
  getName =
    \case
      Class _ n _ -> getName n

instance GetName (Instance a) where
  getName =
    \case
      Instance _ n _ -> getName n

instance GetName (Struct a) where
  getName =
    \case
      Struct n _ -> getName n

instance GetName ((ParaName param) a) where
  getName =
    \case
      ParaName n _ -> getName n

instance GetName (Import a) where
  getName =
    \case
      Import _ n -> getName n

instance GetName (Export a) where
  getName =
    \case
      Export n _ -> getName n

instance GetName (Datum a) where
  getName =
    \case
      DatumLabel n -> getName n
      _ -> error "This datum does not have a name"

instance GetName (Procedure a) where
  getName =
    \case
      Procedure h _ -> getName h

instance GetName (ProcedureDecl a) where
  getName =
    \case
      ProcedureDecl h -> getName h

instance GetName (ProcedureHeader a) where
  getName =
    \case
      ProcedureHeader _ n _ _ -> getName n

instance GetName (Formal a) where
  getName =
    \case
      Formal _ _ _ n -> getName n

instance GetName Kind where
  getName =
    \case
      Kind (StrLit n) -> n

instance GetName (Stmt a) where
  getName =
    \case
      LabelStmt n -> getName n
      ContStmt n _ -> getName n
      _ -> error "This statement does not have a name"

instance GetName (KindName a) where
  getName =
    \case
      KindName _ n -> getName n

instance GetName (LValue a) where
  getName =
    \case
      LVName n -> getName n
      _ -> error "This lvalue does not have a name"

instance GetName (Type a) where
  getName =
    \case
      TName n -> getName n
      TBits n -> T.pack $ "bits" ++ show n
      TAuto (Just n) -> getName n
      _ -> error "This type does not have a name"

instance GetName Conv where
  getName =
    \case
      Foreign (StrLit n) -> n

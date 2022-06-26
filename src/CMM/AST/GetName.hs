{-# LANGUAGE Safe #-}

module CMM.AST.GetName where

import safe Data.Function (($), id)
import safe Data.List ((++))
import safe Data.Maybe (Maybe(Just))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe GHC.Err (error)
import safe Text.Show (Show(show))

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot))

class GetName n where
  getName :: n -> Text

instance GetName Text where
  getName = id

instance GetName (AST.Name a) where
  getName =
    \case
      AST.Name n -> n

instance GetName (n a) => GetName (Annot n a) where
  getName =
    \case
      Annot n _ -> getName n

instance GetName (AST.TopLevel a) where
  getName =
    \case
      AST.TopProcedure p -> getName p
      AST.TopDecl d -> getName d
      AST.TopSection (AST.StrLit n) _ -> n
      AST.TopClass c -> getName c
      AST.TopInstance i -> getName i
      AST.TopStruct s -> getName s

instance GetName (AST.Decl a) where
  getName =
    \case
      AST.ConstDecl _ n _ -> getName n
      AST.PragmaDecl n _ -> getName n
      _ -> error "This declaration does not have a name"

instance GetName (AST.Class a) where
  getName =
    \case
      AST.Class _ n _ -> getName n

instance GetName (AST.Instance a) where
  getName =
    \case
      AST.Instance _ n _ -> getName n

instance GetName (AST.Struct a) where
  getName =
    \case
      AST.Struct n _ -> getName n

instance GetName ((AST.ParaName param) a) where
  getName =
    \case
      AST.ParaName n _ -> getName n

instance GetName (AST.Import a) where
  getName =
    \case
      AST.Import _ n -> getName n

instance GetName (AST.Export a) where
  getName =
    \case
      AST.Export n _ -> getName n

instance GetName (AST.Datum a) where
  getName =
    \case
      AST.DatumLabel n -> getName n
      _ -> error "This datum does not have a name"

instance GetName (AST.Procedure a) where
  getName =
    \case
      AST.Procedure h _ -> getName h

instance GetName (AST.ProcedureDecl a) where
  getName =
    \case
      AST.ProcedureDecl h -> getName h

instance GetName (AST.ProcedureHeader a) where
  getName =
    \case
      AST.ProcedureHeader _ n _ _ -> getName n

instance GetName (AST.Formal a) where
  getName =
    \case
      AST.Formal _ _ _ n -> getName n

instance GetName AST.Kind where
  getName =
    \case
      AST.Kind (AST.StrLit n) -> n

instance GetName (AST.Stmt a) where
  getName =
    \case
      AST.LabelStmt n -> getName n
      AST.ContStmt n _ -> getName n
      _ -> error "This statement does not have a name"

instance GetName (AST.KindName a) where
  getName =
    \case
      AST.KindName _ n -> getName n

instance GetName (AST.LValue a) where
  getName =
    \case
      AST.LVName n -> getName n
      _ -> error "This lvalue does not have a name"

instance GetName (AST.Type a) where
  getName =
    \case
      AST.TName n -> getName n
      AST.TBits n -> T.pack $ "bits" ++ show n
      AST.TAuto (Just n) -> getName n
      _ -> error "This type does not have a name"

instance GetName AST.Conv where
  getName =
    \case
      AST.Foreign (AST.StrLit n) -> n

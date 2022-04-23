{-# LANGUAGE Safe #-}

module CMM.AST.HasName where

import safe Data.Function (($))
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

class HasName n where
  getName :: n -> Text

instance HasName (Name a) where
  getName (Name n) = n

instance HasName (n a) => HasName (Annot n a) where
  getName (Annot n _) = getName n

instance HasName (TopLevel a) where
  getName (TopProcedure p) = getName p
  getName (TopDecl d) = getName d
  getName (TopSection (StrLit n) _) = n
  getName (TopClass c) = getName c
  getName (TopInstance i) = getName i
  getName (TopStruct s) = getName s

instance HasName (Decl a) where
  getName (ConstDecl _ n _) = getName n
  getName (PragmaDecl n _) = getName n
  getName _ = error "This declaration does not have a name"

instance HasName (Class a) where
  getName (Class _ n _) = getName n

instance HasName (Instance a) where
  getName (Instance _ n _) = getName n

instance HasName (Struct a) where
  getName (Struct n _) = getName n

instance HasName ((ParaName param) a) where
  getName (ParaName n _) = getName n

instance HasName (Import a) where
  getName (Import _ n) = getName n

instance HasName (Export a) where
  getName (Export n _) = getName n

instance HasName (Datum a) where
  getName (DatumLabel n) = getName n
  getName _ = error "This datum does not have a name"

instance HasName (Procedure a) where
  getName (Procedure h _) = getName h

instance HasName (ProcedureDecl a) where
  getName (ProcedureDecl h) = getName h

instance HasName (ProcedureHeader a) where
  getName (ProcedureHeader _ n _ _) = getName n

instance HasName (Formal a) where
  getName (Formal _ _ _ n) = getName n

instance HasName Kind where
  getName (Kind (StrLit n)) = n

instance HasName (Stmt a) where
  getName (LabelStmt n) = getName n
  getName (ContStmt n _) = getName n
  getName _ = error "This statement does not have a name"

instance HasName (KindName a) where
  getName (KindName _ n) = getName n

instance HasName (LValue a) where
  getName (LVName n) = getName n
  getName _ = error "This lvalue does not have a name"

instance HasName (Type a) where
  getName (TName n) = getName n
  getName (TBits n) = T.pack $ "bits" ++ show n
  getName (TAuto (Just n)) = getName n
  getName _ = error "This type does not have a name"

instance HasName Conv where
  getName (Foreign (StrLit n)) = n

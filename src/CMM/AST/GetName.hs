{-# LANGUAGE Safe #-}

module CMM.AST.GetName where

import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), mapNode)
import safe CMM.Utils ( HasCallStack )

-- | Gets the name of the given node
class GetName n where
  getName :: n -> Text
  mapName :: (Text -> Text) -> n -> n
  setName :: Text -> n -> n
  setName = mapName . const

instance GetName Text where
  getName = id
  mapName = id

instance GetName (AST.Name a) where
  getName =
    \case
      AST.Name n -> n
  mapName f =
    \case
      AST.Name n -> AST.Name $ f n

instance GetName (n a) => GetName (Annot n a) where
  getName =
    \case
      n `Annot` _ -> getName n
  mapName = mapNode . mapName

instance GetName (AST.TopLevel a) where
  getName =
    \case
      AST.TopSection (AST.StrLit n) _ -> n
      AST.TopDecl d -> getName d
      AST.TopProcedure p -> getName p
      AST.TopClass c -> getName c
      AST.TopInstance i -> getName i
      AST.TopStruct s -> getName s
  mapName f = \case
    AST.StrLit n `AST.TopSection` secs -> AST.StrLit (f n) `AST.TopSection` secs
    AST.TopDecl d -> AST.TopDecl $ mapName f d
    AST.TopProcedure p -> AST.TopProcedure $ mapName f p
    AST.TopClass c -> AST.TopClass $ mapName f c
    AST.TopInstance i -> AST.TopInstance $ mapName f i
    AST.TopStruct s -> AST.TopStruct $ mapName f s

instance GetName (AST.Decl a) where
  getName =
    \case
      AST.ConstDecl _ n _ -> getName n
      AST.PragmaDecl n _ -> getName n
      _ -> noNameDecl
  mapName f = \case
    AST.ConstDecl t n e -> AST.ConstDecl t (mapName f n) e
    AST.PragmaDecl n p -> AST.PragmaDecl (mapName f n) p
    _ -> noNameDecl

noNameDecl :: HasCallStack => a
noNameDecl = error "This declaration does not have a name"

instance GetName (AST.Class a) where
  getName =
    \case
      AST.Class _ n _ _ -> getName n
  mapName f =
    \case
      AST.Class s n fd m -> AST.Class s (mapName f n) fd m

instance GetName (AST.Instance a) where
  getName =
    \case
      AST.Instance _ n _ -> getName n
  mapName f =
    \case
      AST.Instance s n m -> AST.Instance s (mapName f n) m

instance GetName (AST.Struct a) where
  getName =
    \case
      AST.Struct n _ -> getName n
  mapName f =
    \case
      AST.Struct n ds -> AST.Struct (mapName f n) ds

instance GetName ((AST.ParaName param) a) where
  getName =
    \case
      AST.ParaName n _ -> getName n
  mapName f =
    \case
      AST.ParaName n ps -> AST.ParaName (mapName f n) ps

instance GetName (AST.Import a) where
  getName =
    \case
      AST.Import _ n -> getName n
  mapName f =
    \case
      AST.Import s n -> AST.Import s (mapName f n)

instance GetName (AST.Export a) where
  getName =
    \case
      AST.Export n _ -> getName n
  mapName f =
    \case
      AST.Export n s -> AST.Export (mapName f n) s

instance GetName (AST.Datum a) where
  getName =
    \case
      AST.DatumLabel n -> getName n
      _ -> noNameDatum
  mapName f =
    \case
      AST.DatumLabel n -> AST.DatumLabel $ mapName f n
      _ -> noNameDatum

noNameDatum :: HasCallStack => a
noNameDatum = error "This datum does not have a name"

instance GetName (AST.Procedure a) where
  getName =
    \case
      AST.Procedure h _ -> getName h
  mapName f =
    \case
      AST.Procedure h b -> mapName f h `AST.Procedure` b

instance GetName (AST.ProcedureDecl a) where
  getName =
    \case
      AST.ProcedureDecl h -> getName h
  mapName f =
    \case
      AST.ProcedureDecl h -> AST.ProcedureDecl $ mapName f h

instance GetName (AST.ProcedureHeader a) where
  getName =
    \case
      AST.ProcedureHeader _ n _ _ -> getName n
  mapName f =
    \case
      AST.ProcedureHeader c n fs sfs -> AST.ProcedureHeader c (mapName f n) fs sfs

instance GetName (AST.Formal a) where
  getName =
    \case
      AST.Formal _ _ _ n -> getName n
  mapName f =
    \case
      AST.Formal k inv t n -> AST.Formal k inv t $ mapName f n

instance GetName AST.Kind where
  getName =
    \case
      AST.Kind (AST.StrLit n) -> n
  mapName f =
    \case
      AST.Kind (AST.StrLit n) -> AST.Kind . AST.StrLit $ f n

instance GetName (AST.Stmt a) where
  getName =
    \case
      AST.LabelStmt n -> getName n
      AST.ContStmt n _ -> getName n
      _ -> noNameStmt
  mapName f =
    \case
      AST.LabelStmt n -> AST.LabelStmt $ mapName f n
      AST.ContStmt n ks -> mapName f n `AST.ContStmt` ks
      _ -> noNameStmt

noNameStmt :: HasCallStack => a
noNameStmt = error "This statement does not have a name"

instance GetName (AST.KindName a) where
  getName =
    \case
      AST.KindName _ n -> getName n
  mapName f =
    \case
      AST.KindName k n -> AST.KindName k $ mapName f n

instance GetName (AST.LValue a) where
  getName =
    \case
      AST.LVName n -> getName n
      _ -> noNameLValue
  mapName f =
    \case
      AST.LVName n -> AST.LVName $ mapName f n
      _ -> noNameLValue

noNameLValue :: HasCallStack => a
noNameLValue = error "This lvalue does not have a name"

instance GetName (AST.Type a) where
  getName =
    \case
      AST.TName n -> getName n
      AST.TBits n -> T.pack $ "bits" ++ show n
      AST.TAuto (Just n) -> getName n
      _ -> noNameType
  mapName f =
    \case
      AST.TName n -> AST.TName $ mapName f n
      AST.TAuto (Just n) -> AST.TAuto . Just $ mapName f n
      _ -> noNameType

noNameType :: HasCallStack => a
noNameType = error "This type does not have a name"

instance GetName AST.Conv where
  getName =
    \case
      AST.Foreign (AST.StrLit n) -> n
  mapName f =
    \case
      AST.Foreign (AST.StrLit n) -> AST.Foreign . AST.StrLit $ f n

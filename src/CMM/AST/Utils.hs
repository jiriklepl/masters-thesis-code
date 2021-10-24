{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.AST.Utils where

import safe Data.Functor
import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe CMM.AST

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

instance HasName (Decl a) where
  getName (ConstDecl _ n _) = getName n
  getName (PragmaDecl n _) = getName n
  getName _ = error "This declaration does not have a name"

instance HasName (Import a) where
  getName (Import _ n) = getName n

instance HasName (Export a) where
  getName (Export n _) = getName n

instance HasName (Datum a) where
  getName (DatumLabel n) = getName n
  getName _ = error "This datum does not have a name"

instance HasName (Procedure a) where
  getName (Procedure _ n _ _) = getName n

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

instance HasName Conv where
  getName (Foreign (StrLit n)) = n

withAnnot :: a -> n a -> Annot n a
withAnnot = flip Annot

takeAnnot :: Annot n a -> a
takeAnnot (Annot _ annot) = annot

unAnnot :: Annot n a -> n a
unAnnot (Annot node _) = node

updateAnnots :: Annotated n => (a -> b) -> n a -> n b
updateAnnots = fmap

stripAnnots :: Annotated n => n a -> n ()
stripAnnots = void

class EnsureNode n' n where
  ensureNode :: n' a -> n a

instance ASTNode n => EnsureNode n n where
  ensureNode = id

instance EnsureNode (Annot n) n where
  ensureNode = unAnnot

getExprLVName :: Annot Expr a -> Maybe Text
getExprLVName (Annot (LVExpr (Annot (LVName n) _)) _) = Just $ getName n
getExprLVName (Annot (ParExpr expr) _) = getExprLVName expr
getExprLVName Annot {} = Nothing

-- | Returns Nothing on failure
class GetTrivialGotoTarget n where
  getTrivialGotoTarget :: n -> Maybe Text

instance GetTrivialGotoTarget (n a) => GetTrivialGotoTarget (Annot n a) where
  getTrivialGotoTarget (Annot n _) = getTrivialGotoTarget n

instance GetTrivialGotoTarget (Arm a) where
  getTrivialGotoTarget (Arm _ body) = getTrivialGotoTarget body

instance GetTrivialGotoTarget (BodyItem a) where
  getTrivialGotoTarget (BodyStmt stmt) = getTrivialGotoTarget stmt
  getTrivialGotoTarget _ = Nothing

instance GetTrivialGotoTarget (Body a) where
  getTrivialGotoTarget (Body [bodyItem]) = getTrivialGotoTarget bodyItem
  getTrivialGotoTarget _ = Nothing

instance GetTrivialGotoTarget (Stmt a) where
  getTrivialGotoTarget (GotoStmt expr _) = getExprLVName expr
  getTrivialGotoTarget _ = Nothing

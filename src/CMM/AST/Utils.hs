{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.AST.Utils where

import safe Data.Text (Text)

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.AST.HasName

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

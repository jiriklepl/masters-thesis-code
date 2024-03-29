{-# LANGUAGE Safe #-}

module CMM.AST.Utils where

import safe Data.List (foldl')
import safe Data.Text (Text)

import safe CMM.AST
  ( Arm(Arm)
  , Body(Body)
  , BodyItem(BodyStmt)
  , Expr(LVExpr, ParExpr)
  , LValue(LVName)
  , Stmt(GotoStmt)
  , TopLevel
  , Unit(Unit)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot), unAnnot, withAnnot)
import safe CMM.AST.GetName (getName)

-- | returns the node representation without any annotation
class EnsureNode n' n where
  ensureNode :: n' a -> n a

instance EnsureNode n n where
  ensureNode = id

instance EnsureNode (Annot n) n where
  ensureNode = unAnnot

-- | Returns `Just` name of the expression, if it has any, otherwise, `Nothing`
getExprLVName :: Annot Expr a -> Maybe Text
getExprLVName =
  \case
    LVExpr (LVName n `Annot` _) `Annot` _ -> Just $ getName n
    ParExpr expr `Annot` _ -> getExprLVName expr
    Annot {} -> Nothing

-- | Returns `Just` the target of the given trivial goto, otherwise `Nothing`
class GetTrivialGotoTarget n where
  getTrivialGotoTarget :: n -> Maybe Text

instance GetTrivialGotoTarget (n a) => GetTrivialGotoTarget (Annot n a) where
  getTrivialGotoTarget =
    \case
      Annot n _ -> getTrivialGotoTarget n

instance GetTrivialGotoTarget (Arm a) where
  getTrivialGotoTarget =
    \case
      Arm _ body -> getTrivialGotoTarget body

instance GetTrivialGotoTarget (BodyItem a) where
  getTrivialGotoTarget =
    \case
      BodyStmt stmt -> getTrivialGotoTarget stmt
      _ -> Nothing

instance GetTrivialGotoTarget (Body a) where
  getTrivialGotoTarget =
    \case
      Body [bodyItem] -> getTrivialGotoTarget bodyItem
      _ -> Nothing

instance GetTrivialGotoTarget (Stmt a) where
  getTrivialGotoTarget =
    \case
      GotoStmt expr _ -> getExprLVName expr
      _ -> Nothing

-- | Adds a `TopLevel` definition to the given `Unit`
addTopLevel :: Annot TopLevel a -> Annot Unit a -> Annot Unit a
addTopLevel topLevel (Unit topLevels `Annot` a) =
  withAnnot a . Unit $ topLevel : topLevels

-- | Adds multiple `TopLevel` definition to the given `Unit`
addTopLevels :: [Annot TopLevel a] -> Annot Unit a -> Annot Unit a
addTopLevels topLevels unit = foldl' (flip addTopLevel) unit topLevels

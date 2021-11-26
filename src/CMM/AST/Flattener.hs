{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module CMM.AST.Flattener where

import safe Control.Applicative (Applicative(liftA2))
import safe Control.Monad.State.Lazy (MonadState(get, put), evalState)
import safe Data.Text (Text)
import safe qualified Data.Text as T

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.Utils

class Flatten n where
  flatten :: n a -> n a

class Functor n =>
      FlattenTrivial n


helperName :: String -> Name a
helperName = Name . addPrefix flattenerPrefix . T.pack

flattenerPrefix :: Text
flattenerPrefix = "F"

type FlattenAnnot n = Flatten (Annot n)

type FlattenAnnot1 n = FlattenAnnot n

type FlattenAnnot2 n1 n2 = (FlattenAnnot n1, FlattenAnnot n2)

type FlattenAnnot3 n1 n2 n3 = (FlattenAnnot2 n1 n2, FlattenAnnot n3)

instance {-# OVERLAPPING #-} (Flatten n) => Flatten (Annot n) where
  flatten (Annot n a) = Annot (flatten n) a

instance {-# OVERLAPPABLE #-} FlattenTrivial n => Flatten n where
  flatten = id

instance FlattenAnnot TopLevel => Flatten Unit where
  flatten (Unit topLevels) = Unit $ flatten <$> topLevels

instance FlattenAnnot2 Procedure Section => Flatten TopLevel where
  flatten (TopSection strLit items) = TopSection strLit $ flatten <$> items
  flatten (TopProcedure procedure) = TopProcedure $ flatten procedure
  flatten (TopDecl decl) = TopDecl decl

instance FlattenAnnot3 Section Procedure Expr => Flatten Section where
  flatten (SecDecl decl) = SecDecl decl
  flatten (SecProcedure procedure) = SecProcedure $ flatten procedure
  flatten (SecDatum datum) = SecDatum datum
  flatten (SecSpan left right items) =
    SecSpan (flatten left) (flatten right) (flatten <$> items)

instance FlattenTrivial TargetDirective

instance FlattenTrivial Import

instance FlattenTrivial Export

instance FlattenTrivial Init

instance FlattenTrivial Registers

instance FlattenTrivial Size

fresh :: MonadState Int m => m Int
fresh = do
  num <- get
  put $ num + 1
  return num

class FlattenBodyItems n where
  flattenBodyItems :: MonadState Int m => [n a] -> m [Annot BodyItem a]

instance FlattenBodyItems (Annot BodyItem) => Flatten Body where
  flatten (Body bodyItems) = Body $ evalState (flattenBodyItems bodyItems) 0

instance FlattenBodyItems (Annot BodyItem) =>
         FlattenBodyItems (Annot Body) where
  flattenBodyItems [] = pure []
  flattenBodyItems (Annot (Body bodyItems) _:bodies) =
    liftA2 (++) (flattenBodyItems bodyItems) (flattenBodyItems bodies)

class FlattenStmt n where
  flattenStmt :: MonadState Int m => n a -> m [Annot BodyItem a]

instance (FlattenStmt (Annot BodyItem)) =>
         FlattenBodyItems (Annot BodyItem) where
  flattenBodyItems [] = pure []
  flattenBodyItems (decl@(Annot BodyDecl {} _):bodyItems) =
    (decl :) <$> flattenBodyItems bodyItems
  flattenBodyItems (stackDecl@(Annot BodyStackDecl {} _):bodyItems) =
    (stackDecl :) <$> flattenBodyItems bodyItems
  flattenBodyItems (stmt:bodyItems) =
    liftA2 (<>) (flattenStmt stmt) (flattenBodyItems bodyItems)

instance {-# OVERLAPPING #-} FlattenStmt (Annot Stmt) =>
                             FlattenStmt (Annot BodyItem) where
  flattenStmt (Annot (BodyStmt stmt) _) = flattenStmt stmt
  flattenStmt _ = error "Not a statement"

toBodyStmt :: Annot Stmt annot -> Annot BodyItem annot
toBodyStmt stmt@(Annot _ a) = Annot (BodyStmt stmt) a

toBody :: Annot BodyItem a -> Annot Body a
toBody bodyItem@(Annot _ a) = withAnnot a $ Body [bodyItem]

trivialGoto :: a -> Name a -> Annot Stmt a
trivialGoto a =
  withAnnot a .
  flip GotoStmt Nothing . withAnnot a . LVExpr . withAnnot a . LVName

brCond :: Annot Expr a -> Name a -> Name a -> a -> Annot Stmt a
brCond cond tName eName a =
  withAnnot a $
  IfStmt
    cond
    (toBody . toBodyStmt $ trivialGoto a tName)
    (Just . toBody . toBodyStmt $ trivialGoto a eName)

instance (FlattenBodyItems (Annot Body)) => FlattenStmt (Annot Stmt) where
  flattenStmt stmt@(Annot (LabelStmt n) a) =
    return $ toBodyStmt (trivialGoto a n) : [toBodyStmt stmt]
  flattenStmt (Annot (IfStmt cond tBody Nothing) a) = do
    num <- show <$> fresh
    let tName = helperName $ "then_" ++ num
        fName = helperName $ "fi_" ++ num
    tTransl <- flattenBodyItems [tBody]
    pure $
      toBodyStmt (brCond cond tName fName a) :
      (toBodyStmt . withAnnot a $ LabelStmt tName) :
      tTransl ++ [toBodyStmt . withAnnot a $ LabelStmt fName]
  flattenStmt (Annot (IfStmt cond tBody (Just eBody)) a) = do
    num <- show <$> fresh
    let tName = helperName $ "then_" ++ num
        eName = helperName $ "else_" ++ num
        fName = helperName $ "fi_" ++ num
    tTransl <- flattenBodyItems [tBody]
    eTransl <- flattenBodyItems [eBody]
    pure $
      toBodyStmt (brCond cond tName eName a) :
      (toBodyStmt . withAnnot a $ LabelStmt tName) :
      tTransl ++
      toBodyStmt (trivialGoto a fName) :
      (toBodyStmt . withAnnot a $ LabelStmt eName) :
      eTransl ++ [toBodyStmt . withAnnot a $ LabelStmt fName]
  flattenStmt (Annot (SwitchStmt expr arms) annot) = do
    num <- show <$> fresh
    let endName = helperName $ "switch_" ++ num ++ "_end"
        caseNames =
          helperName . (("switch_" ++ num ++ "_") ++) . show <$>
          take (length arms) [(1 :: Int) ..]
    armsTransl <-
      sequence
        [ ((toBodyStmt . withAnnot a $ LabelStmt caseName) :) .
        reverse . ((toBodyStmt $ trivialGoto annot endName) :) . reverse <$>
        flattenBodyItems [body]
        | (Annot (Arm _ body) a, caseName) <- zip arms caseNames
        ]
    let newArms =
          [ withAnnot a . Arm ranges . toBody . toBodyStmt $
          trivialGoto a caseName
          | (Annot (Arm ranges _) a, caseName) <- zip arms caseNames
          ]
    pure $
      toBodyStmt (withAnnot annot $ SwitchStmt expr newArms) :
      concat armsTransl ++ [toBodyStmt . withAnnot annot $ LabelStmt endName]
  flattenStmt (Annot (SpanStmt lExpr rExpr body) annot) = do
    bodyTransl <- flattenBodyItems [body]
    pure
      [ toBodyStmt . withAnnot annot . SpanStmt lExpr rExpr . withAnnot annot $
        Body bodyTransl
      ]
  flattenStmt (Annot EmptyStmt _) = pure []
  flattenStmt stmt = pure [toBodyStmt stmt]

instance (FlattenAnnot2 Body Formal, Flatten Name) => Flatten Procedure where
  flatten (Procedure mConv name formals body) =
    Procedure mConv (flatten name) (flatten <$> formals) (flatten body)

instance FlattenTrivial Formal

instance FlattenTrivial Actual

instance FlattenTrivial KindName

instance FlattenAnnot2 Range Body => Flatten Arm where
  flatten (Arm ranges body) = Arm (flatten <$> ranges) (flatten body)

instance FlattenTrivial Range

instance FlattenTrivial LValue

instance FlattenTrivial Flow

instance FlattenTrivial Alias

instance FlattenTrivial CallAnnot

instance FlattenTrivial Targets

instance FlattenTrivial Expr

instance FlattenTrivial Lit

instance FlattenTrivial Asserts

instance FlattenTrivial Name

instance FlattenTrivial Pragma

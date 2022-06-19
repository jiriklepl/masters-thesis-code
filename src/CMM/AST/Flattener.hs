{-# LANGUAGE Safe #-}

module CMM.AST.Flattener where

import safe Control.Applicative (Applicative(pure), liftA2)
import safe Control.Monad (Monad(return), sequence)
import safe Control.Monad.State (get, put, evalState, State)
import safe Data.Function (($), (.), flip)
import safe Data.Functor ((<$>))
import safe Data.Int (Int)
import safe Data.List ((++), concat, length, reverse, take, zip)
import safe Data.Maybe (Maybe(Just, Nothing))
import safe Data.Monoid ((<>))
import safe Data.String (String)
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe GHC.Err (error)
import safe Text.Show (Show(show))
import safe Data.Data ( Data(gmapT), Typeable )

import safe CMM.AST
  ( Arm(Arm)
  , Body(Body)
  , BodyItem(BodyDecl, BodyStackDecl, BodyStmt)
  , Expr(LVExpr)
  , LValue(LVName)
  , Name(Name)
  , Stmt(EmptyStmt, GotoStmt, IfStmt, LabelStmt, SpanStmt, SwitchStmt)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.Data.Num (Num((+)))
import safe CMM.Utils (addPrefix)
import safe CMM.Data.Generics ( (*|*) )

-- | Flattens the given AST node, more specifically nested blocks
flatten :: forall a n . (Data (n a), Typeable a) => n a -> n a
flatten = go
  where
    go :: forall d . Data d => d -> d
    go = flattenBody *|* gmapT go
    flattenBody :: Body a -> Body a
    flattenBody (Body bodyItems) = Body $ evalState (flattenBodyItems bodyItems) 0

-- | Creates a `Name` from a `String` with `flattenerPrefix`
helperName :: String -> Name a
helperName = Name . addPrefix flattenerPrefix . T.pack

flattenerPrefix :: Text
flattenerPrefix = "F"

-- | Generates a fresh integer
fresh :: State Int Int
fresh = do
  num <- get
  put $ num + 1
  return num

class FlattenBodyItems n where
  flattenBodyItems :: [n a] -> State Int [Annot BodyItem a]

instance FlattenBodyItems (Annot Body) where
  flattenBodyItems [] = pure []
  flattenBodyItems (Annot (Body bodyItems) _:bodies) =
    liftA2 (++) (flattenBodyItems bodyItems) (flattenBodyItems bodies)

class FlattenStmt n where
  flattenStmt ::  n a -> State Int [Annot BodyItem a]

instance FlattenBodyItems (Annot BodyItem) where
  flattenBodyItems [] = pure []
  flattenBodyItems (decl@(Annot BodyDecl {} _):bodyItems) =
    (decl :) <$> flattenBodyItems bodyItems
  flattenBodyItems (stackDecl@(Annot BodyStackDecl {} _):bodyItems) =
    (stackDecl :) <$> flattenBodyItems bodyItems
  flattenBodyItems (stmt:bodyItems) =
    liftA2 (<>) (flattenStmt stmt) (flattenBodyItems bodyItems)

instance FlattenStmt (Annot BodyItem) where
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

instance FlattenStmt (Annot Stmt) where
  flattenStmt stmt@(stmt' `Annot` annot) =
    case stmt' of
      LabelStmt n ->
        return $ toBodyStmt (trivialGoto annot n) : [toBodyStmt stmt]
      IfStmt cond tBody Nothing -> do
        num <- show <$> fresh
        let tName = helperName $ "then_" ++ num
            fName = helperName $ "fi_" ++ num
        tTransl <- flattenBodyItems [tBody]
        pure $
          toBodyStmt (brCond cond tName fName annot) :
          (toBodyStmt . withAnnot annot $ LabelStmt tName) :
          tTransl ++ [toBodyStmt . withAnnot annot $ LabelStmt fName]
      IfStmt cond tBody (Just eBody) -> do
        num <- show <$> fresh
        let tName = helperName $ "then_" ++ num
            eName = helperName $ "else_" ++ num
            fName = helperName $ "fi_" ++ num
        tTransl <- flattenBodyItems [tBody]
        eTransl <- flattenBodyItems [eBody]
        pure $
          toBodyStmt (brCond cond tName eName annot) :
          (toBodyStmt . withAnnot annot $ LabelStmt tName) :
          tTransl ++
          toBodyStmt (trivialGoto annot fName) :
          (toBodyStmt . withAnnot annot $ LabelStmt eName) :
          eTransl ++ [toBodyStmt . withAnnot annot $ LabelStmt fName]
      SwitchStmt expr arms -> do
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
          concat armsTransl ++
          [toBodyStmt . withAnnot annot $ LabelStmt endName]
      SpanStmt lExpr rExpr body -> do
        bodyTransl <- flattenBodyItems [body]
        pure
          [ toBodyStmt .
            withAnnot annot . SpanStmt lExpr rExpr . withAnnot annot $
            Body bodyTransl
          ]
      EmptyStmt -> pure []
      _ -> pure [toBodyStmt stmt]

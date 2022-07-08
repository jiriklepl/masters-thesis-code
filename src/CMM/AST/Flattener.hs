{-# LANGUAGE Safe #-}

module CMM.AST.Flattener where

import safe Control.Applicative (liftA2)
import safe Control.Monad.State (evalState)
import safe Data.Data (Data(gmapM), Typeable)

import safe qualified CMM.AST.Flattener.State as State
import safe CMM.AST.Flattener.State (Flattener)

import safe CMM.AST
  ( Arm(Arm)
  , Body(Body)
  , BodyItem(BodyStmt, BodyStackDecl)
  , Expr(LVExpr)
  , LValue(LVName)
  , Name(Name)
  , Stmt(EmptyStmt, GotoStmt, IfStmt, LabelStmt, SpanStmt, SwitchStmt, ReturnStmt, CallStmt), StackDecl (StackDecl), Datum (DatumLabel, Datum), Actual (Actual)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import Data.List (partition)
import Data.Generics.Aliases (extM)
import safe CMM.AST.GetName ( GetName(getName) )
import qualified Data.Set as Set
import Data.Functor ((<&>))

-- | Flattens the given AST node, more specifically nested blocks
flatten ::
     forall a n. (Data (n a), Typeable a)
  => n a
  -> n a
flatten ast = evalState (go ast) State.initFlattenerState
  where
    go ::
         forall d. Data d
      => d
      -> Flattener d
    go = gmapM go `extM` flattenBody
    flattenBody :: Body a -> Flattener (Body a)
    flattenBody (Body bodyItems) = do
      State.clearFlattener
      bodyItems' <- flattenBodyItems bodyItems
      return $ Body bodyItems'

-- | Flattens a list of nodes into a list of flat body items
class FlattenBodyItems n where
  flattenBodyItems :: [n a] -> Flattener [Annot BodyItem a]

-- | Flattens a list  of bodies into a list of flat body items
instance FlattenBodyItems (Annot Body) where
  flattenBodyItems [] = pure []
  flattenBodyItems (Annot (Body bodyItems) _:bodies) =
    liftA2 (++) (flattenBodyItems bodyItems) (flattenBodyItems bodies)

-- | Flattens a statement into a list of flat body items
class FlattenStmt n where
  flattenStmt :: n a -> Flattener [Annot BodyItem a]

-- | Returns `True` iff the body item is a `BodyStmt`
isBodyStmt :: Annot BodyItem a -> Bool
isBodyStmt (item `Annot` _) = case item of
  BodyStmt {} -> True
  _ -> False

-- | Finds resource objects, names them, and registers their respective contractors
determineContractors :: Annot BodyItem a -> Flattener (Annot BodyItem a)
determineContractors (bodyItem `Annot` annot) = withAnnot annot <$>
  case bodyItem of
    BodyStackDecl (StackDecl datums `Annot` annot') ->
      BodyStackDecl. withAnnot annot' . StackDecl <$> go [] datums
    item -> return item
    where
      go _ [] = return []
      go cache (annotated@(Annot datum a):datums) = case datum of
        DatumLabel name -> (annotated :) <$> go (getName name:cache) datums
        Datum True _ _ _ -> do
          name <- State.freshContractName
          State.registerContractors cache $ getName name
          datums' <- go [] datums
          return $ withAnnot a (DatumLabel name) : annotated : datums'
        _ -> (annotated:) <$> go [] datums

instance FlattenBodyItems (Annot BodyItem) where
  flattenBodyItems bodyItems = do
    bodyDecls' <- traverse determineContractors bodyDecls
    bodyStmts' <- traverse flattenStmt bodyStmts
    return $ bodyDecls' <> concat bodyStmts'
    where
      (bodyStmts, bodyDecls) = partition isBodyStmt bodyItems

instance FlattenStmt (Annot BodyItem) where
  flattenStmt (Annot (BodyStmt stmt) _) = flattenStmt stmt
  flattenStmt _ = error "Not a statement"

-- | Wraps a statement in a `BodyStmt`, copying its annotation
toBodyStmt :: Annot Stmt annot -> Annot BodyItem annot
toBodyStmt stmt@(Annot _ a) = Annot (BodyStmt stmt) a

-- | Wraps a body item in a `Body`, copying its annotation
toBody :: Annot BodyItem a -> Annot Body a
toBody bodyItem@(Annot _ a) = withAnnot a $ Body [bodyItem]

-- | Generates a trivial goto statement, a branch
trivialGoto :: a -> Name a -> Annot Stmt a
trivialGoto a =
  withAnnot a .
  flip GotoStmt Nothing . withAnnot a . LVExpr . withAnnot a . LVName

-- | Generates a representation for a conditional branch, using trivial gotos and an if statement
brCond :: Annot Expr a -> Name a -> Name a -> a -> Annot Stmt a
brCond cond tName eName a =
  withAnnot a $
  IfStmt
    cond
    (toBody . toBodyStmt $ trivialGoto a tName)
    (Just . toBody . toBodyStmt $ trivialGoto a eName)

-- | generates a "drop" call for each
makeContractCalls :: a -> Flattener [Annot Stmt a]
makeContractCalls annot = do
  contracts <- State.getContracts
  return $ Set.toList contracts <&> \contract ->
    makeCall "drop" contract `Annot` annot
  where
    makeCall name arg = CallStmt [] Nothing (makeExpr name) [makeArg arg] Nothing []
    makeReference name = LVName (Name name) `Annot` annot
    makeExpr name = LVExpr (makeReference name) `Annot` annot
    makeArg arg = Actual Nothing (makeExpr arg) `Annot` annot

-- | None of the following cases should be too surprising, just rewrites nested statements
--   (always if statements) into flattened representations
instance FlattenStmt (Annot Stmt) where
  flattenStmt stmt@(stmt' `Annot` annot) =
    case stmt' of
      LabelStmt n ->
        return $ toBodyStmt (trivialGoto annot n) : [toBodyStmt $ LabelStmt n `Annot` annot]
      IfStmt cond tBody Nothing -> do
        num <- State.freshBranchNum
        let tName = State.helperName $ "then_" ++ num
            fName = State.helperName $ "fi_" ++ num
        tTransl <- flattenBodyItems [tBody]
        pure $
          toBodyStmt (brCond cond tName fName annot) :
          (toBodyStmt . withAnnot annot $ LabelStmt tName) :
          tTransl ++ [toBodyStmt (trivialGoto annot fName), toBodyStmt . withAnnot annot $ LabelStmt fName]
      IfStmt cond tBody (Just eBody) -> do
        num <- State.freshBranchNum
        let tName = State.helperName $ "then_" ++ num
            eName = State.helperName $ "else_" ++ num
            fName = State.helperName $ "fi_" ++ num
        tTransl <- flattenBodyItems [tBody]
        eTransl <- flattenBodyItems [eBody]
        pure $
          toBodyStmt (brCond cond tName eName annot) :
          (toBodyStmt . withAnnot annot $ LabelStmt tName) :
          tTransl ++
          toBodyStmt (trivialGoto annot fName) :
          (toBodyStmt . withAnnot annot $ LabelStmt eName) :
          eTransl ++ [toBodyStmt (trivialGoto annot fName), toBodyStmt . withAnnot annot $ LabelStmt fName]
      SwitchStmt expr arms -> do
        num <- State.freshBranchNum
        let endName = State.helperName $ "switch_" ++ num ++ "_end"
            caseNames =
              State.helperName . (("switch_" ++ num ++ "_") ++) . show <$>
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
      ReturnStmt {} -> do
        calls' <- makeContractCalls annot
        pure $ toBodyStmt <$> calls' <> [stmt]
      _ -> pure [toBodyStmt stmt]

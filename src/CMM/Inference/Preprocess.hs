{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CMM.Inference.Preprocess where

import Data.Foldable
import Data.Function
import safe qualified Data.Kind as Kind
import safe Prelude hiding (init)

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
import safe CMM.Inference.BuiltIn
import safe CMM.Inference.State
import safe CMM.Inference.Type
import safe CMM.Parser.HasPos
import Control.Applicative
import Control.Monad

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
-- TODO: maybe split this into multiple files according to phases

class WithTypeHandle a b =>
      Preprocess n n' a b
  | n -> n' a
  , n' -> n b
  where
  preprocess :: (MonadInferPreprocessor m, HasPos a) => n -> m n'

class (WithTypeHandle a b, Functor n) =>
      PreprocessTrivial (n :: Kind.Type -> Kind.Type) a b
  where
  preprocessTrivial :: HasPos a => n a -> n b
  preprocessTrivial = (withTypeHandle NoType <$>)

instance PreprocessTrivial n a b => PreprocessTrivial (AST.Annot n) a b

instance {-# OVERLAPPABLE #-} (PreprocessTrivial n a b, Functor n) =>
                              Preprocess (AST.Annot n a) (AST.Annot n b) a b where
  preprocess = return . preprocessTrivial

withNoTypeHandleAnnot :: WithTypeHandle a b => a -> n b -> AST.Annot n b
withNoTypeHandleAnnot = withTypeHandledAnnot NoType

withTypeHandledAnnot ::
     WithTypeHandle a b => TypeHandle -> a -> n b -> AST.Annot n b
withTypeHandledAnnot = (AST.withAnnot .) . withTypeHandle

liftAnnot ::
     WithTypeHandle a b
  => (n a -> (TypeHandle, n b))
  -> AST.Annot n a
  -> AST.Annot n b
liftAnnot f (AST.Annot n a) =
  let (handle, n') = f n
   in withTypeHandledAnnot handle a n'

liftAnnotM ::
     (Monad m, WithTypeHandle a b)
  => (n a -> m (TypeHandle, n b))
  -> AST.Annot n a
  -> m (AST.Annot n b)
liftAnnotM f (AST.Annot n a) = do
  (handle, n') <- f n
  return $ withTypeHandledAnnot handle a n'

pureAnnotM ::
     (Monad m, WithTypeHandle a b, Functor n)
  => TypeHandle
  -> n a
  -> m (TypeHandle, n b)
pureAnnotM handle = return . (handle, ) . (withTypeHandle handle <$>)

instance {-# OVERLAPPABLE #-} (Preprocess n n' a b, Traversable f) => Preprocess (f n) (f n') a b where
  preprocess = traverse preprocess

preprocessConstr ::
     ( Preprocess n1 n1' a1 b1
     , MonadInferPreprocessor m
     , HasPos a1
     , WithTypeHandle a2 b2
     , WithTypeHandle a1 b1
     )
  => a2
  -> (n1' -> n2 b2)
  -> n1
  -> m (AST.Annot n2 b2)
preprocessConstr a constr content =
  withNoTypeHandleAnnot a . constr <$> preprocess content

preprocessInherit ::
     ( Preprocess (AST.Annot n1 a1) (AST.Annot n1 b1) a1 b1
     , MonadInferPreprocessor m
     , HasPos a1
     , WithTypeHandle a2 b2
     )
  => a2
  -> (AST.Annot n1 b1 -> n2 b2)
  -> AST.Annot n1 a1
  -> m (AST.Annot n2 b2)
preprocessInherit a constr content = do
  content' <- preprocess content
  return . withTypeHandledAnnot (getTypeHandle content') a $ constr content'

instType :: MonadInferPreprocessor m => TypeHandle -> m TypeHandle
instType = undefined -- TODO: continue from here

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Unit a) (AST.Annot AST.Unit b) a b where
  preprocess (AST.Annot (AST.Unit topLevels) a) =
    preprocessConstr a AST.Unit topLevels

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.TopLevel a) (AST.Annot AST.TopLevel b) a b where
  preprocess (AST.Annot (AST.TopSection name sectionItems) a) =
    preprocessConstr a (AST.TopSection name) sectionItems
  preprocess (AST.Annot (AST.TopDecl decl) a) =
    preprocessConstr a AST.TopDecl decl
  preprocess (AST.Annot (AST.TopProcedure procedure) a) =
    preprocessConstr a AST.TopProcedure procedure

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Section a) (AST.Annot AST.Section b) a b where
  preprocess (AST.Annot (AST.SecDecl decl) a) =
    preprocessConstr a AST.SecDecl decl
  preprocess (AST.Annot (AST.SecProcedure procedure) a) =
    preprocessConstr a AST.SecProcedure procedure
  preprocess (AST.Annot (AST.SecDatum datum) a) = do
    preprocessConstr a AST.SecDatum datum
  preprocess (AST.Annot (AST.SecSpan key value sectionItems) a) = do
    (key', value') <- preprocessSpanCommon key value
    sectionItems' <- preprocess sectionItems
    return . withNoTypeHandleAnnot a $ AST.SecSpan key' value' sectionItems'

preprocessSpanCommon :: (MonadInferPreprocessor m,
 WithTypeHandle a b, HasPos a) =>
  AST.Annot AST.Expr a -> AST.Annot AST.Expr a -> m (AST.Annot AST.Expr b, AST.Annot AST.Expr b)
preprocessSpanCommon key value = do
    key' <- preprocess key
    value' <- preprocess value
    storeFact $ constExprConstraint (getTypeHandle key')
    storeFact $ linkExprConstraint (getTypeHandle value')
    storeFact $ getTypeHandle value' `subType` getTypeHandle key'
    return (key', value')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Decl a) (AST.Annot AST.Decl b) a b where
  preprocess (AST.Annot (AST.ImportDecl imports) a) =
    preprocessConstr a AST.ImportDecl imports
  preprocess (AST.Annot (AST.ExportDecl exports) a) =
    preprocessConstr a AST.ExportDecl exports
  preprocess (AST.Annot (AST.RegDecl invar registers) a) =
    preprocessConstr a (AST.RegDecl invar) registers
  preprocess (AST.Annot (AST.PragmaDecl name pragma) a) =
    preprocessConstr a (AST.PragmaDecl $ preprocessTrivial name) pragma
  preprocess (AST.Annot (AST.TargetDecl targetDirectives) a) =
    preprocessConstr a AST.TargetDecl targetDirectives
    -- the constant is typed implicitly
  preprocess (AST.Annot (AST.ConstDecl Nothing name expr) a) = do
    expr' <- preprocess expr
    storeVar (AST.getName name) (getTypeHandle expr')
    return . withNoTypeHandleAnnot a $
      AST.ConstDecl Nothing (preprocessTrivial name) expr'
    -- the constant is typed explicitly
  preprocess (AST.Annot (AST.ConstDecl (Just type') name expr) a) = do
    handle <- freshTypeHandle
    expr' <- preprocess expr
    type'' <- preprocess type'
    storeFact $ constExprType (getTypeHandle type'') handle
    storeFact $ unifyConstraint handle (getTypeHandle expr')
    storeVar (AST.getName name) handle
    return . withNoTypeHandleAnnot a $
      AST.ConstDecl (Just type'') (preprocessTrivial name) expr'
  preprocess typedef@(AST.Annot (AST.TypedefDecl type' names) _) = do
    type'' <- preprocess type'
    let handle = getTypeHandle type''
    traverse_ (`storeTVar` handle) (AST.getName <$> names)
    return $ withTypeHandle handle <$> typedef

instance WithTypeHandle a b => PreprocessTrivial AST.TargetDirective a b

instance WithTypeHandle a b => PreprocessTrivial AST.Pragma a b

instance WithTypeHandle a b => PreprocessTrivial AST.Asserts a b

instance WithTypeHandle a b => PreprocessTrivial AST.Name a b

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Import a) (AST.Annot AST.Import b) a b where
  preprocess =
    liftAnnotM $ \case
      import'@AST.Import {} -> do
        handle <- freshTypeHandle
        storeVar (AST.getName import') handle
        pureAnnotM handle import'

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Export a) (AST.Annot AST.Export b) a b where
  preprocess =
    liftAnnotM $ \case
      export@AST.Export {} -> do
        handle <- freshTypeHandle
        storeVar (AST.getName export) handle
        pureAnnotM handle export

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Type a) (AST.Annot AST.Type b) a b where
  preprocess =
    liftAnnotM $ \case
      tBits@(AST.TBits int) -> pureAnnotM (TBitsType int) tBits
      tName@(AST.TName name) -> do
        handle <- lookupTVar (AST.getName name)
        pureAnnotM handle tName

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Registers a) (AST.Annot AST.Registers b) a b where
  preprocess =
    liftAnnotM $ \(AST.Registers mKind type' nameStrLits) -> do
      type'' <- preprocess type'
      let typeType = getTypeHandle type''
      handle <-
        case mKind of
          Just kind -> do
            handle <- freshTypeHandle
            storeFact $ kindedType (AST.getName kind) typeType handle
            return handle
          Nothing -> return typeType
      let go (name, Nothing) = do
            storeVar (AST.getName name) handle
            return (withTypeHandle handle <$> name, Nothing)
          go (name, Just (AST.StrLit strLit)) = do
            handle' <- freshTypeHandle
            storeFact $ registerType strLit handle handle'
            storeVar (AST.getName name) handle'
            return (withTypeHandle handle' <$> name, Just (AST.StrLit strLit))
      nameStrLits' <- traverse go nameStrLits
      return (NoType, AST.Registers mKind type'' nameStrLits')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Procedure a) (AST.Annot AST.Procedure b) a b where
  preprocess = liftAnnotM $ \(AST.Procedure mConv name formals body) -> do
    -- TODO: consult conventions with man
    pushVars
    pushTVars
    formals' <- preprocess formals
    let formalTypes = getTypeHandle <$> formals'
    beginProc
    body' <- preprocess body
    let bodyType = getTypeHandle body'
    let argumentsType = makeTuple formalTypes
    let procedureType = makeFunction argumentsType bodyType
    let procedureSchema = forallType (freeTypeVars procedureType) procedureType
    popTVars
    popVars
    storeProc (AST.getName name) procedureSchema
    return (procedureSchema, AST.Procedure mConv (preprocessTrivial name) formals' body')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Formal a) (AST.Annot AST.Formal b) a b where
  preprocess = liftAnnotM $ \(AST.Formal mKind invar type' name) -> do
    handle <- freshTypeHandle
    type'' <- preprocess type'

    storeFact . (handle &) . (getTypeHandle type'' &) $
      maybe unifyConstraint (kindedType . AST.getName) mKind

    return (handle, AST.Formal mKind invar type'' (preprocessTrivial name))

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Body a) (AST.Annot AST.Body b) a b where
  preprocess = liftAnnotM $ \(AST.Body bodyItems) ->
    (NoType,) . AST.Body <$> preprocess bodyItems

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.BodyItem a) (AST.Annot AST.BodyItem b) a b where
  preprocess = liftAnnotM $ ((NoType,) <$>) <$> \case
    (AST.BodyDecl decl) -> AST.BodyDecl <$> preprocess decl
    (AST.BodyStackDecl stackDecl) -> AST.BodyStackDecl <$> preprocess stackDecl
    (AST.BodyStmt stmt) -> AST.BodyStmt <$> preprocess stmt

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.StackDecl a) (AST.Annot AST.StackDecl b) a b where
  preprocess = liftAnnotM $ \(AST.StackDecl datums) -> do
    datums' <- preprocess datums
    return (NoType, AST.StackDecl datums')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Stmt a) (AST.Annot AST.Stmt b) a b where
  preprocess = liftAnnotM $ \case
    AST.EmptyStmt -> pureAnnotM NoType AST.EmptyStmt
    AST.IfStmt cond thenBody mElseBody -> do
      cond' <- preprocess cond
      storeFact $ unifyConstraint BoolType (getTypeHandle cond')
      (NoType,) <$> liftA2
        (AST.IfStmt cond')
        (preprocess thenBody)
        (preprocess mElseBody)
    AST.SwitchStmt scrutinee arms -> do
      scrutinee' <- preprocess scrutinee
      let scrutineeType = getTypeHandle scrutinee'
      arms' <- preprocess arms
      let armTypes = getTypeHandle <$> arms'
      traverse_ (storeFact . subType scrutineeType) armTypes
      return (NoType, AST.SwitchStmt scrutinee' arms')
    AST.SpanStmt key value body -> do
      (key', value') <- preprocessSpanCommon key value
      body' <- preprocess body
      return (NoType, AST.SpanStmt key' value' body')
    AST.AssignStmt lvalues exprs -> do
      lvalues' <- preprocess lvalues
      exprs' <- preprocess exprs
      let exprTypes = getTypeHandle <$> exprs'
      zipWithM_
        (\lvalue exprType -> storeFact $ unifyConstraint (getTypeHandle lvalue) exprType)
        lvalues'
        exprTypes
      return (NoType, AST.AssignStmt lvalues' exprs')
    AST.PrimOpStmt{} -> undefined -- TODO: continue from here
    AST.CallStmt{} -> undefined -- TODO: continue from here
    AST.JumpStmt{} -> undefined -- TODO: continue from here
    AST.ReturnStmt mConv Nothing actuals -> do
      -- TODO: consult conventions with man
      actuals' <- preprocess actuals
      let retType = makeTuple (getTypeHandle <$> actuals')
      storeReturn retType
      return (NoType, AST.ReturnStmt mConv Nothing actuals')
    AST.ReturnStmt{} -> undefined -- TODO: continue from here
    label@AST.LabelStmt{} -> do
      storeVar (AST.getName label) LabelType
      pureAnnotM NoType label
    AST.ContStmt{} -> undefined -- TODO: continue from here
    AST.GotoStmt expr mTargets -> do -- TODO: check if cosher
      expr' <- preprocess expr
      storeFact $ classConstraint LabelClass [getTypeHandle expr']
      (NoType,) . AST.GotoStmt expr' <$> preprocess mTargets
    AST.CutToStmt{} -> undefined -- TODO: continue from here

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.KindName a) (AST.Annot AST.KindName b) a b where
  preprocess = liftAnnotM $ \(AST.KindName mKind name) -> do
    handle <- freshTypeHandle
    maybe (return ()) (storeFact . flip kindedConstraint handle . AST.getName) mKind
    return (handle, AST.KindName mKind (preprocessTrivial name))

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Arm a) (AST.Annot AST.Arm b) a b where
  preprocess = undefined -- TODO: continue from here

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Targets a) (AST.Annot AST.Targets b) a b where
  preprocess = undefined -- TODO: continue from here

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Lit a) (AST.Annot AST.Lit b) a b where
  preprocess = liftAnnotM $ \lit -> do
    handle <- freshTypeHandle
    storeFact $ constraint lit handle
    pureAnnotM handle lit
    where
      constraint AST.LitInt{} = numericConstraint
      constraint AST.LitFloat{} = realConstraint
      constraint AST.LitChar{} = characterConstraint

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Actual a) (AST.Annot AST.Actual b) a b where
  preprocess =
    liftAnnotM $ \(AST.Actual mKind expr) -> do
      expr' <- preprocess expr
      let exprType = getTypeHandle expr'
      case mKind of
        Just kind -> do
          handle <- freshTypeHandle
          storeFact $ kindedType (AST.getName kind) exprType handle
          return (handle, AST.Actual mKind expr')
        Nothing -> return (exprType, AST.Actual mKind expr')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Init a) (AST.Annot AST.Init b) a b where
  preprocess =
    liftAnnotM $ \case
      (AST.ExprInit exprs) -> do
        exprs' <- preprocess exprs
        handle <- freshTypeHandle
        let exprTypes = getTypeHandle <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . unifyConstraint handle) exprTypes
        return (handle, AST.ExprInit exprs')
      strInit@AST.StrInit {} -> pureAnnotM StringType strInit
      strInit@AST.Str16Init {} -> pureAnnotM String16Type strInit

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Datum a) (AST.Annot AST.Datum b) a b where
  preprocess =
    liftAnnotM $ \case
      datum@(AST.DatumLabel name) -> do
        let handle = AddrType NoType
        storeVar (AST.getName name) handle
        pureAnnotM handle datum
      datum@(AST.DatumAlign _) -> do
        pureAnnotM NoType datum
      (AST.Datum type' mSize mInit) -> do
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        mSize' <-
          case mSize of
            Just (AST.Annot (AST.Size (Just expr)) a') -> do
              expr' <- preprocess expr
              let exprType = getTypeHandle expr'
              storeFact $ constExprConstraint exprType
              return . Just . withTypeHandledAnnot exprType a' . AST.Size $
                Just expr'
                    -- TODO: maybe enforce sizeType being numeric
            Just (AST.Annot (AST.Size Nothing) a') -> do
              return . Just . withNoTypeHandleAnnot a' $ AST.Size Nothing
            Nothing -> return Nothing
        mInit' <-
          case mInit of
            Just init -> do
              init' <- preprocess init
              let initType = getTypeHandle init'
              storeFact $ linkExprType typeType initType
              return $ Just init'
            Nothing -> return Nothing
        return (AddrType typeType, AST.Datum type'' mSize' mInit')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.LValue a) (AST.Annot AST.LValue b) a b where
  preprocess =
    liftAnnotM $ \case
      lvName@AST.LVName {} -> do
        handle <- lookupVar (AST.getName lvName)
        pureAnnotM handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      (AST.LVRef type' expr mAsserts) -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
        storeFact $ classConstraint AddressClass [getTypeHandle expr']
        return (getTypeHandle type'', AST.LVRef type'' expr' mAsserts')

instance WithTypeHandle a b =>
         Preprocess (AST.Annot AST.Expr a) (AST.Annot AST.Expr b) a b where
  preprocess (AST.Annot (AST.ParExpr expr) a) =
    preprocessInherit a AST.ParExpr expr
  preprocess (AST.Annot (AST.LVExpr lvalue) a) =
    preprocessInherit a AST.LVExpr lvalue
  preprocess (AST.Annot (AST.BinOpExpr op left right) a) = do
    handle <- freshTypeHandle
    left' <- preprocess left
    let leftType = getTypeHandle left'
    right' <- preprocess right
    let rightType = getTypeHandle right'
    storeFact $ subType handle leftType
    storeFact $ subType handle rightType
    -- TODO: add constraint dependent on the operator
    return $ withTypeHandledAnnot handle a $ AST.BinOpExpr op left' right'
  preprocess (AST.Annot (AST.NegExpr expr) a) =
    preprocessInherit a AST.NegExpr expr
        -- TODO: add constraint dependent on the operator
  preprocess (AST.Annot (AST.ComExpr expr) a) =
    preprocessInherit a AST.ComExpr expr
        -- TODO: add constraint dependent on the operator
  preprocess (AST.Annot (AST.LitExpr lit mType) a) = do
    lit' <- preprocess lit
    let litType = getTypeHandle lit'
    mType' <-
      case mType of
        Nothing -> return Nothing
        Just type' -> do
          type'' <- preprocess type'
          let typeType = getTypeHandle type''
          storeFact $ constExprType typeType litType
          return $ Just type''
    return . withTypeHandledAnnot litType a $ AST.LitExpr lit' mType'
  preprocess (AST.Annot (AST.PrefixExpr name actuals) a) = do
    handle <- freshTypeHandle
    argType <- freshTypeHandle
    retType <- freshTypeHandle
    actuals' <- preprocess actuals
    let actualTypes = getTypeHandle <$> actuals'
        (mClassHandle, opSchema) = getNamedOperator $ AST.getName name
        tupleType = makeTuple actualTypes
    opType <- instType opSchema
    storeFact $
      unifyConstraint opType (makeFunction argType retType)
    storeFact $ subType argType tupleType
    storeFact $ subType handle retType
    case mClassHandle of
      Nothing -> return ()
      Just classHandle -> storeFact $ classConstraint classHandle actualTypes
    return . withTypeHandledAnnot handle a $
      AST.PrefixExpr (preprocessTrivial name) actuals'
  preprocess (AST.Annot (AST.InfixExpr name left right) a) = do
    handle <- freshTypeHandle
    argType <- freshTypeHandle
    retType <- freshTypeHandle
    left' <- preprocess left
    right' <- preprocess right
    let leftType = getTypeHandle left'
        rightType = getTypeHandle left'
        (mClassHandle, opSchema) = getNamedOperator $ AST.getName name
        tupleType = makeTuple [leftType, rightType]
    opType <- instType opSchema
    case mClassHandle of
      Nothing -> return ()
      Just classHandle ->
        storeFact $ classConstraint classHandle [leftType, rightType]
    storeFact $
        opType `unifyConstraint`
        makeFunction argType retType
    storeFact $ subType argType tupleType
    storeFact $ subType handle retType
    return . withTypeHandledAnnot handle a $
      AST.InfixExpr (preprocessTrivial name) left' right'

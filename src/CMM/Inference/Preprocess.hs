{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CMM.Inference.Preprocess where

import Data.Foldable
import Data.Function
import safe qualified Data.Kind as Kind
import safe Prelude hiding (init)

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.AST.HasName
import safe CMM.Inference.BuiltIn
import safe CMM.Inference.Preprocess.State
import safe CMM.Inference.Type
import safe CMM.Parser.HasPos
import Control.Applicative
import Control.Monad

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class WithTypeHandle a b =>
      Preprocess n a b
  where
  preprocess :: (MonadInferPreprocessor m, HasPos a) => n a -> m (n b)

preprocessT ::
     (Preprocess n a b, Traversable t, MonadInferPreprocessor m, HasPos a)
  => t (n a)
  -> m (t (n b))
preprocessT = traverse preprocess

class (WithTypeHandle a b, Functor n) =>
      PreprocessTrivial (n :: Kind.Type -> Kind.Type) a b
  where
  preprocessTrivial :: HasPos a => n a -> n b
  preprocessTrivial = (withTypeHandle NoType <$>)

instance PreprocessTrivial n a b => PreprocessTrivial (Annot n) a b

instance {-# OVERLAPPABLE #-} (PreprocessTrivial n a b, Functor n) =>
                              Preprocess (Annot n) a b where
  preprocess = return . preprocessTrivial

withNoTypeHandleAnnot :: WithTypeHandle a b => a -> n b -> Annot n b
withNoTypeHandleAnnot = withTypeHandledAnnot NoType

withTypeHandledAnnot ::
     WithTypeHandle a b => TypeHandle -> a -> n b -> Annot n b
withTypeHandledAnnot = (withAnnot .) . withTypeHandle

liftAnnot ::
     WithTypeHandle a b => (n a -> (TypeHandle, n b)) -> Annot n a -> Annot n b
liftAnnot f (Annot n a) =
  let (handle, n') = f n
   in withTypeHandledAnnot handle a n'

liftAnnotM ::
     (Monad m, WithTypeHandle a b)
  => (n a -> m (TypeHandle, n b))
  -> Annot n a
  -> m (Annot n b)
liftAnnotM f (Annot n a) = do
  (handle, n') <- f n
  return $ withTypeHandledAnnot handle a n'

pureAnnotM ::
     (Monad m, WithTypeHandle a b, Functor n)
  => TypeHandle
  -> n a
  -> m (TypeHandle, n b)
pureAnnotM handle = return . (handle, ) . (withTypeHandle handle <$>)

preprocessConstr ::
     (MonadInferPreprocessor m, WithTypeHandle a2 b2)
  => a2
  -> (n1' -> n2 b2)
  -> m n1'
  -> m (Annot n2 b2)
preprocessConstr a constr content = withNoTypeHandleAnnot a . constr <$> content

preprocessInherit ::
     ( Preprocess (Annot n1) a1 b1
     , MonadInferPreprocessor m
     , HasPos a1
     , WithTypeHandle a2 b2
     )
  => a2
  -> (Annot n1 b1 -> n2 b2)
  -> Annot n1 a1
  -> m (Annot n2 b2)
preprocessInherit a constr content = do
  content' <- preprocess content
  return . withTypeHandledAnnot (getTypeHandle content') a $ constr content'

instType :: MonadInferPreprocessor m => TypeHandle -> m TypeHandle
instType = undefined -- TODO: continue from here

instance WithTypeHandle a b => Preprocess (Annot Unit) a b where
  preprocess (Annot (Unit topLevels) a) =
    preprocessConstr a Unit $ traverse preprocess topLevels

instance WithTypeHandle a b => Preprocess (Annot TopLevel) a b where
  preprocess (Annot (TopSection name sectionItems) a) =
    preprocessConstr a (TopSection name) $ traverse preprocess sectionItems
  preprocess (Annot (TopDecl decl) a) =
    preprocessConstr a TopDecl $ preprocess decl
  preprocess (Annot (TopProcedure procedure) a) =
    preprocessConstr a TopProcedure $ preprocess procedure

instance WithTypeHandle a b => Preprocess (Annot Section) a b where
  preprocess (Annot (SecDecl decl) a) =
    preprocessConstr a SecDecl $ preprocess decl
  preprocess (Annot (SecProcedure procedure) a) =
    preprocessConstr a SecProcedure $ preprocess procedure
  preprocess (Annot (SecDatum datum) a) = do
    preprocessConstr a SecDatum $ preprocess datum
  preprocess (Annot (SecSpan key value sectionItems) a) = do
    (key', value') <- preprocessSpanCommon key value
    sectionItems' <- traverse preprocess sectionItems
    return . withNoTypeHandleAnnot a $ SecSpan key' value' sectionItems'

preprocessSpanCommon ::
     (MonadInferPreprocessor m, WithTypeHandle a b, HasPos a)
  => Annot Expr a
  -> Annot Expr a
  -> m (Annot Expr b, Annot Expr b)
preprocessSpanCommon key value = do
  key' <- preprocess key
  value' <- preprocess value
  storeFact $ constExprConstraint (getTypeHandle key')
  storeFact $ linkExprConstraint (getTypeHandle value')
  storeFact $ getTypeHandle value' `subType` getTypeHandle key'
  return (key', value')

instance WithTypeHandle a b => Preprocess (Annot Decl) a b where
  preprocess (Annot (ImportDecl imports) a) =
    preprocessConstr a ImportDecl $ traverse preprocess imports
  preprocess (Annot (ExportDecl exports) a) =
    preprocessConstr a ExportDecl $ traverse preprocess exports
  preprocess (Annot (RegDecl invar registers) a) =
    preprocessConstr a (RegDecl invar) $ preprocess registers
  preprocess (Annot (PragmaDecl name pragma) a) =
    preprocessConstr a (PragmaDecl $ preprocessTrivial name) $ preprocess pragma
  preprocess (Annot (TargetDecl targetDirectives) a) =
    preprocessConstr a TargetDecl $ traverse preprocess targetDirectives
    -- the constant is typed implicitly
  preprocess (Annot (ConstDecl Nothing name expr) a) = do
    expr' <- preprocess expr
    storeVar (getName name) (getTypeHandle expr')
    return . withNoTypeHandleAnnot a $
      ConstDecl Nothing (preprocessTrivial name) expr'
    -- the constant is typed explicitly
  preprocess (Annot (ConstDecl (Just type') name expr) a) = do
    handle <- freshTypeHandle
    expr' <- preprocess expr
    type'' <- preprocess type'
    storeFact $ unifyConstraint (constExprType (getTypeHandle type'')) handle
    storeFact $ unifyConstraint handle (getTypeHandle expr')
    storeVar (getName name) handle
    return . withNoTypeHandleAnnot a $
      ConstDecl (Just type'') (preprocessTrivial name) expr'
  preprocess typedef@(Annot (TypedefDecl type' names) _) = do
    type'' <- preprocess type'
    let handle = getTypeHandle type''
    traverse_ (`storeTVar` handle) (getName <$> names)
    return $ withTypeHandle handle <$> typedef

instance WithTypeHandle a b => PreprocessTrivial TargetDirective a b

instance WithTypeHandle a b => PreprocessTrivial Pragma a b

instance WithTypeHandle a b => PreprocessTrivial Asserts a b

instance WithTypeHandle a b => PreprocessTrivial Name a b

instance WithTypeHandle a b => Preprocess (Annot Import) a b where
  preprocess =
    liftAnnotM $ \case
      import'@Import {} -> do
        handle <- freshTypeHandle
        storeVar (getName import') handle
        pureAnnotM handle import'

instance WithTypeHandle a b => Preprocess (Annot Export) a b where
  preprocess =
    liftAnnotM $ \case
      export@Export {} -> do
        handle <- freshTypeHandle
        storeVar (getName export) handle
        pureAnnotM handle export

instance WithTypeHandle a b => Preprocess (Annot Type) a b where
  preprocess =
    liftAnnotM $ \case
      tBits@(TBits int) -> pureAnnotM (SimpleType $ TBitsType int) tBits
      tName@(TName name) -> do
        handle <- lookupTVar (getName name)
        pureAnnotM handle tName

instance WithTypeHandle a b => Preprocess (Annot Registers) a b where
  preprocess =
    liftAnnotM $ \(Registers mKind type' nameStrLits) -> do
      type'' <- preprocess type'
      let typeType = getTypeHandle type''
      handle <-
        case mKind of
          Just kind -> do
            return $ kindedType (getName kind) typeType
          Nothing -> return typeType
      let go (name, Nothing) = do
            storeVar (getName name) handle
            return (withTypeHandle handle <$> name, Nothing)
          go (name, Just (StrLit strLit)) = do
            handle' <- freshTypeHandle
            storeFact $ unifyConstraint (registerType strLit handle) handle'
            storeVar (getName name) handle'
            return (withTypeHandle handle' <$> name, Just (StrLit strLit))
      nameStrLits' <- traverse go nameStrLits
      return (NoType, Registers mKind type'' nameStrLits')

instance WithTypeHandle a b => Preprocess (Annot Procedure) a b where
  preprocess =
    liftAnnotM $ \(Procedure mConv name formals body)
    -- TODO: consult conventions with man
     -> do
      pushVars
      pushTVars
      formals' <- traverse preprocess formals
      let formalTypes = getTypeHandle <$> formals'
      beginProc
      body' <- preprocess body
      retType <- endProc
      let argumentsType = makeTuple formalTypes
      let procedureType = makeFunction argumentsType retType
      let procedureSchema =
            forallType (freeTypeVars procedureType) procedureType
      popTVars
      popVars
      storeProc (getName name) procedureSchema
      return
        ( procedureSchema
        , Procedure mConv (preprocessTrivial name) formals' body')

instance WithTypeHandle a b => Preprocess (Annot Formal) a b where
  preprocess =
    liftAnnotM $ \(Formal mKind invar type' name) -> do
      handle <- freshTypeHandle
      type'' <- preprocess type'
      storeFact . (handle &) . (getTypeHandle type'' &) $
        maybe unifyConstraint ((unifyConstraint .) . kindedType . getName) mKind
      storeVar (getName name) handle
      return (handle, Formal mKind invar type'' (preprocessTrivial name))

instance WithTypeHandle a b => Preprocess (Annot Body) a b where
  preprocess =
    liftAnnotM $ \(Body bodyItems) ->
      (NoType, ) . Body <$> traverse preprocess bodyItems

instance WithTypeHandle a b => Preprocess (Annot BodyItem) a b where
  preprocess =
    liftAnnotM $
    ((NoType, ) <$>) <$> \case
      (BodyDecl decl) -> BodyDecl <$> preprocess decl
      (BodyStackDecl stackDecl) -> BodyStackDecl <$> preprocess stackDecl
      (BodyStmt stmt) -> BodyStmt <$> preprocess stmt

instance WithTypeHandle a b => Preprocess (Annot StackDecl) a b where
  preprocess =
    liftAnnotM $ \(StackDecl datums) -> do
      datums' <- traverse preprocess datums
      return (NoType, StackDecl datums')

instance WithTypeHandle a b => Preprocess (Annot Stmt) a b where
  preprocess =
    liftAnnotM $ \case
      EmptyStmt -> pureAnnotM NoType EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFact $ unifyConstraint (SimpleType BoolType) (getTypeHandle cond')
        (NoType, ) <$>
          liftA2
            (IfStmt cond')
            (preprocess thenBody)
            (traverse preprocess mElseBody)
      SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        let scrutineeType = getTypeHandle scrutinee'
        arms' <- traverse preprocess arms
        let armTypes = getTypeHandle <$> arms'
        traverse_ (storeFact . subType scrutineeType) armTypes
        return (NoType, SwitchStmt scrutinee' arms')
      SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (NoType, SpanStmt key' value' body')
      AssignStmt lvalues exprs -> do
        lvalues' <- traverse preprocess lvalues
        exprs' <- traverse preprocess exprs
        let exprTypes = getTypeHandle <$> exprs'
        zipWithM_
          (\lvalue exprType ->
             storeFact $ unifyConstraint (getTypeHandle lvalue) exprType)
          lvalues'
          exprTypes
        return (NoType, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined -- TODO: continue from here
      CallStmt {} -> undefined -- TODO: continue from here
      JumpStmt {} -> undefined -- TODO: continue from here
      ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- traverse preprocess actuals
        let retType = makeTuple (getTypeHandle <$> actuals')
        storeReturn retType
        return (NoType, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined -- TODO: continue from here
      label@LabelStmt {} -> do
        storeVar (getName label) (SimpleType LabelType)
        pureAnnotM NoType label
      ContStmt {} -> undefined -- TODO: continue from here
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        storeFact $ classConstraint LabelClass [getTypeHandle expr']
        (NoType, ) . GotoStmt expr' <$> traverse preprocess mTargets
      CutToStmt {} -> undefined -- TODO: continue from here

instance WithTypeHandle a b => Preprocess (Annot KindName) a b where
  preprocess =
    liftAnnotM $ \(KindName mKind name) -> do
      handle <- freshTypeHandle
      maybe
        (return ())
        (storeFact . (`kindedConstraint` handle) . getName)
        mKind
      return (handle, KindName mKind (preprocessTrivial name))

instance WithTypeHandle a b => Preprocess (Annot Arm) a b where
  preprocess = undefined -- TODO: continue from here

instance WithTypeHandle a b => Preprocess (Annot Targets) a b where
  preprocess = undefined -- TODO: continue from here

instance WithTypeHandle a b => Preprocess (Annot Lit) a b where
  preprocess =
    liftAnnotM $ \lit -> do
      handle <- freshTypeHandle
      storeFact $ constraint lit handle
      pureAnnotM handle lit
    where
      constraint LitInt {} = numericConstraint
      constraint LitFloat {} = realConstraint
      constraint LitChar {} = characterConstraint

instance WithTypeHandle a b => Preprocess (Annot Actual) a b where
  preprocess =
    liftAnnotM $ \(Actual mKind expr) -> do
      expr' <- preprocess expr
      let exprType = getTypeHandle expr'
      case mKind of
        Just kind -> do
          return (kindedType (getName kind) exprType, Actual mKind expr')
        Nothing -> return (exprType, Actual mKind expr')

instance WithTypeHandle a b => Preprocess (Annot Init) a b where
  preprocess =
    liftAnnotM $ \case
      (ExprInit exprs) -> do
        exprs' <- traverse preprocess exprs
        handle <- freshTypeHandle
        let exprTypes = getTypeHandle <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . unifyConstraint handle) exprTypes
        return (handle, ExprInit exprs')
      strInit@StrInit {} -> pureAnnotM (SimpleType StringType) strInit
      strInit@Str16Init {} -> pureAnnotM (SimpleType String16Type) strInit

instance WithTypeHandle a b => Preprocess (Annot Datum) a b where
  preprocess =
    liftAnnotM $ \case
      datum@(DatumLabel name) -> do
        let handle = SimpleType $ AddrType NoType
        storeVar (getName name) handle
        pureAnnotM handle datum
      datum@(DatumAlign _) -> do
        pureAnnotM NoType datum
      (Datum type' mSize mInit) -> do
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        mSize' <-
          case mSize of
            Just (Annot (Size (Just expr)) a') -> do
              expr' <- preprocess expr
              let exprType = getTypeHandle expr'
              storeFact $ constExprConstraint exprType
              return . Just . withTypeHandledAnnot exprType a' . Size $
                Just expr'
                    -- TODO: maybe enforce sizeType being numeric
            Just (Annot (Size Nothing) a') -> do
              return . Just . withNoTypeHandleAnnot a' $ Size Nothing
            Nothing -> return Nothing
        mInit' <-
          case mInit of
            Just init -> do
              init' <- preprocess init
              let initType = getTypeHandle init'
              storeFact $ unifyConstraint (linkExprType typeType) initType
              return $ Just init'
            Nothing -> return Nothing
        return (SimpleType $ AddrType typeType, Datum type'' mSize' mInit')

instance WithTypeHandle a b => Preprocess (Annot LValue) a b where
  preprocess =
    liftAnnotM $ \case
      lvName@LVName {} -> do
        handle <- lookupVar (getName lvName)
        pureAnnotM handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      (LVRef type' expr mAsserts) -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
        storeFact $ classConstraint AddressClass [getTypeHandle expr']
        return (getTypeHandle type'', LVRef type'' expr' mAsserts')

instance WithTypeHandle a b => Preprocess (Annot Expr) a b where
  preprocess (Annot (ParExpr expr) a) = preprocessInherit a ParExpr expr
  preprocess (Annot (LVExpr lvalue) a) = preprocessInherit a LVExpr lvalue
  preprocess (Annot (BinOpExpr op left right) a) = do
    handle <- freshTypeHandle
    left' <- preprocess left
    let leftType = getTypeHandle left'
    right' <- preprocess right
    let rightType = getTypeHandle right'
    storeFact $ subType handle leftType
    storeFact $ subType handle rightType
    -- TODO: add constraint dependent on the operator
    return $ withTypeHandledAnnot handle a $ BinOpExpr op left' right'
  preprocess (Annot (NegExpr expr) a) = preprocessInherit a NegExpr expr
        -- TODO: add constraint dependent on the operator
  preprocess (Annot (ComExpr expr) a) = preprocessInherit a ComExpr expr
        -- TODO: add constraint dependent on the operator
  preprocess (Annot (LitExpr lit mType) a) = do
    lit' <- preprocess lit
    let litType = getTypeHandle lit'
    mType' <-
      case mType of
        Nothing -> return Nothing
        Just type' -> do
          type'' <- preprocess type'
          let typeType = getTypeHandle type''
          storeFact $ unifyConstraint (constExprType typeType) litType
          return $ Just type''
    return . withTypeHandledAnnot litType a $ LitExpr lit' mType'
  preprocess (Annot (PrefixExpr name actuals) a) = do
    handle <- freshTypeHandle
    argType <- freshTypeHandle
    retType <- freshTypeHandle
    actuals' <- traverse preprocess actuals
    let actualTypes = getTypeHandle <$> actuals'
        (mClassHandle, opSchema) = getNamedOperator $ getName name
        tupleType = makeTuple actualTypes
    opType <- instType opSchema
    storeFact $ unifyConstraint opType (makeFunction argType retType)
    storeFact $ subType argType tupleType
    storeFact $ subType handle retType
    case mClassHandle of
      Nothing -> return ()
      Just classHandle -> storeFact $ classConstraint classHandle actualTypes
    return . withTypeHandledAnnot handle a $
      PrefixExpr (preprocessTrivial name) actuals'
  preprocess (Annot (InfixExpr name left right) a) = do
    handle <- freshTypeHandle
    argType <- freshTypeHandle
    retType <- freshTypeHandle
    left' <- preprocess left
    right' <- preprocess right
    let leftType = getTypeHandle left'
        rightType = getTypeHandle left'
        (mClassHandle, opSchema) = getNamedOperator $ getName name
        tupleType = makeTuple [leftType, rightType]
    opType <- instType opSchema
    case mClassHandle of
      Nothing -> return ()
      Just classHandle ->
        storeFact $ classConstraint classHandle [leftType, rightType]
    storeFact $ opType `unifyConstraint` makeFunction argType retType
    storeFact $ subType argType tupleType
    storeFact $ subType handle retType
    return . withTypeHandledAnnot handle a $
      InfixExpr (preprocessTrivial name) left' right'

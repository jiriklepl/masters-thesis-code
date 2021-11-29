{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module CMM.Inference.Preprocess where

import safe Data.Foldable
import safe Data.Traversable
import safe Data.Function
import safe Control.Monad.State.Lazy
import safe Prelude hiding (init)

import safe CMM.AST as AST
import safe CMM.AST.Maps as AST
import safe CMM.AST.Annot as AST
import safe CMM.AST.HasName as AST
import safe CMM.AST.Variables as AST
import safe CMM.Inference.BuiltIn as Infer
import safe CMM.Inference.Preprocess.State as Infer
import safe CMM.Inference.Type as Infer
import safe CMM.Parser.HasPos
import Control.Applicative
import Control.Monad

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class Preprocess n a b where
  preprocess ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => Annot n a
    -> m (Annot n b)
  preprocess (Annot n a) =
    preprocessImpl n >>= \(handle, n') ->
      return $ withTypeHandledAnnot handle a n'
  preprocessImpl ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => n a
    -> m (Infer.Type, n b)

preprocessTrivial :: (Functor n, WithTypeHandle a b) => n a -> n b
preprocessTrivial = (withTypeHandle NoType <$>)

data PreprocessHint = PreprocessHint

type instance Constraint PreprocessHint a b = (WithTypeHandle a b, HasPos a)
type instance Space PreprocessHint = Preprocess'

class Preprocess' a b n where
  preprocess' ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => n a
    -> m (n b)
instance Preprocess n a b => Preprocess' a b (Annot n) where
  preprocess' = preprocess

instance ASTmapGen PreprocessHint a b

pass :: (ASTmap PreprocessHint n a b, WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => n a
    -> m (n b)
pass = astMapM PreprocessHint preprocess'

instance {-# OVERLAPPABLE #-} ASTmap PreprocessHint n a b => Preprocess n a b where
  preprocessImpl = fmap (NoType,) . pass

preprocessT ::
     ( Preprocess n a b
     , WithTypeHandle a b
     , Traversable t
     , MonadInferPreprocessor m
     , HasPos a
     , MonadIO m
     )
  => t (Annot n a)
  -> m (t (Annot n b))
preprocessT = traverse preprocess

withTypeHandledAnnot ::
     WithTypeHandle a b => Infer.Type -> a -> n b -> Annot n b
withTypeHandledAnnot = (withAnnot .) . withTypeHandle

purePreprocess ::
     (Monad m, WithTypeHandle a b, Functor n)
  => Infer.Type
  -> n a
  -> m (Infer.Type, n b)
purePreprocess handle = return . (handle, ) . (withTypeHandle handle <$>)

instType :: MonadInferPreprocessor m => Infer.Type -> m Infer.Type
instType = undefined -- TODO: continue from here, store facts

instance Preprocess Unit a b where
  preprocessImpl (Unit topLevels) = (NoType, ) . Unit <$> preprocessT topLevels

instance Preprocess Section a b where
  preprocessImpl =
    ((NoType, ) <$>) . \case
      SecDecl decl -> SecDecl <$> preprocess decl
      SecProcedure procedure -> SecProcedure <$> preprocess procedure
      SecDatum datum -> SecDatum <$> preprocess datum
      SecSpan key value sectionItems -> do
        (key', value') <- preprocessSpanCommon key value
        sectionItems' <- preprocessT sectionItems
        return $ SecSpan key' value' sectionItems'

preprocessSpanCommon ::
     (MonadInferPreprocessor m, WithTypeHandle a b, HasPos a, MonadIO m)
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

instance Preprocess Decl a b where
  preprocessImpl =
    ((NoType, ) <$>) . \case
      ImportDecl imports -> ImportDecl <$> preprocessT imports
      ExportDecl exports -> ExportDecl <$> preprocessT exports
      RegDecl invar registers -> RegDecl invar <$> preprocess registers
      PragmaDecl name pragma ->
        PragmaDecl (preprocessTrivial name) <$> preprocess pragma
      TargetDecl targetDirectives -> TargetDecl <$> preprocessT targetDirectives
    -- the constant is typed implicitly
      ConstDecl Nothing name expr -> do
        expr' <- preprocess expr
        storeVar (getName name) (getTypeHandle expr')
        return $ ConstDecl Nothing (preprocessTrivial name) expr'
    -- the constant is typed explicitly
      ConstDecl (Just type') name expr -> do
        handle <- freshTypeHandle
        expr' <- preprocess expr
        type'' <- preprocess type'
        storeFact $
          constExprType (getTypeHandle type'') `unifyConstraint` handle
        storeFact $ handle `unifyConstraint` getTypeHandle expr'
        storeVar (getName name) handle
        return $ ConstDecl (Just type'') (preprocessTrivial name) expr'
      TypedefDecl type' names -> do
        type'' <- preprocess type'
        let handle = getTypeHandle type''
        traverse_ (`storeTVar` handle) (getName <$> names)
        return $ TypedefDecl type'' (preprocessTrivial <$> names)

instance Preprocess Import a b where
  preprocessImpl import'@Import {} = do
    handle <- freshTypeHandle
    storeVar (getName import') handle
    purePreprocess handle import'

instance Preprocess Export a b where
  preprocessImpl export@Export {} = do
    handle <- freshTypeHandle
    storeVar (getName export) handle
    purePreprocess handle export

instance Preprocess AST.Type a b where
  preprocessImpl =
    \case
      tBits@(TBits int) -> purePreprocess (SimpleType $ TBitsType int) tBits
      tName@(TName name) -> do
        handle <- lookupTVar (getName name)
        purePreprocess handle tName

instance Preprocess Registers a b where
  preprocessImpl (Registers mKind type' nameStrLits) = do
    type'' <- preprocess type'
    let typeType = getTypeHandle type''
    let handle = flip (maybe typeType) mKind $ \kind -> kindedType (getName kind) typeType
    let go (name, Nothing) = do
          storeVar (getName name) handle
          return (withTypeHandle handle <$> name, Nothing)
        go (name, Just (StrLit strLit)) = do
          handle' <- freshTypeHandle
          storeFact $ registerType strLit handle `unifyConstraint` handle'
          storeVar (getName name) handle'
          return (withTypeHandle handle' <$> name, Just (StrLit strLit))
    nameStrLits' <- traverse go nameStrLits
    return (NoType, Registers mKind type'' nameStrLits')

-- TODO: consult conventions with man
instance Preprocess Procedure a b
                                        where
  preprocessImpl procedure@(Procedure mConv name formals body) = do
    (vars, tVars) <- localVariables procedure
    beginProc vars tVars
    formals' <- preprocessT formals
    let formalTypes = getTypeHandle <$> formals'
    body' <- preprocess body
    retType <- endProc
    let argumentsType = makeTuple formalTypes
    let procedureType = makeFunction argumentsType retType
    let procedureScheme = forall (freeTypeVars procedureType) [] procedureType -- TODO: add context to facts
    storeProc (getName name) procedureScheme
    return
      (procedureType, Procedure mConv (preprocessTrivial name) formals' body')

instance Preprocess Formal a b where
  preprocessImpl (Formal mKind invar type' name) = do
    handle <- freshTypeHandle
    type'' <- preprocess type'
    storeFact . (handle &) . (getTypeHandle type'' &) $
      maybe unifyConstraint ((unifyConstraint .) . kindedType . getName) mKind
    storeVar (getName name) handle
    return (handle, Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess Stmt a b where
  preprocessImpl =
    \case
      EmptyStmt -> purePreprocess NoType EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFact $ SimpleType BoolType `unifyConstraint` getTypeHandle cond'
        (NoType, ) <$>
          liftA2 (IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        let scrutineeType = getTypeHandle scrutinee'
        arms' <- preprocessT arms
        let armTypes = getTypeHandle <$> arms'
        traverse_ (storeFact . subType scrutineeType) armTypes
        return (NoType, SwitchStmt scrutinee' arms')
      SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (NoType, SpanStmt key' value' body')
      AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        let exprTypes = getTypeHandle <$> exprs'
        zipWithM_
          (\lvalue exprType ->
             storeFact $ getTypeHandle lvalue `unifyConstraint` exprType)
          lvalues'
          exprTypes
        return (NoType, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined -- TODO: continue from here
      CallStmt {} -> undefined -- TODO: continue from here
      JumpStmt {} -> undefined -- TODO: continue from here
      ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- preprocessT actuals
        let retType = makeTuple (getTypeHandle <$> actuals')
        storeReturn retType
        return (NoType, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined -- TODO: continue from here
      label@LabelStmt {} -> do
        storeVar (getName label) (SimpleType LabelType)
        purePreprocess NoType label
      ContStmt {} -> undefined -- TODO: continue from here
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        storeFact $ classConstraint LabelClass [getTypeHandle expr']
        (NoType, ) . GotoStmt expr' <$> preprocessT mTargets
      CutToStmt {} -> undefined -- TODO: continue from here

instance Preprocess KindName a b where
  preprocessImpl (KindName mKind name) = do
    handle <- freshTypeHandle
    traverse_ (storeFact . (`kindedConstraint` handle) . getName) mKind
    return (handle, KindName mKind (preprocessTrivial name))

instance Preprocess Arm a b where
  preprocessImpl = undefined -- TODO: continue from here

instance Preprocess Targets a b where
  preprocessImpl = undefined -- TODO: continue from here

instance Preprocess Lit a b where
  preprocessImpl lit = do
    handle <- freshTypeHandle
    storeFact $ constraint lit handle
    purePreprocess handle lit
    where
      constraint LitInt {} = numericConstraint
      constraint LitFloat {} = realConstraint
      constraint LitChar {} = characterConstraint

instance Preprocess Actual a b where
  preprocessImpl (Actual mKind expr) = do
    expr' <- preprocess expr
    let exprType = getTypeHandle expr'
    return (flip (maybe exprType) mKind $ \kind ->
      kindedType (getName kind) exprType, Actual mKind expr')

instance Preprocess Init a b where
  preprocessImpl =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshTypeHandle
        let exprTypes = getTypeHandle <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . unifyConstraint handle) exprTypes
        return (handle, ExprInit exprs')
      strInit@StrInit {} -> purePreprocess (SimpleType StringType) strInit
      strInit@Str16Init {} -> purePreprocess (SimpleType String16Type) strInit

instance Preprocess Datum a b where
  preprocessImpl =
    \case
      datum@(DatumLabel name) -> do
        let handle = SimpleType $ AddrType NoType
        storeVar (getName name) handle
        purePreprocess handle datum
      datum@(DatumAlign _) -> do
        purePreprocess NoType datum
      Datum type' mSize mInit -> do
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        mSize' <- traverse preprocess mSize
        mInit' <- for mInit $ \init -> do
          init' <- preprocess init
          let initType = getTypeHandle init'
          storeFact $ linkExprType typeType `unifyConstraint` initType
          return init'
        return (SimpleType $ AddrType typeType, Datum type'' mSize' mInit')

instance Preprocess Size a b where
  preprocessImpl = \case
    Size (Just expr) -> do
      expr' <- preprocess expr
      let exprType = getTypeHandle expr'
      storeFact $ constExprConstraint exprType
      return (exprType, Size $ Just expr')
    size -> purePreprocess NoType size

instance Preprocess LValue a b where
  preprocessImpl =
    \case
      lvName@LVName {} -> do
        handle <- lookupVar (getName lvName)
        purePreprocess handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      LVRef type' expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
        storeFact $ classConstraint AddressClass [getTypeHandle expr']
        return (getTypeHandle type'', LVRef type'' expr' mAsserts')

instance Preprocess Expr a b where
  preprocessImpl =
    \case
      ParExpr expr -> ParExpr `preprocessInherit` expr
      LVExpr lvalue -> LVExpr `preprocessInherit` lvalue
      BinOpExpr op left right -> do
        handle <- freshTypeHandle
        left' <- preprocess left
        let leftType = getTypeHandle left'
        right' <- preprocess right
        let rightType = getTypeHandle right'
        storeFact $ subType handle leftType
        storeFact $ subType handle rightType
      -- TODO: add constraint dependent on the operator
        return (handle, BinOpExpr op left' right')
      NegExpr expr -> NegExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      ComExpr expr -> ComExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      LitExpr lit mType -> do
        lit' <- preprocess lit
        let litType = getTypeHandle lit'
        mType' <- for mType $ \type' -> do
          type'' <- preprocess type'
          let typeType = getTypeHandle type''
          storeFact $ constExprType typeType `unifyConstraint` litType
          return type''
        return (litType, LitExpr lit' mType')
      PrefixExpr name actuals -> do
        handle <- freshTypeHandle
        argType <- freshTypeHandle
        retType <- freshTypeHandle
        actuals' <- preprocessT actuals
        let actualTypes = getTypeHandle <$> actuals'
            opScheme = getNamedOperator $ getName name
            tupleType = makeTuple actualTypes
        opType <- instType opScheme
        storeFact $ opType `unifyConstraint` makeFunction argType retType
        storeFact $ subType argType tupleType
        storeFact $ subType handle retType
        return (handle, PrefixExpr (preprocessTrivial name) actuals')
      InfixExpr name left right -> do
        handle <- freshTypeHandle
        argType <- freshTypeHandle
        retType <- freshTypeHandle
        left' <- preprocess left
        right' <- preprocess right
        let leftType = getTypeHandle left'
            rightType = getTypeHandle left'
            opScheme = getNamedOperator $ getName name
            tupleType = makeTuple [leftType, rightType]
        opType <- instType opScheme
        storeFact $ opType `unifyConstraint` makeFunction argType retType
        storeFact $ subType argType tupleType
        storeFact $ subType handle retType
        return (handle, InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHandle n', c n')

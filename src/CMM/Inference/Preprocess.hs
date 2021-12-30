{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: add kinds and constnesses where they make sense
-- TODO: all types of things inside procedures should be subtypes of the return type
module CMM.Inference.Preprocess where

import safe Control.Applicative (Applicative(liftA2))
import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (Field2(_2))
import safe Control.Monad.State.Lazy (MonadIO, zipWithM_)
import safe Data.Foldable (for_, traverse_)
import safe Data.Traversable (for)
import safe qualified Data.Text as T
import safe Prelude hiding (init)

import safe CMM.Data.Tuple (complSnd3, uncurry3)

import safe CMM.AST as AST
  ( Actual(..)
  , Arm
  , Conv(..)
  , Datum(..)
  , Decl(..)
  , Export(..)
  , Expr(..)
  , Formal(..)
  , Import(..)
  , Init(..)
  , KindName(..)
  , LValue(..)
  , Lit(..)
  , Op(..)
  , Procedure(..)
  , Registers(..)
  , Section(..)
  , Size(..)
  , Stmt(..)
  , StrLit(..)
  , Targets
  , Type(..)
  , Unit(..)
  )
import safe CMM.AST.Annot as AST (Annot, Annotation(Annot), withAnnot)
import safe CMM.AST.HasName as AST (HasName(getName))
import safe CMM.AST.Maps as AST (ASTmap(..), ASTmapGen, Constraint, Space)
import safe CMM.AST.Variables as AST (globalVariables, localVariables)
import safe CMM.Inference.BuiltIn as Infer
  ( addressKind
  , floatKind
  , getDataKind
  , getNamedOperator
  , integerKind
  )
import safe CMM.Inference.Preprocess.State as Infer
  ( HasTypeHandle(getTypeHandle)
  , MonadInferPreprocessor
  , WithTypeHandle(..)
  , beginProc
  , beginUnit
  , endProc
  , freshTypeHandle
  , getCurrentReturn
  , lookupFVar
  , lookupTVar
  , lookupVar
  , storeCSymbol
  , storeFact
  , storeProc
  , storeTVar
  , storeVar, freshTypeHelper
  )
import safe CMM.Inference.Type as Infer
  ( Fact(SubConst, SubKind, SubType, Typing)
  , Type(AddrType, BoolType, LabelType, String16Type, StringType,
     TBitsType, VarType)
  , TypeKind(Star)
  , TypeVar(NoType)
  , constExprConstraint
  , instType
  , maxKindConstraint
  , minKindConstraint
  , linkExprConstraint
  , makeFunction
  , makeTuple
  , registerConstraint
  , regularExprConstraint
  , subType
  , typeConstraint
  , typeUnion
  )
import safe CMM.Parser.HasPos (HasPos)

-- TODO: check everywhere whether propagating types correctly (via subtyping)
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
    -> m (TypeVar, n b)

preprocessTrivial :: (Functor n, WithTypeHandle a b) => n a -> n b
preprocessTrivial = (withTypeHandle NoType <$>)

data PreprocessHint =
  PreprocessHint

type instance Constraint PreprocessHint a b =
     (WithTypeHandle a b, HasPos a)

type instance Space PreprocessHint = Preprocess'

class Preprocess' a b n where
  preprocess' ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => n a
    -> m (n b)

instance Preprocess n a b => Preprocess' a b (Annot n) where
  preprocess' = preprocess

instance ASTmapGen PreprocessHint a b

pass ::
     ( ASTmap PreprocessHint n a b
     , WithTypeHandle a b
     , MonadInferPreprocessor m
     , HasPos a
     , MonadIO m
     )
  => n a
  -> m (n b)
pass = astMapM PreprocessHint preprocess'

instance {-# OVERLAPPABLE #-} ASTmap PreprocessHint n a b =>
                              Preprocess n a b where
  preprocessImpl = fmap (NoType, ) . pass

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

withTypeHandledAnnot :: WithTypeHandle a b => TypeVar -> a -> n b -> Annot n b
withTypeHandledAnnot = (withAnnot .) . withTypeHandle

-- TODO: create a better name
purePreprocess ::
     (Monad m, WithTypeHandle a b, Functor n)
  => TypeVar
  -> n a
  -> m (TypeVar, n b)
purePreprocess handle = return . (handle, ) . (withTypeHandle handle <$>)

instance Preprocess Unit a b where
  preprocessImpl unit@(Unit topLevels) = do
    globalVariables unit >>= uncurry3 beginUnit
    (NoType, ) . Unit <$> preprocessT topLevels

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
        storeVar (getName name) (VarType $ getTypeHandle expr')
        return $ ConstDecl Nothing (preprocessTrivial name) expr'
      -- the constant is typed explicitly
      ConstDecl (Just type') name expr -> do
        handle <- lookupVar (getName name)
        expr' <- preprocess expr
        type'' <- preprocess type'
        storeFact $ getTypeHandle type'' `subType` handle
        storeFact $ constExprConstraint handle
        storeFact $ handle `subType` getTypeHandle expr'
        return $ ConstDecl (Just type'') (preprocessTrivial name) expr'
      TypedefDecl type' names -> do
        type'' <- preprocess type'
        let handle = VarType $ getTypeHandle type''
        traverse_ (`storeTVar` handle) (getName <$> names)
        return $ TypedefDecl type'' (preprocessTrivial <$> names)

instance Preprocess Import a b where
  preprocessImpl import'@Import {} = do
    handle <- lookupVar (getName import')
    purePreprocess handle import'

instance Preprocess Export a b where
  preprocessImpl export@Export {} = do
    handle <- lookupVar (getName export)
    purePreprocess handle export

instance Preprocess AST.Type a b where
  preprocessImpl =
    \case
      tBits@(TBits int) -> do
        handle <- freshTypeHelper Star
        storeFact $ handle `typeUnion` TBitsType int
        purePreprocess handle tBits
      tName@(TName name) -> do
        handle <- lookupTVar (getName name)
        purePreprocess handle tName

instance Preprocess Registers a b where
  preprocessImpl (Registers mKind type' nameStrLits) = do
    type'' <- preprocess type'
    let typeType = getTypeHandle type''
        setType handle = do
          storeFact $ typeType `subType` handle
          for_ mKind $
            storeFact . (`minKindConstraint` handle) . getDataKind . getName
        go name mStrLit = do
          handle <- lookupVar (getName name)
          setType handle
          case mStrLit of
            Nothing -> return (withTypeHandle handle <$> name, Nothing)
            Just (StrLit strLit) -> do
              storeFact $ registerConstraint strLit handle
              return (withTypeHandle handle <$> name, Just (StrLit strLit))
    nameStrLits' <- traverse (uncurry go) nameStrLits
    return (NoType, Registers mKind type'' nameStrLits')

-- TODO: consult conventions with man
instance Preprocess Procedure a b where
  preprocessImpl procedure@(Procedure mConv name formals body) = do
    localVariables procedure >>= uncurry beginProc . complSnd3
    formals' <- preprocessT formals
    let formalTypes = getTypeHandle <$> formals'
    body' <- preprocess body
    case mConv of
      Just (Foreign (StrLit conv))
        | conv == "C" -> do
          retType <- getCurrentReturn
          storeFact $ regularExprConstraint retType
          traverse_ (storeFact . regularExprConstraint) formalTypes
          storeCSymbol $ getName name
        | otherwise -> undefined
      Nothing -> return ()
    (fs, retType) <- (_2 %~ VarType) <$> endProc
    let argumentsType = makeTuple $ VarType <$> formalTypes
    let procedureType = makeFunction argumentsType retType
    storeProc (getName name) fs procedureType
    return (NoType, Procedure mConv (preprocessTrivial name) formals' body')

instance Preprocess Formal a b where
  preprocessImpl (Formal mKind invar type' name) = do
    handle <- lookupVar (getName name)
    type'' <- preprocess type'
    storeFact $ getTypeHandle type'' `subType` handle
    for_ mKind $ storeFact . (`minKindConstraint` handle) . getDataKind . getName
    return (handle, Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess Stmt a b where
  preprocessImpl =
    \case
      EmptyStmt -> purePreprocess NoType EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFact $ getTypeHandle cond' `typeConstraint` BoolType
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
             storeFact $ getTypeHandle lvalue `subType` exprType)
          lvalues'
          exprTypes
        return (NoType, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined
      (CallStmt names mConv expr actuals mTargets annots) -- TODO: this is just a placeholder
       -> do
        retTypes <- traverse (\name -> freshTypeHandle Star (getName name) name) names
        argTypes <- traverse (\(num, actual) -> freshTypeHandle Star (T.pack $ "actual" <> show num) actual) (zip [0..] actuals)
        names' <- preprocessT names
        expr' <- preprocess expr
        actuals' <- preprocessT actuals
        mTargets' <- preprocessT mTargets
        annots' <- preprocessT annots
        storeFact $
          getTypeHandle expr' `typeUnion`
          makeFunction
            (makeTuple (VarType <$> argTypes))
            (makeTuple (VarType <$> retTypes))
        traverse_ storeFact $
          zipWith subType (getTypeHandle <$> names') retTypes
        traverse_ storeFact $
          zipWith subType argTypes (getTypeHandle <$> actuals')
        return (NoType, CallStmt names' mConv expr' actuals' mTargets' annots')
      JumpStmt {} -> undefined
      ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- preprocessT actuals
        let retType = makeTuple (VarType . getTypeHandle <$> actuals')
        handle <- freshTypeHelper Star
        storeFact $ handle `typeUnion` retType
        getCurrentReturn >>= storeFact . (`subType` handle)
        return (NoType, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined
      label@LabelStmt {} -> do
        handle <- lookupVar (getName label)
        storeFact $ addressKind `minKindConstraint` handle -- TODO: maybe add the constexpr constraint
        storeFact $ handle `typeConstraint` LabelType
        purePreprocess NoType label
      ContStmt {} -> undefined
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        let exprType = getTypeHandle expr'
        storeFact $ addressKind `minKindConstraint` exprType
        storeFact $ exprType `typeConstraint` LabelType
        (NoType, ) . GotoStmt expr' <$> preprocessT mTargets
      CutToStmt {} -> undefined

-- TODO: this seems wrong
instance Preprocess KindName a b where
  preprocessImpl (KindName mKind name) = do
    handle <- freshTypeHelper Star
    traverse_
      (storeFact . (`minKindConstraint` handle) . getDataKind . getName)
      mKind
    return (handle, KindName mKind (preprocessTrivial name))

instance Preprocess Arm a b where
  preprocessImpl = undefined

instance Preprocess Targets a b where
  preprocessImpl = undefined

instance Preprocess Lit a b where
  preprocessImpl lit = do
    handle <- freshTypeHelper Star
    case lit of
      LitInt {} -> storeFact $ integerKind `minKindConstraint` handle
      LitFloat {} -> storeFact $ floatKind `minKindConstraint` handle
      LitChar {} -> storeFact $ integerKind `minKindConstraint` handle -- TODO: check this one? but probably correctus
    storeFact $ constExprConstraint handle
    purePreprocess handle lit

instance Preprocess Actual a b where
  preprocessImpl (Actual mKind expr) = do
    expr' <- preprocess expr
    let exprType = getTypeHandle expr'
    for_ mKind $ \kind ->
      storeFact $ (getDataKind . getName) kind `minKindConstraint` exprType
    return (exprType, Actual mKind expr')

instance Preprocess Init a b where
  preprocessImpl =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshTypeHelper Star
        let exprTypes = getTypeHandle <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . subType handle) exprTypes
        return (handle, ExprInit exprs')
      strInit@StrInit {} -> strInitCommon StringType strInit
      strInit@Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- freshTypeHelper Star
        storeFact $ handle `typeConstraint` c
        purePreprocess handle strInit

instance Preprocess Datum a b where
  preprocessImpl =
    \case
      datum@(DatumLabel name) -> do
        handle <- lookupVar (getName name)
        storeFact $ addressKind `minKindConstraint` handle
        storeFact $ handle `typeConstraint` AddrType (VarType NoType)
        purePreprocess handle datum
      datum@(DatumAlign _) -> do
        purePreprocess NoType datum
      Datum type' mSize mInit -> do
        handle <- freshTypeHelper Star
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        mSize' <- traverse preprocess mSize
        mInit' <-
          for mInit $ \init -> do
            init' <- preprocess init
            let initType = getTypeHandle init'
            storeFact $ typeType `subType` initType
            storeFact $ linkExprConstraint initType
            storeFact $ addressKind `minKindConstraint` handle
            storeFact $ handle `typeConstraint` AddrType (VarType typeType)
            return init'
        return (handle, Datum type'' mSize' mInit')

instance Preprocess Size a b where
  preprocessImpl =
    \case
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
        lookupFVar (getName lvName) >>= \case
          NoType -> lookupVar (getName lvName) >>= (`purePreprocess` lvName)
          scheme -> do
            handle <- freshTypeHelper Star
            storeFact $ scheme `instType` handle
            purePreprocess handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      LVRef type' expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
        storeFact $ addressKind `minKindConstraint` getTypeHandle expr'
        return (getTypeHandle type'', LVRef type'' expr' mAsserts')

instance Preprocess Expr a b where
  preprocessImpl =
    \case
      ParExpr expr -> ParExpr `preprocessInherit` expr
      LVExpr lvalue -> LVExpr `preprocessInherit` lvalue
      BinOpExpr op left right -- TODO: implement correctly, this is just a placeholder
       -> do
        handle <- freshTypeHelper Star
        left' <- preprocess left
        let leftType = getTypeHandle left'
        right' <- preprocess right
        let rightType = getTypeHandle right'
        storeFact $ SubKind leftType rightType
        storeFact $ SubKind rightType leftType
        if op `elem` [EqOp, NeqOp, GtOp, LtOp, GeOp, LeOp]
          then do
            storeFact $ Typing leftType (VarType rightType)
            storeFact $ SubConst handle leftType
            storeFact $ SubConst handle rightType
            storeFact $ Typing handle BoolType
          else do
            storeFact $ SubType handle leftType
            storeFact $ SubType handle rightType
      -- TODO: add constraint dependent on the operator
        return (handle, BinOpExpr op left' right')
      NegExpr expr -> NegExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      ComExpr expr -> ComExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      LitExpr lit mType -> do
        lit' <- preprocess lit
        let litType = getTypeHandle lit'
        mType' <-
          for mType $ \type' -> do
            type'' <- preprocess type'
            let typeType = getTypeHandle type''
            storeFact $ typeType `subType` litType
            storeFact $ constExprConstraint litType
            return type''
        return (litType, LitExpr lit' mType')
      PrefixExpr name actuals -> do
        handle <- freshTypeHelper Star
        tupleType <- freshTypeHelper Star
        argType <- freshTypeHelper Star
        retType <- freshTypeHelper Star
        fType <- freshTypeHelper Star
        opScheme <- freshTypeHelper Star
        actuals' <- preprocessT actuals
        storeFact $
          tupleType `typeUnion` makeTuple (VarType . getTypeHandle <$> actuals')
        storeFact $
          fType `typeUnion` makeFunction (VarType argType) (VarType retType)
        storeFact $ opScheme `typeUnion` getNamedOperator (getName name)
        storeFact $ opScheme `instType` fType
        storeFact $ argType `subType` tupleType
        storeFact $ handle `subType` retType
        return (handle, PrefixExpr (preprocessTrivial name) actuals')
      InfixExpr name left right -> do
        handle <- freshTypeHelper Star
        tupleType <- freshTypeHelper Star
        argType <- freshTypeHelper Star
        retType <- freshTypeHelper Star
        fType <- freshTypeHelper Star
        opScheme <- freshTypeHelper Star
        left' <- preprocess left
        right' <- preprocess right
        let leftType = VarType $ getTypeHandle left'
            rightType = VarType $ getTypeHandle left'
        storeFact $ tupleType `typeUnion` makeTuple [leftType, rightType]
        storeFact $
          fType `typeUnion` makeFunction (VarType argType) (VarType retType)
        storeFact $ opScheme `typeUnion` getNamedOperator (getName name)
        storeFact $ opScheme `instType` fType
        storeFact $ argType `subType` tupleType
        storeFact $ handle `subType` retType
        return (handle, InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHandle n', c n')

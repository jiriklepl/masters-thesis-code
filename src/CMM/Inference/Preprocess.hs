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
import safe Control.Monad ((>=>))
import safe Control.Monad.State.Lazy (MonadIO, zipWithM_)
import safe Data.Foldable (for_, traverse_)
import safe Data.Functor ((<&>))
import safe qualified Data.Map as Map
import safe Data.Maybe (fromJust)
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (for)
import safe Prelude hiding (init)

import safe CMM.AST as AST
  ( Actual(..)
  , Arm
  , Class(Class)
  , Conv(..)
  , Datum(..)
  , Decl(..)
  , Export(..)
  , Expr(..)
  , Formal(..)
  , Import(..)
  , Init(..)
  , Instance(Instance)
  , KindName(..)
  , LValue(..)
  , Lit(..)
  , Name
  , Op(..)
  , ParaName(ParaName)
  , ParaType(..)
  , Procedure(..)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(..)
  , Registers(..)
  , Section(..)
  , Size(..)
  , Stmt(..)
  , StrLit(..)
  , Targets
  , Type(..)
  , Unit(..)
  )
import safe CMM.AST.Annot as AST
  ( Annot
  , Annotation(Annot)
  , takeAnnot
  , unAnnot
  , withAnnot
  )
import safe CMM.AST.HasName as AST (HasName(getName))
import safe CMM.AST.Maps as AST (ASTmap(..), ASTmapGen, Constraint, Space)
import safe CMM.AST.Variables as AST
  ( classVariables
  , globalVariables
  , instanceVariables
  , localVariables
  )
import safe CMM.Inference.BuiltIn as Infer
  ( addressKind
  , boolKind
  , builtInTypeFacts
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
  , freshASTTypeHandle
  , freshNamedTypeHandle
  , freshTypeHelper
  , getCurrentReturn
  , getTypeHandleId
  , lookupClass
  , lookupFIVar
  , lookupFVar
  , lookupTCon
  , lookupTVar
  , lookupVar
  , nextHandleCounter
  , popContext
  , popTypeVariables
  , pushClass
  , pushFacts
  , pushInstance
  , pushTypeVariables
  , storeCSymbol
  , storeFact
  , storeProc
  , storeTCon
  , storeVar
  )
import safe CMM.Inference.Type as Infer
  ( NestedFact(Fact)
  , ToType(toType)
  , Type(ComplType, VarType)
  , TypeCompl(AddrType, AppType, BoolType, LabelType, String16Type,
          StringType, TBitsType, TupleType)
  , TypeKind(Constraint, GenericType, Star)
  , TypeVar(NoType, tVarId)
  , constExprConstraint
  , functionKind
  , instType
  , kindConstraint
  , linkExprConstraint
  , makeFunction
  , makeTuple
  , minKindConstraint
  , registerConstraint
  , regularExprConstraint
  , subConst
  , subType
  , typeConstraint
  , typeUnion
  , unstorableConstraint
  )
import safe CMM.Inference.TypeHandle (TypeHandle, emptyTypeHandle, handleId)
import safe CMM.Parser.HasPos (HasPos)

-- TODO: check everywhere whether propagating types correctly (via subtyping)
-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class Preprocess n a b where
  preprocess ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => Annot n a
    -> m (Annot n b)
  preprocess (Annot n a) = preprocessFinalize a $ preprocessImpl a n
  preprocessImpl ::
       (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => a
    -> n a
    -> m (TypeHandle, n b)

preprocessTrivial :: (Functor n, WithTypeHandle a b) => n a -> n b
preprocessTrivial = (withTypeHandle emptyTypeHandle <$>)

preprocessFinalize ::
     (Monad m, WithTypeHandle a b) => a -> m (TypeHandle, n b) -> m (Annot n b)
preprocessFinalize a preprocessed =
  preprocessed >>= \(handle, n') -> return $ withTypeHandledAnnot handle a n'

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

instance (WithTypeHandle a b, HasPos a) => Preprocess' a b Name where
  preprocess' = return . preprocessTrivial

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
  preprocessImpl _ = fmap (emptyTypeHandle, ) . pass

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
     WithTypeHandle a b => TypeHandle -> a -> n b -> Annot n b
withTypeHandledAnnot = (withAnnot .) . withTypeHandle

-- TODO: create a better name
purePreprocess ::
     (Monad m, WithTypeHandle a b, Functor n)
  => TypeHandle
  -> n a
  -> m (TypeHandle, n b)
purePreprocess handle = return . (handle, ) . (withTypeHandle handle <$>)

handleVars :: (Functor f, HasTypeHandle a) => f a -> f TypeVar
handleVars types = handleId . getTypeHandle <$> types

instance Preprocess Unit a b where
  preprocessImpl _ unit@(Unit topLevels) = do
    (vars, fVars, fIVars, tCons, _, tClasses, sMems) <- globalVariables unit
    beginUnit vars fVars fIVars tCons tClasses sMems
    traverse_ (storeFact . Fact) builtInTypeFacts
    let storeFacts var = do
          storeFact $ constExprConstraint var
          storeFact $ functionKind (tVarId var) var -- TODO: think this through
    for_ (Map.keys fIVars) $ lookupFIVar >=> storeFacts . handleId
    for_ (Map.keys fVars) $ lookupFVar >=> storeFacts . handleId
    (emptyTypeHandle, ) . Unit <$> preprocessT topLevels

instance Preprocess Section a b where
  preprocessImpl _ =
    ((emptyTypeHandle, ) <$>) . \case
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
  let keyType = getTypeHandleId key'
  value' <- preprocess value
  let valueType = getTypeHandleId key'
  storeFact $ constExprConstraint keyType
  storeFact $ linkExprConstraint valueType
  storeFact $ valueType `subType` keyType
  return (key', value')

instance Preprocess Decl a b where
  preprocessImpl _ =
    ((emptyTypeHandle, ) <$>) . \case
      ImportDecl imports -> ImportDecl <$> preprocessT imports
      ExportDecl exports -> ExportDecl <$> preprocessT exports
      RegDecl invar registers -> RegDecl invar <$> preprocess registers
      PragmaDecl name pragma ->
        PragmaDecl (preprocessTrivial name) <$> preprocess pragma
      TargetDecl targetDirectives -> TargetDecl <$> preprocessT targetDirectives
      -- the constant is typed implicitly
      ConstDecl Nothing name expr -> do
        expr' <- preprocess expr
        storeVar (getName name) (VarType $ getTypeHandleId expr')
        return $ ConstDecl Nothing (preprocessTrivial name) expr'
      -- the constant is typed explicitly
      ConstDecl (Just type') name expr -> do
        handle <- lookupVar (getName name)
        expr' <- preprocess expr
        type'' <- preprocess type'
        let typeType = getTypeHandleId type''
        storeFact $ typeType `subType` handleId handle
        storeFact $ constExprConstraint $ handleId handle
        storeFact $ handleId handle `subType` getTypeHandleId expr'
        return $ ConstDecl (Just type'') (preprocessTrivial name) expr'
      TypedefDecl type' names -> do
        type'' <- preprocess type'
        let handle = VarType $ getTypeHandleId type''
        traverse_ (`storeTCon` handle) (getName <$> names)
        return $ TypedefDecl type'' (preprocessTrivial <$> names)

instance Preprocess Class a b where
  preprocessImpl _ class'@(Class paraNames paraName methods) = do
    (_, _, _, _, tVars, _, _) <- classVariables class'
    pushTypeVariables tVars
    paraNames' <- preprocessT paraNames
    paraName' <- preprocess paraName
    pushClass (nameAndHandle paraName') (nameAndHandle <$> paraNames')
    (emptyTypeHandle, ) . Class paraNames' paraName' <$> preprocessT methods <*
      popContext <*
      popTypeVariables

instance Preprocess Instance a b where
  preprocessImpl _ instance'@(Instance paraNames paraName methods) = do
    (_, _, _, _, tVars, _, _) <- instanceVariables instance'
    pushTypeVariables tVars
    pushFacts
    paraNames' <- preprocessT paraNames
    paraName' <- preprocess paraName
    pushInstance (nameAndHandle paraName') (nameAndHandle <$> paraNames')
    (emptyTypeHandle, ) . Instance paraNames' paraName' <$> preprocessT methods <*
      popContext <*
      popTypeVariables

nameAndHandle :: (HasName a, HasTypeHandle a) => a -> (Text, TypeHandle)
nameAndHandle node = (getName node, getTypeHandle node)

class Preprocess param a b =>
      PreprocessParam param a b
  where
  preprocessParam ::
       (MonadInferPreprocessor m, WithTypeHandle a b, HasPos a, MonadIO m)
    => Annot param a
    -> m (Annot param b)

instance PreprocessParam Name a b where
  preprocessParam name = do
    handle <- lookupTVar $ getName name
    return $ withTypeHandle handle <$> name

instance PreprocessParam AST.Type a b where
  preprocessParam = preprocess

instance PreprocessParam param a b => Preprocess (ParaName param) a b where
  preprocessImpl annot (ParaName name params) = do
    let name' = preprocessTrivial name
    params' <- traverse preprocessParam params
    class' <- lookupClass $ getName name'
    handle <- freshNamedTypeHandle (getName name) annot Constraint
    storeFact $ handleId handle `typeUnion`
      foldl
        ((toType .) . AppType)
        (toType $ handleId class')
        (toType . getTypeHandleId <$> params')
    return (handle, ParaName name' params')

instance Preprocess Import a b where
  preprocessImpl _ import'@Import {} = do
    handle <- lookupVar (getName import')
    purePreprocess handle import'

instance Preprocess Export a b where
  preprocessImpl _ export@Export {} = do
    handle <- lookupVar (getName export)
    purePreprocess handle export

instance Preprocess AST.Type a b where
  preprocessImpl annot =
    \case
      tBits@(TBits int) -> do
        handle <- freshASTTypeHandle annot Star
        storeFact $ handleId handle `typeUnion` ComplType (TBitsType int)
        purePreprocess handle tBits
      tName@(TName name) -> do
        handle <- lookupTCon (getName name)
        purePreprocess handle tName
      tAuto@(TAuto Nothing) -> do
        handle <- freshASTTypeHandle annot Star -- TODO:, really Star?
        purePreprocess handle tAuto
      tAuto@(TAuto (Just name)) -> do
        handle <- lookupTVar (getName name)
        purePreprocess handle tAuto
      TPar parType -> do
        parType' <- preprocess parType
        return (getTypeHandle parType', TPar parType')

instance Preprocess ParaType a b where
  preprocessImpl annot (ParaType type' types') = do
    type'' <- preprocess type'
    types'' <- traverse preprocess types'
    handle <- freshASTTypeHandle annot GenericType -- TODO: determine the kind
    storeFact $ handleId handle `typeUnion`
      foldl
        ((toType .) . AppType)
        (toType $ getTypeHandleId type'')
        (toType . getTypeHandleId <$> types'')
    return (handle, ParaType type'' types'')

instance Preprocess Registers a b where
  preprocessImpl _ (Registers mKind type' nameStrLits) = do
    type'' <- preprocess type'
    let typeType = getTypeHandleId type''
        setType handle = do
          storeFact $ typeType `subType` handle
          for_ mKind $ storeFact . (`minKindConstraint` handle) . getDataKind .
            getName
        go name mStrLit = do
          handle <- lookupVar (getName name)
          setType $ handleId handle
          case mStrLit of
            Nothing -> return (withTypeHandle handle <$> name, Nothing)
            Just (StrLit strLit) -> do
              storeFact $ strLit `registerConstraint` handleId handle
              return (withTypeHandle handle <$> name, Just (StrLit strLit))
    nameStrLits' <- traverse (uncurry go) nameStrLits
    return (emptyTypeHandle, Registers mKind type'' nameStrLits')

-- TODO: consult conventions with man
preprocessProcedureHeader ::
     (WithTypeHandle a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
  => ProcedureHeader a
  -> m (TypeHandle, ProcedureHeader b)
preprocessProcedureHeader (ProcedureHeader mConv name formals mTypes) = do
  formals' <- preprocessT formals
  mTypes' <- traverse (traverse preprocess) mTypes
  let formalTypes = getTypeHandleId <$> formals'
  case mConv of
    Just (Foreign (StrLit conv))
      | conv == "C" -> do
        retType <- handleId <$> getCurrentReturn
        storeFact $ regularExprConstraint retType
        traverse_ (storeFact . regularExprConstraint) formalTypes
        storeCSymbol $ getName name
      | otherwise -> undefined
    Nothing -> return ()
  mTypes' `for_` \types -> do
    retHandle <- getCurrentReturn
    retType'@(~(TupleType retVars)) <-
      makeTuple <$>
      traverse (fmap handleId . (`freshASTTypeHandle` Star)) (fromJust mTypes)
    storeFact $ handleId retHandle `typeUnion` retType'
    zipWithM_ ((storeFact .) . subType) (handleVars types) retVars
  (fs, retType) <- (_2 %~ VarType . handleId) <$> endProc
  let argumentsType = VarType <$> formalTypes
  let procedureType = makeFunction argumentsType retType
  int <- nextHandleCounter
  storeFact $ constExprConstraint procedureType
  storeFact $ int `functionKind` procedureType
  storeProc (getName name) fs procedureType
  return
    ( emptyTypeHandle
    , ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

-- TODO: consult conventions with the man
-- TODO: add handle (dependent on the context) to the node
instance Preprocess Procedure a b where
  preprocessImpl _ procedure@(Procedure header body) = do
    (vars, _, _, tCons, tVars, _, _) <- localVariables procedure
    beginProc (getName procedure) vars tCons tVars
    body' <- preprocess body
    header' <-
      preprocessFinalize (takeAnnot header) $
      preprocessProcedureHeader (unAnnot header)
    return (emptyTypeHandle, Procedure header' body')

-- TODO: ditto
instance Preprocess ProcedureDecl a b where
  preprocessImpl _ procedure@(ProcedureDecl header) = do
    (vars, _, _, tCons, tVars, _, _) <- localVariables procedure
    beginProc (getName procedure) vars tCons tVars
    header' <-
      preprocessFinalize (takeAnnot header) $
      preprocessProcedureHeader (unAnnot header)
    return (emptyTypeHandle, ProcedureDecl header')

instance Preprocess Formal a b where
  preprocessImpl _ (Formal mKind invar type' name) = do
    handle <- lookupVar (getName name)
    type'' <- preprocess type'
    storeFact $ getTypeHandleId type'' `subType` handleId handle
    for_ mKind $ storeFact . (`minKindConstraint` handleId handle) . getDataKind .
      getName
    return (handle, Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess Stmt a b where
  preprocessImpl _ =
    \case
      EmptyStmt -> purePreprocess emptyTypeHandle EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFact $ getTypeHandleId cond' `typeConstraint` ComplType BoolType
        storeFact $ boolKind `minKindConstraint` getTypeHandleId cond'
        (emptyTypeHandle, ) <$>
          liftA2 (IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        let scrutineeType = getTypeHandleId scrutinee'
        arms' <- preprocessT arms
        let armTypes = getTypeHandleId <$> arms'
        traverse_ (storeFact . subType scrutineeType) armTypes
        return (emptyTypeHandle, SwitchStmt scrutinee' arms')
      SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (emptyTypeHandle, SpanStmt key' value' body')
      AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        let exprTypes = getTypeHandleId <$> exprs'
        zipWithM_
          (\lvalue exprType ->
             storeFact $ getTypeHandleId lvalue `subType` exprType)
          lvalues'
          exprTypes
        return (emptyTypeHandle, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined
      (CallStmt names mConv expr actuals mTargets annots) -- TODO: this is just a placeholder
       -> do
        retTypes <-
          (handleId <$>) <$>
          traverse
            (\name -> freshNamedTypeHandle (getName name) name Star)
            names
        argTypes <-
          (handleId <$>) <$>
          traverse
            (\(num, actual) ->
               freshNamedTypeHandle (T.pack $ "actual" <> show num) actual Star)
            (zip [0 :: Int ..] actuals)
        names' <- preprocessT names
        expr' <- preprocess expr
        actuals' <- preprocessT actuals
        mTargets' <- preprocessT mTargets
        annots' <- preprocessT annots
        storeFact . typeUnion (getTypeHandleId expr') . AddrType $
          makeFunction (VarType <$> argTypes) (toType $ makeTuple retTypes)
        traverse_ storeFact $
          zipWith subType (getTypeHandleId <$> names') retTypes
        traverse_ storeFact $
          zipWith subType argTypes (getTypeHandleId <$> actuals')
        return
          ( emptyTypeHandle
          , CallStmt names' mConv expr' actuals' mTargets' annots')
      JumpStmt {} -> undefined
      ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- preprocessT actuals
        retType@(~(TupleType retVars)) <-
          makeTuple <$>
          traverse ((handleId <$>) . (`freshASTTypeHandle` Star)) actuals
        zipWithM_ ((storeFact .) . subType) retVars (handleVars actuals')
        getCurrentReturn >>= storeFact . (`typeUnion` retType) . handleId
        return (emptyTypeHandle, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined
      label@LabelStmt {} -> do
        handle <- lookupVar (getName label)
        storeFact $ addressKind `kindConstraint` handleId handle
        storeFact $ handleId handle `typeConstraint` ComplType LabelType
        storeFact $ constExprConstraint $ handleId handle
        purePreprocess handle label
      ContStmt {} -> undefined
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        let exprType = handleId $ getTypeHandle expr'
        storeFact $ addressKind `minKindConstraint` exprType
        storeFact $ exprType `typeConstraint` ComplType LabelType
        (emptyTypeHandle, ) . GotoStmt expr' <$> preprocessT mTargets
      CutToStmt {} -> undefined

-- TODO: this seems wrong
instance Preprocess KindName a b where
  preprocessImpl annot (KindName mKind name) = do
    handle <- freshNamedTypeHandle (getName name) annot Star
    traverse_
      (storeFact . (`minKindConstraint` handleId handle) . getDataKind . getName)
      mKind
    return (handle, KindName mKind (preprocessTrivial name))

instance Preprocess Arm a b where
  preprocessImpl = undefined

instance Preprocess Targets a b where
  preprocessImpl = undefined

instance Preprocess Lit a b where
  preprocessImpl annot lit = do
    handle <- freshASTTypeHandle annot Star
    case lit of
      LitInt {} -> storeFact $ integerKind `minKindConstraint` handleId handle
      LitFloat {} -> storeFact $ floatKind `minKindConstraint` handleId handle
      LitChar {} -> storeFact $ integerKind `minKindConstraint` handleId handle -- TODO: check this one? but probably correctus
    storeFact $ constExprConstraint $ handleId handle
    purePreprocess handle lit

instance Preprocess Actual a b where
  preprocessImpl _ (Actual mKind expr) = do
    expr' <- preprocess expr
    let exprType = getTypeHandle expr'
    for_ mKind $ \kind ->
      storeFact $ (getDataKind . getName) kind `minKindConstraint`
      handleId exprType
    return (exprType, Actual mKind expr')

instance Preprocess Init a b where
  preprocessImpl annot =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshASTTypeHandle annot Star
        let exprTypes = getTypeHandleId <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . (`subType` handleId handle)) exprTypes
        return (handle, ExprInit exprs')
      strInit@StrInit {} -> strInitCommon StringType strInit
      strInit@Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- freshTypeHelper Star
        storeFact $ handleId handle `typeConstraint` ComplType c
        purePreprocess handle strInit

instance Preprocess Datum a b where
  preprocessImpl annot =
    \case
      datum@(DatumLabel name) -> do
        handle <- lookupVar (getName name)
        storeFact $ addressKind `kindConstraint` handleId handle
        storeFact $ handleId handle `typeConstraint`
          toType (AddrType $ VarType NoType)
        purePreprocess handle datum
      datum@(DatumAlign _) -> do
        purePreprocess emptyTypeHandle datum
      Datum type' mSize mInit -> do
        handle <- freshASTTypeHandle annot Star
        type'' <- preprocess type'
        let typeType = handleId $ getTypeHandle type''
        mSize' <- traverse preprocess mSize
        mInit' <-
          for mInit $ \init -> do
            init' <- preprocess init
            let initType = handleId $ getTypeHandle init'
            storeFact $ typeType `subType` initType
            storeFact $ linkExprConstraint initType
            storeFact $ addressKind `kindConstraint` handleId handle
            storeFact $ handleId handle `typeConstraint`
              AddrType (VarType typeType)
            return init'
        return (handle, Datum type'' mSize' mInit')

instance Preprocess Size a b where
  preprocessImpl _ =
    \case
      Size (Just expr) -> do
        expr' <- preprocess expr
        let exprType = getTypeHandle expr'
        storeFact $ constExprConstraint $ handleId exprType
        return (exprType, Size $ Just expr')
      size -> purePreprocess emptyTypeHandle size

instance Preprocess LValue a b where
  preprocessImpl annot =
    \case
      lvName@LVName {} -> do
        lookupFVar (getName lvName) <&> handleId >>= \case
          NoType -> lookupVar (getName lvName) >>= (`purePreprocess` lvName)
          scheme -> do
            inst <-
              handleId <$> freshNamedTypeHandle (getName lvName) annot Star
            handle <- freshNamedTypeHandle (getName lvName) annot Star
            let var = handleId handle
            storeFact $ scheme `instType` inst
            storeFact $ var `typeUnion` AddrType inst
            storeFact $ constExprConstraint var
            storeFact $ addressKind `kindConstraint` var
            purePreprocess handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      LVRef type' expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle emptyTypeHandle <$>) <$> mAsserts
        storeFact $ addressKind `minKindConstraint` getTypeHandleId expr'
        return (getTypeHandle type'', LVRef type'' expr' mAsserts')

instance Preprocess Expr a b where
  preprocessImpl _ -- TODO: this is just ugly
   =
    \case
      ParExpr expr -> ParExpr `preprocessInherit` expr
      LVExpr lvalue -> LVExpr `preprocessInherit` lvalue
      BinOpExpr op left right -- TODO: implement correctly, this is just a placeholder
       -> do
        handle <- freshTypeHelper Star
        operator <- freshTypeHelper Star
        left' <- preprocess left
        let leftType = handleId $ getTypeHandle left'
        right' <- preprocess right
        let rightType = handleId $ getTypeHandle right'
        storeFact $ handleId operator `typeUnion`
          makeFunction [leftType, rightType] (handleId handle)
        storeFact $ tVarId (handleId operator) `functionKind` handleId operator
        if op `elem` [EqOp, NeqOp, GtOp, LtOp, GeOp, LeOp]
          then do
            storeFact $ leftType `typeConstraint` rightType
            storeFact $ handleId handle `subConst` leftType
            storeFact $ handleId handle `subConst` rightType
            storeFact $ handleId handle `typeConstraint` ComplType BoolType
            storeFact $ boolKind `kindConstraint` handleId handle
          else do
            storeFact $ handleId handle `subType` leftType
            storeFact $ handleId handle `subType` rightType
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
            let typeType = getTypeHandleId type''
            storeFact $ typeType `subType` handleId litType
            storeFact $ constExprConstraint $ handleId litType
            return type''
        return (litType, LitExpr lit' mType')
      PrefixExpr name actuals -- TODO: fix this
       -> do
        handle <- freshTypeHelper Star
        tupleType <- handleId <$> freshTypeHelper Star
        argType <- freshTypeHelper Star
        retType <- freshTypeHelper Star
        fType <- freshTypeHelper Star
        opScheme <- freshTypeHelper Star
        actuals' <- preprocessT actuals
        storeFact $ tupleType `typeUnion`
          makeTuple (handleId . getTypeHandle <$> actuals')
        storeFact $ unstorableConstraint tupleType
        storeFact $ handleId fType `typeUnion`
          makeFunction [handleId argType] (handleId retType)
        storeFact $ handleId opScheme `typeUnion`
          getNamedOperator (getName name)
        storeFact $ handleId opScheme `instType` handleId fType
        storeFact $ handleId argType `subType` tupleType
        storeFact $ handleId handle `subType` handleId retType
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
        let leftType = handleId $ getTypeHandle left'
            rightType = handleId $ getTypeHandle left'
        storeFact $ handleId tupleType `typeUnion`
          makeTuple [leftType, rightType]
        storeFact $ handleId fType `typeUnion`
          makeFunction [handleId argType] (handleId retType)
        storeFact $ handleId opScheme `typeUnion`
          getNamedOperator (getName name)
        storeFact $ handleId opScheme `instType` handleId fType
        storeFact $ handleId argType `subType` handleId tupleType
        storeFact $ handleId handle `subType` handleId retType
        return (handle, InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHandle n', c n')

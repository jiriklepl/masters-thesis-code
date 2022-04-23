{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: add kinds and constnesses where they make sense
-- TODO: all types of things inside procedures should be subtypes of the return type
module CMM.Inference.Preprocess where

import safe Control.Applicative (Applicative((<*), pure), liftA2)
import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_2)
import safe Control.Monad (Monad((>>=), return), (>=>), zipWithM_)
import safe Control.Monad.IO.Class (MonadIO)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Foldable (for_, traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$>))
import safe Data.Int (Int)
import safe Data.List (elem, foldl, unzip, zip)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), fromJust)
import safe Data.Monoid ((<>))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (Traversable(traverse), for)
import safe Data.Tuple (uncurry)
import safe GHC.Err (undefined)
import safe Text.Show (Show(show))

import safe CMM.AST as AST
  ( Actual(Actual)
  , Arm
  , Class(Class)
  , Conv(Foreign)
  , Datum(Datum, DatumAlign, DatumLabel)
  , Decl(ConstDecl, ExportDecl, ImportDecl, PragmaDecl, RegDecl,
     TargetDecl, TypedefDecl)
  , Export(Export)
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, NegExpr,
     ParExpr, PrefixExpr)
  , Formal(Formal)
  , Import(Import)
  , Init(ExprInit, Str16Init, StrInit)
  , Instance(Instance)
  , KindName(KindName)
  , LValue(LVName, LVRef)
  , Lit(LitChar, LitFloat, LitInt)
  , Name
  , Op(EqOp, GeOp, GtOp, LeOp, LtOp, NeqOp)
  , ParaName(ParaName)
  , ParaType(ParaType)
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Registers(Registers)
  , Section(SecDatum, SecDecl, SecProcedure, SecSpan)
  , Size(Size)
  , Stmt(AssignStmt, CallStmt, ContStmt, CutToStmt, EmptyStmt,
     GotoStmt, IfStmt, JumpStmt, LabelStmt, PrimOpStmt, ReturnStmt,
     SpanStmt, SwitchStmt)
  , StrLit(StrLit)
  , Targets
  , Type(TAuto, TBits, TName, TPar)
  , Unit(Unit)
  )
import safe CMM.AST.Annot as AST
  ( Annot
  , Annotation(Annot)
  , takeAnnot
  , unAnnot
  , withAnnot
  )
import safe CMM.AST.HasName as AST (HasName(getName))
import safe CMM.AST.Maps as AST (ASTmap(astMapM), Constraint, Space)
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
import safe CMM.Inference.Fact as Infer
  ( NestedFact(Fact)
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
import safe CMM.Inference.Preprocess.HasTypeHole
  ( HasTypeHole(getTypeHole)
  , WithTypeHole(withTypeHole)
  , getTypeHoleId
  )
import safe CMM.Inference.Preprocess.State as Infer
  ( MonadInferPreprocessor
  , beginProc
  , beginUnit
  , endProc
  , freshASTTypeHandle
  , freshNamedTypeHandle
  , freshTypeHelper
  , getCurrentReturn
  , lookupClass
  , lookupFIVar
  , lookupFVar
  , lookupTCon
  , lookupTVar
  , lookupVar
  , openProc
  , popContext
  , popTopContext
  , popTypeVariables
  , pushClass
  , pushContext
  , pushInstance
  , pushTypeVariables
  , storeCSymbol
  , storeFact
  , storeProc
  , storeTCon
  , storeVar
  )
import safe CMM.Inference.Preprocess.TypeHole
  ( TypeHole(EmptyTypeHole, LVInstTypeHole, SimpleTypeHole)
  , holeId
  )
import safe CMM.Inference.Type as Infer
  ( ToType(toType)
  , Type(ComplType, VarType)
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, LabelType, String16Type,
          StringType, TBitsType, TupleType)
  )
import safe CMM.Inference.TypeHandle (handleId)
import safe CMM.Inference.TypeKind (TypeKind(Constraint, GenericType, Star))
import safe CMM.Inference.TypeVar (TypeVar(NoType, tVarId))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Inference.HandleCounter ( nextHandleCounter )

-- TODO: check everywhere whether propagating types correctly (via subtyping)
-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class Preprocess n a b where
  preprocess ::
       (WithTypeHole a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => Annot n a
    -> m (Annot n b)
  preprocess (Annot n a) = preprocessFinalize a $ preprocessImpl a n
  preprocessImpl ::
       (WithTypeHole a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => a
    -> n a
    -> m (TypeHole, n b)

preprocessTrivial :: (Functor n, WithTypeHole a b) => n a -> n b
preprocessTrivial = (withTypeHole EmptyTypeHole <$>)

preprocessFinalize ::
     (Monad m, WithTypeHole a b) => a -> m (TypeHole, n b) -> m (Annot n b)
preprocessFinalize a preprocessed =
  preprocessed >>= \(handle, n') -> return $ withTypeHoledAnnot handle a n'

data PreprocessHint =
  PreprocessHint

type instance Constraint PreprocessHint a b =
     (WithTypeHole a b, HasPos a)

type instance Space PreprocessHint = Preprocess'

class Preprocess' a b n where
  preprocess' ::
       (WithTypeHole a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
    => n a
    -> m (n b)

instance WithTypeHole a b => Preprocess' a b Name where
  preprocess' = return . preprocessTrivial

instance Preprocess n a b => Preprocess' a b (Annot n) where
  preprocess' = preprocess

pass ::
     ( ASTmap PreprocessHint n a b
     , WithTypeHole a b
     , MonadInferPreprocessor m
     , HasPos a
     , MonadIO m
     )
  => n a
  -> m (n b)
pass = astMapM PreprocessHint preprocess'

instance {-# OVERLAPPABLE #-} ASTmap PreprocessHint n a b =>
                              Preprocess n a b where
  preprocessImpl _ = fmap (EmptyTypeHole, ) . pass

preprocessT ::
     ( Preprocess n a b
     , WithTypeHole a b
     , Traversable t
     , MonadInferPreprocessor m
     , HasPos a
     , MonadIO m
     )
  => t (Annot n a)
  -> m (t (Annot n b))
preprocessT = traverse preprocess

withTypeHoledAnnot :: WithTypeHole a b => TypeHole -> a -> n b -> Annot n b
withTypeHoledAnnot = (withAnnot .) . withTypeHole

-- TODO: create a better name
purePreprocess ::
     (Monad m, WithTypeHole a b, Functor n)
  => TypeHole
  -> n a
  -> m (TypeHole, n b)
purePreprocess hole = return . (hole, ) . (withTypeHole hole <$>)

handleVars :: (Functor f, HasTypeHole a) => f a -> f TypeVar
handleVars types = holeId . getTypeHole <$> types

instance Preprocess Unit a b where
  preprocessImpl _ unit@(Unit topLevels) = do
    (vars, fVars, fIVars, tCons, _, tClasses, sMems) <- globalVariables unit
    beginUnit vars fVars fIVars tCons tClasses sMems
    traverse_ (storeFact . Fact) builtInTypeFacts
    let storeFacts var = do
          storeFact $ constExprConstraint var
          storeFact $ functionKind (tVarId var) var -- TODO: think this through
    for_ (Map.keys fIVars) $ lookupFIVar >=> storeFacts . holeId
    for_ (Map.keys fVars) $ lookupFVar >=> storeFacts . holeId
    (EmptyTypeHole, ) . Unit <$> preprocessT topLevels

instance Preprocess Section a b where
  preprocessImpl _ =
    ((EmptyTypeHole, ) <$>) . \case
      SecDecl decl -> SecDecl <$> preprocess decl
      SecProcedure procedure -> SecProcedure <$> preprocess procedure
      SecDatum datum -> SecDatum <$> preprocess datum
      SecSpan key value sectionItems -> do
        (key', value') <- preprocessSpanCommon key value
        sectionItems' <- preprocessT sectionItems
        return $ SecSpan key' value' sectionItems'

preprocessSpanCommon ::
     (MonadInferPreprocessor m, WithTypeHole a b, HasPos a, MonadIO m)
  => Annot Expr a
  -> Annot Expr a
  -> m (Annot Expr b, Annot Expr b)
preprocessSpanCommon key value = do
  key' <- preprocess key
  let keyType = getTypeHoleId key'
  value' <- preprocess value
  let valueType = getTypeHoleId value'
  storeFact $ constExprConstraint keyType
  storeFact $ linkExprConstraint valueType
  storeFact $ valueType `subType` keyType
  return (key', value')

instance Preprocess Decl a b where
  preprocessImpl _ =
    ((EmptyTypeHole, ) <$>) . \case
      ImportDecl imports -> ImportDecl <$> preprocessT imports
      ExportDecl exports -> ExportDecl <$> preprocessT exports
      RegDecl invar registers -> RegDecl invar <$> preprocess registers
      PragmaDecl name pragma ->
        PragmaDecl (preprocessTrivial name) <$> preprocess pragma
      TargetDecl targetDirectives -> TargetDecl <$> preprocessT targetDirectives
      -- the constant is typed implicitly
      ConstDecl Nothing name expr -> do
        expr' <- preprocess expr
        storeVar (getName name) (VarType $ getTypeHoleId expr')
        return $ ConstDecl Nothing (preprocessTrivial name) expr'
      -- the constant is typed explicitly
      ConstDecl (Just type') name expr -> do
        handle <- lookupVar (getName name)
        expr' <- preprocess expr
        type'' <- preprocess type'
        let typeType = getTypeHoleId type''
        storeFact $ typeType `subType` holeId handle
        storeFact $ constExprConstraint $ holeId handle
        storeFact $ holeId handle `subType` getTypeHoleId expr'
        return $ ConstDecl (Just type'') (preprocessTrivial name) expr'
      TypedefDecl type' names -> do
        type'' <- preprocess type'
        let handle = VarType $ getTypeHoleId type''
        traverse_ (`storeTCon` handle) (getName <$> names)
        return $ TypedefDecl type'' (preprocessTrivial <$> names)

-- TODO: combine with Instance
instance Preprocess Class a b where
  preprocessImpl _ class'@(Class paraNames paraName methods) = do
    (_, _, _, _, tVars, _, _) <- classVariables class'
    pushTypeVariables tVars
    (constraints, paraNames') <- unzip <$> traverse preprocessParaName paraNames
    (constraint, paraName') <- preprocessParaName paraName
    let hole = getTypeHole paraName'
    pushClass
      (getName paraName, hole)
      (getName paraName, constraint)
      (fmap getName paraNames `zip` constraints)
    (hole, ) . Class paraNames' paraName' <$> preprocessT methods <* popContext <*
      popTypeVariables

instance Preprocess Instance a b where
  preprocessImpl _ instance'@(Instance paraNames paraName methods) = do
    (_, _, _, _, tVars, _, _) <- instanceVariables instance'
    pushTypeVariables tVars
    (constraints, paraNames') <- unzip <$> traverse preprocessParaName paraNames
    (constraint, paraName') <- preprocessParaName paraName
    let hole = getTypeHole paraName'
    pushInstance
      (getName paraName, getTypeHole paraName')
      (getName paraName, constraint)
      (fmap getName paraNames `zip` constraints)
    (hole, ) . Instance paraNames' paraName' <$> preprocessT methods <*
      popContext <*
      popTypeVariables

class Preprocess param a b =>
      PreprocessParam param a b
  where
  preprocessParam ::
       (MonadInferPreprocessor m, WithTypeHole a b, HasPos a, MonadIO m)
    => Annot param a
    -> m (Annot param b)

instance PreprocessParam Name a b where
  preprocessParam name = do
    handle <- lookupTVar $ getName name
    return $ withTypeHole handle <$> name

instance PreprocessParam AST.Type a b where
  preprocessParam = preprocess

preprocessParaName ::
     ( WithTypeHole a b
     , PreprocessParam param a b
     , MonadInferPreprocessor m
     , HasPos a
     , MonadIO m
     )
  => Annot (ParaName param) a
  -> m (Infer.Type, Annot (ParaName param) b)
preprocessParaName (Annot (ParaName name params) annot) = do
  let name' = preprocessTrivial name
  params' <- traverse preprocessParam params
  class' <- lookupClass $ getName name'
  handle <- freshNamedTypeHandle (getName name) annot Constraint
  let constraint =
        foldl
          ((toType .) . AppType)
          (toType $ holeId class')
          (toType . getTypeHoleId <$> params')
  storeFact $ handleId handle `typeUnion` constraint
  return
    ( constraint
    , withTypeHole (SimpleTypeHole handle) annot `withAnnot`
      ParaName name' params')

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
        purePreprocess (SimpleTypeHole handle) tBits
      tName@(TName name) -> do
        hole <- lookupTCon (getName name)
        purePreprocess hole tName
      tAuto@(TAuto Nothing) -> do
        handle <- freshASTTypeHandle annot Star -- TODO:, really Star?
        purePreprocess (SimpleTypeHole handle) tAuto
      tAuto@(TAuto (Just name)) -> do
        hole <- lookupTVar (getName name)
        purePreprocess hole tAuto
      TPar parType -> do
        parType' <- preprocess parType
        return (getTypeHole parType', TPar parType')

instance Preprocess ParaType a b where
  preprocessImpl annot (ParaType type' types') = do
    type'' <- preprocess type'
    types'' <- traverse preprocess types'
    handle <- freshASTTypeHandle annot GenericType -- TODO: determine the kind
    storeFact $ handleId handle `typeUnion`
      foldl
        ((toType .) . AppType)
        (toType $ getTypeHoleId type'')
        (toType . getTypeHoleId <$> types'')
    return (SimpleTypeHole handle, ParaType type'' types'')

maybeKindUnif ::
     (MonadInferPreprocessor m, ToType t, ToType t', HasName n)
  => Maybe n
  -> t
  -> t'
  -> m ()
maybeKindUnif mKind derived base = do
  case mKind of
    Nothing -> storeFact $ derived `typeUnion` base
    Just kind -> do
      storeFact $ derived `typeConstraint` base
      storeFact $ derived `subConst` base
      storeFact $ base `subConst` derived
      storeFact $ getDataKind (getName kind) `kindConstraint` derived

instance Preprocess Registers a b where
  preprocessImpl _ (Registers mKind type' nameStrLits) = do
    type'' <- preprocess type'
    let typeType = getTypeHoleId type''
        go name mStrLit = do
          hole <- lookupVar (getName name)
          maybeKindUnif mKind (holeId hole) typeType
          case mStrLit of
            Nothing -> return (withTypeHole hole <$> name, Nothing)
            Just (StrLit strLit) -> do
              storeFact $ strLit `registerConstraint` holeId hole
              return (withTypeHole hole <$> name, Just (StrLit strLit))
    nameStrLits' <- traverse (uncurry go) nameStrLits
    return (EmptyTypeHole, Registers mKind type'' nameStrLits')

class FormalNames a where
  formalNames :: a -> [Text]

instance FormalNames (n a) => FormalNames (Annot n a) where
  formalNames = formalNames . unAnnot

instance FormalNames (Formal a) where
  formalNames = pure . getName

instance FormalNames (ProcedureHeader a) where
  formalNames (ProcedureHeader _ _ formals _) = getName <$> formals

instance FormalNames (ProcedureDecl a) where
  formalNames (ProcedureDecl header) = formalNames header

instance FormalNames (Procedure a) where
  formalNames (Procedure header _) = formalNames header

-- TODO: consult conventions with man
preprocessProcedureHeader ::
     (WithTypeHole a b, MonadInferPreprocessor m, HasPos a, MonadIO m)
  => ProcedureHeader a
  -> m (TypeHole, ProcedureHeader b)
preprocessProcedureHeader (ProcedureHeader mConv name formals mTypes) = do
  formals' <- doOutsideCtx $ preprocessT formals
  mTypes' <- doOutsideCtx $ traverse (traverse preprocess) mTypes
  let formalTypes = getTypeHoleId <$> formals'
  case mConv of
    Just (Foreign (StrLit conv))
      | conv == "C" -> do
        retType <- holeId <$> getCurrentReturn
        storeFact $ regularExprConstraint retType
        traverse_ (storeFact . regularExprConstraint) formalTypes
        storeCSymbol $ getName name
      | otherwise -> undefined
    Nothing -> return ()
  mTypes' `for_` \types -> do
    retHandle <- getCurrentReturn
    retType'@(~(TupleType retVars)) <-
      doOutsideCtx $ makeTuple <$>
      traverse (fmap handleId . (`freshASTTypeHandle` Star)) (fromJust mTypes)
    storeFact $ holeId retHandle `typeUnion` retType'
    zipWithM_ ((storeFact .) . typeUnion) (handleVars types) retVars
  (fs, retType) <- (_2 %~ VarType . holeId) <$> endProc
  let argumentsType = VarType <$> formalTypes
  let procedureType = makeFunction argumentsType retType
  int <- nextHandleCounter
  storeFact $ constExprConstraint procedureType
  storeFact $ int `functionKind` procedureType
  hole <- storeProc (getName name) fs procedureType
  return (hole, ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

-- TODO: consult conventions with the man
-- TODO: add handle (dependent on the context) to the node
instance Preprocess Procedure a b where
  preprocessImpl _ procedure@(Procedure header body) = do
    (vars, _, _, tCons, tVars, _, _) <- localVariables header
    beginProc (getName procedure) vars tCons tVars
    (vars', _, _, tCons', tVars', _, _) <- localVariables body
    openProc vars' tCons' tVars'
    body' <- preprocess body
    header' <-
      preprocessFinalize (takeAnnot header) $
      preprocessProcedureHeader (unAnnot header)
    return (getTypeHole header', Procedure header' body')

-- TODO: ditto
instance Preprocess ProcedureDecl a b where
  preprocessImpl _ procedure@(ProcedureDecl header) = do
    (vars, _, _, tCons, tVars, _, _) <- localVariables header
    beginProc (getName procedure) vars tCons tVars
    header' <-
      preprocessFinalize (takeAnnot header) $
      preprocessProcedureHeader (unAnnot header)
    return (EmptyTypeHole, ProcedureDecl header')

instance Preprocess Formal a b where
  preprocessImpl _ (Formal mKind invar type' name) = do
    hole <- lookupVar (getName name)
    type'' <- preprocess type'
    maybeKindUnif mKind (holeId hole) (getTypeHoleId type'')
    return (hole, Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess Stmt a b where
  preprocessImpl _ =
    \case
      EmptyStmt -> purePreprocess EmptyTypeHole EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFact $ getTypeHoleId cond' `typeConstraint` ComplType BoolType
        storeFact $ boolKind `minKindConstraint` getTypeHoleId cond'
        (EmptyTypeHole, ) <$>
          liftA2 (IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        let scrutineeType = getTypeHoleId scrutinee'
        arms' <- preprocessT arms
        let armTypes = getTypeHoleId <$> arms'
        traverse_ (storeFact . subType scrutineeType) armTypes
        return (EmptyTypeHole, SwitchStmt scrutinee' arms')
      SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (EmptyTypeHole, SpanStmt key' value' body')
      AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        let exprTypes = getTypeHoleId <$> exprs'
        zipWithM_
          (\lvalue exprType ->
             storeFact $ getTypeHoleId lvalue `subType` exprType)
          lvalues'
          exprTypes
        return (EmptyTypeHole, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined
      (CallStmt names mConv expr actuals mTargets annots) -- TODO: this is just a placeholder
       -> do
        retTypes <-
          traverse
            (\name -> handleId <$> freshNamedTypeHandle (getName name) name Star)
            names
        argTypes <-
          traverse
            (\(num, actual) ->
               handleId <$>
               freshNamedTypeHandle (T.pack $ "actual" <> show num) actual Star)
            (zip [0 :: Int ..] actuals)
        names' <- preprocessT names
        expr' <- preprocess expr
        actuals' <- preprocessT actuals
        mTargets' <- preprocessT mTargets
        annots' <- preprocessT annots
        storeFact . typeUnion (getTypeHoleId expr') . AddrType $
          makeFunction (VarType <$> argTypes) (toType $ makeTuple retTypes)
        zipWithM_ ((storeFact .) . subType) (getTypeHoleId <$> names') retTypes
        zipWithM_
          ((storeFact .) . subType)
          argTypes
          (getTypeHoleId <$> actuals')
        return
          ( EmptyTypeHole
          , CallStmt names' mConv expr' actuals' mTargets' annots')
      JumpStmt {} -> undefined
      ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- preprocessT actuals
        retType@(~(TupleType retVars)) <-
          doOutsideCtx $ makeTuple <$>
          traverse ((handleId <$>) . (`freshASTTypeHandle` Star)) actuals
        zipWithM_ ((storeFact .) . subType) retVars (handleVars actuals')
        getCurrentReturn >>= storeFact . (`typeUnion` retType) . holeId
        return (EmptyTypeHole, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined
      label@LabelStmt {} -> do
        hole <- lookupVar (getName label)
        storeFact $ addressKind `kindConstraint` holeId hole
        storeFact $ holeId hole `typeConstraint` ComplType LabelType
        storeFact $ constExprConstraint $ holeId hole
        purePreprocess hole label
      ContStmt {} -> undefined
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        let exprType = getTypeHoleId expr'
        storeFact $ addressKind `minKindConstraint` exprType
        storeFact $ exprType `typeConstraint` ComplType LabelType
        (EmptyTypeHole, ) . GotoStmt expr' <$> preprocessT mTargets
      CutToStmt {} -> undefined

doOutsideCtx :: MonadInferPreprocessor m => m a -> m a
doOutsideCtx action = popTopContext >>= (action <*) . pushContext

-- TODO: this seems wrong
instance Preprocess KindName a b where
  preprocessImpl annot (KindName mKind name) = do
    nameType <- lookupVar (getName name)
    handle <- freshNamedTypeHandle (getName name) annot Star
    maybeKindUnif mKind (handleId handle) (holeId nameType)
    return (SimpleTypeHole handle, KindName mKind (preprocessTrivial name))

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
    purePreprocess (SimpleTypeHole handle) lit

instance Preprocess Actual a b where
  preprocessImpl annot (Actual mKind expr) = do
    expr' <- preprocess expr
    handle <- freshASTTypeHandle annot Star
    maybeKindUnif mKind (handleId handle) (getTypeHoleId expr')
    return (SimpleTypeHole handle, Actual mKind expr')

instance Preprocess Init a b where
  preprocessImpl annot =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshASTTypeHandle annot Star
        let exprTypes = getTypeHoleId <$> exprs'
        traverse_ (storeFact . constExprConstraint) exprTypes
        traverse_ (storeFact . (`subType` handleId handle)) exprTypes
        return (SimpleTypeHole handle, ExprInit exprs')
      strInit@StrInit {} -> strInitCommon StringType strInit
      strInit@Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- freshTypeHelper Star
        storeFact $ handleId handle `typeConstraint` ComplType c
        purePreprocess (SimpleTypeHole handle) strInit

instance Preprocess Datum a b where
  preprocessImpl annot =
    \case
      datum@(DatumLabel name) -> do
        hole <- lookupVar (getName name)
        storeFact $ addressKind `kindConstraint` holeId hole
        storeFact $ holeId hole `typeConstraint`
          toType (AddrType $ VarType NoType)
        purePreprocess hole datum
      datum@(DatumAlign _) -> do
        purePreprocess EmptyTypeHole datum
      Datum type' mSize mInit -> do
        handle <- freshASTTypeHandle annot Star
        type'' <- preprocess type'
        let typeType = getTypeHoleId type''
        mSize' <- traverse preprocess mSize
        mInit' <-
          for mInit $ \init -> do
            init' <- preprocess init
            let initType = getTypeHoleId init'
            storeFact $ typeType `subType` initType
            storeFact $ linkExprConstraint initType
            storeFact $ addressKind `kindConstraint` handleId handle
            storeFact $ handleId handle `typeConstraint`
              AddrType (VarType typeType)
            return init'
        return (SimpleTypeHole handle, Datum type'' mSize' mInit')

instance Preprocess Size a b where
  preprocessImpl _ =
    \case
      Size (Just expr) -> do
        expr' <- preprocess expr
        let hole = getTypeHole expr'
        storeFact $ constExprConstraint $ holeId hole
        return (hole, Size $ Just expr')
      size -> purePreprocess EmptyTypeHole size

instance Preprocess LValue a b where
  preprocessImpl annot =
    \case
      lvName@LVName {} -> do
        lookupFVar (getName lvName) >>= \schemeHole ->
          case schemeHole of
            EmptyTypeHole ->
              lookupVar (getName lvName) >>= (`purePreprocess` lvName)
            scheme -> do
              inst <-
                handleId <$> freshNamedTypeHandle (getName lvName) annot Star
              handle <- freshNamedTypeHandle (getName lvName) annot Star
              storeFact $ holeId scheme `instType` inst
              storeFact $ handleId handle `typeUnion` AddrType inst
              storeFact $ constExprConstraint (handleId handle)
              storeFact $ constExprConstraint inst
              storeFact $ addressKind `kindConstraint` handleId handle
              storeFact $ tVarId inst `functionKind` inst
              purePreprocess (LVInstTypeHole handle schemeHole) lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      LVRef type' expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHole EmptyTypeHole <$>) <$> mAsserts
        storeFact $ addressKind `minKindConstraint` getTypeHoleId expr'
        return (getTypeHole type'', LVRef type'' expr' mAsserts')

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
        let leftType = getTypeHoleId left'
        right' <- preprocess right
        let rightType = getTypeHoleId right'
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
        return (SimpleTypeHole handle, BinOpExpr op left' right')
      NegExpr expr -> NegExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      ComExpr expr -> ComExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      LitExpr lit mType -> do
        lit' <- preprocess lit
        let litType = getTypeHole lit'
        mType' <-
          for mType $ \type' -> do
            type'' <- preprocess type'
            let typeType = getTypeHoleId type''
            storeFact $ typeType `subType` holeId litType
            storeFact $ constExprConstraint $ holeId litType
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
        storeFact $ tupleType `typeUnion` makeTuple (getTypeHoleId <$> actuals')
        storeFact $ unstorableConstraint tupleType
        storeFact $ handleId fType `typeUnion`
          makeFunction [handleId argType] (handleId retType)
        storeFact $ handleId opScheme `typeUnion`
          getNamedOperator (getName name)
        storeFact $ handleId opScheme `instType` handleId fType
        storeFact $ handleId argType `subType` tupleType
        storeFact $ handleId handle `subType` handleId retType
        return
          (SimpleTypeHole handle, PrefixExpr (preprocessTrivial name) actuals')
      InfixExpr name left right -> do
        handle <- freshTypeHelper Star
        tupleType <- freshTypeHelper Star
        argType <- freshTypeHelper Star
        retType <- freshTypeHelper Star
        fType <- freshTypeHelper Star
        opScheme <- freshTypeHelper Star
        left' <- preprocess left
        right' <- preprocess right
        let leftType = getTypeHoleId left'
            rightType = getTypeHoleId left'
        storeFact $ handleId tupleType `typeUnion`
          makeTuple [leftType, rightType]
        storeFact $ handleId fType `typeUnion`
          makeFunction [handleId argType] (handleId retType)
        storeFact $ handleId opScheme `typeUnion`
          getNamedOperator (getName name)
        storeFact $ handleId opScheme `instType` handleId fType
        storeFact $ handleId argType `subType` handleId tupleType
        storeFact $ handleId handle `subType` handleId retType
        return
          ( SimpleTypeHole handle
          , InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHole n', c n')

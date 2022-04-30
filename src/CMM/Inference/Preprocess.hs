{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: add kinds and constnesses where they make sense
-- TODO: all types of things inside procedures should be subtypes of the return type
module CMM.Inference.Preprocess where

import safe Control.Applicative (Applicative((<*), pure), liftA2)
import safe Control.Lens.Getter ((^.), uses)
import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_2)
import safe Control.Monad (Monad((>>=), return), (>=>), zipWithM_, zipWithM)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Foldable (for_, traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$>), (<&>))
import safe Data.Int (Int)
import safe Data.List (elem, foldl, unzip, zip, head)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), fromJust)
import safe Data.Monoid ((<>))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (Traversable(traverse), for)
import safe Data.Tuple (uncurry, snd)
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
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, MemberExpr,
     NegExpr, ParExpr, PrefixExpr)
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
  , Unit(Unit), Struct (Struct)
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
  , localVariables, structVariables
  )
import safe CMM.AST.Variables.State
  ( funcInstVariables
  , funcVariables
  , typeVariables
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
  ( constExprConstraint
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
  , unstorableConstraint, forall
  )
import safe CMM.Inference.HandleCounter (nextHandleCounter)
import safe CMM.Inference.Preprocess.HasTypeHole
  ( HasTypeHole(getTypeHole)
  , WithTypeHole(withTypeHole)
  , getTypeHoleId
  )
import safe CMM.Inference.Preprocess.State as Infer
  ( Preprocessor
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
  , storeVar, pushStruct, currentContext, collectTVars, lookupSIMem, pushFacts, popTopFacts, lookupSMem, storeFacts
  )
import safe CMM.Inference.Preprocess.TypeHole
  ( TypeHole(EmptyTypeHole, LVInstTypeHole, SimpleTypeHole, MemberTypeHole)
  , holeId, holeHandle
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
import safe CMM.Inference.TypeVar (TypeVar(tVarId))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Inference.Preprocess.Context ( Context(StructCtx) )
import safe CMM.Control.Applicative ( (<:>) )

-- TODO: check everywhere whether propagating types correctly (via subtyping)
-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class Preprocess n a b where
  preprocess ::
       (WithTypeHole a b, HasPos a) => Annot n a -> Preprocessor (Annot n b)
  preprocess (Annot n a) = preprocessFinalize a $ preprocessImpl a n
  preprocessImpl ::
       (WithTypeHole a b, HasPos a) => a -> n a -> Preprocessor (TypeHole, n b)

preprocessTrivial :: (Functor n, WithTypeHole a b) => n a -> n b
preprocessTrivial = (withTypeHole EmptyTypeHole <$>)

preprocessFinalize ::
     (Functor m, WithTypeHole a b) => a -> m (TypeHole, n b) -> m (Annot n b)
preprocessFinalize a preprocessed =
  preprocessed <&> \(handle, n') -> withTypeHoledAnnot handle a n'

data PreprocessHint =
  PreprocessHint

type instance Constraint PreprocessHint a b =
     (WithTypeHole a b, HasPos a)

type instance Space PreprocessHint = Preprocess'

class Preprocess' a b n where
  preprocess' :: (WithTypeHole a b, HasPos a) => n a -> Preprocessor (n b)

instance WithTypeHole a b => Preprocess' a b Name where
  preprocess' = return . preprocessTrivial

instance Preprocess n a b => Preprocess' a b (Annot n) where
  preprocess' = preprocess

pass ::
     (ASTmap PreprocessHint n a b, WithTypeHole a b, HasPos a)
  => n a
  -> Preprocessor (n b)
pass = astMapM PreprocessHint preprocess'

instance {-# OVERLAPPABLE #-} ASTmap PreprocessHint n a b =>
                              Preprocess n a b where
  preprocessImpl _ = fmap (EmptyTypeHole, ) . pass

preprocessT ::
     (Preprocess n a b, WithTypeHole a b, Traversable t, HasPos a)
  => t (Annot n a)
  -> Preprocessor (t (Annot n b))
preprocessT = traverse preprocess

withTypeHoledAnnot :: WithTypeHole a b => TypeHole -> a -> n b -> Annot n b
withTypeHoledAnnot = (withAnnot .) . withTypeHole

-- TODO: create a better name
purePreprocess ::
     (WithTypeHole a b, Functor n)
  => TypeHole
  -> n a
  -> Preprocessor (TypeHole, n b)
purePreprocess hole = return . (hole, ) . (withTypeHole hole <$>)

handleVars :: (Functor f, HasTypeHole a) => f a -> f TypeVar
handleVars types = holeId . getTypeHole <$> types

instance Preprocess Unit a b where
  preprocessImpl _ unit@(Unit topLevels) = do
    let collector = globalVariables unit
        fVars = collector ^. funcVariables
        fIVars = collector ^. funcInstVariables
    beginUnit collector
    storeFacts builtInTypeFacts
    -- TODO: rename this
    let storeFacts' var =
          storeFacts
            [ constExprConstraint var
            ,  functionKind (tVarId var) var -- TODO: think this through
            ]
    for_ (Map.keys fIVars) $ lookupFIVar >=> storeFacts' . holeId
    for_ (Map.keys fVars) $ lookupFVar >=> storeFacts' . holeId
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
     (WithTypeHole a b, HasPos a)
  => Annot Expr a
  -> Annot Expr a
  -> Preprocessor (Annot Expr b, Annot Expr b)
preprocessSpanCommon key value = do
  key' <- preprocess key
  let keyType = getTypeHoleId key'
  value' <- preprocess value
  let valueType = getTypeHoleId value'
  storeFacts
    [ constExprConstraint keyType
    , linkExprConstraint valueType
    , valueType `subType` keyType
    ]
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
        storeFacts
          [ typeType `subType` holeId handle
          , constExprConstraint $ holeId handle
          , holeId handle `subType` getTypeHoleId expr'
          ]
        return $ ConstDecl (Just type'') (preprocessTrivial name) expr'
      TypedefDecl type' names -> do
        type'' <- preprocess type'
        let handle = VarType $ getTypeHoleId type''
        traverse_ (`storeTCon` handle) (getName <$> names)
        return $ TypedefDecl type'' (preprocessTrivial <$> names)

-- TODO: continue from here
instance Preprocess Struct a b where
  preprocessImpl _ struct@(Struct paraName datums) = do
    pushTypeVariables $ structVariables struct ^. typeVariables
    (constraint, paraName') <- preprocessParaName lookupTCon Star paraName
    let hole = getTypeHole paraName'
    pushStruct
      (getName paraName, hole)
      (getName paraName, constraint)
    datumResults <- preprocessDatums [] datums
    datums' <- zipWithM preprocessFinalize (takeAnnot <$> datums) (return <$> datumResults)
    return (EmptyTypeHole, Struct paraName' datums')

instance Preprocess Class a b where
  preprocessImpl _ class'@(Class paraNames paraName methods) = do
    pushTypeVariables $ classVariables class' ^. typeVariables
    (constraints, paraNames') <- unzip <$> traverse (preprocessParaName lookupClass Constraint) paraNames
    (constraint, paraName') <- preprocessParaName lookupClass Constraint paraName
    let hole = getTypeHole paraName'
    pushClass
      (getName paraName, hole)
      (getName paraName, constraint)
      (fmap getName paraNames `zip` constraints)
    (hole, ) . Class paraNames' paraName' <$> preprocessT methods <* popContext <*
      popTypeVariables

instance Preprocess Instance a b where
  preprocessImpl _ instance'@(Instance paraNames paraName methods) = do
    pushTypeVariables $ instanceVariables instance' ^. typeVariables
    (constraints, paraNames') <- unzip <$> traverse (preprocessParaName lookupClass Constraint) paraNames
    (constraint, paraName') <- preprocessParaName lookupClass Constraint paraName
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
       (WithTypeHole a b, HasPos a)
    => Annot param a
    -> Preprocessor (Annot param b)

instance PreprocessParam Name a b where
  preprocessParam name = do
    handle <- lookupTVar $ getName name
    return $ withTypeHole handle <$> name

instance PreprocessParam AST.Type a b where
  preprocessParam = preprocess

preprocessParaName ::
     (WithTypeHole a b, PreprocessParam param a b, HasPos a)
  => (Text -> Preprocessor TypeHole)
  -> TypeKind
  -> Annot (ParaName param) a
  -> Preprocessor (Infer.Type, Annot (ParaName param) b)
preprocessParaName looker kind (Annot (ParaName name params) annot) = do
  let name' = preprocessTrivial name
  params' <- traverse preprocessParam params
  class' <- looker $ getName name'
  handle <- freshNamedTypeHandle (getName name) annot kind
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
     (ToType t, ToType t', HasName n) => Maybe n -> t -> t' -> Preprocessor ()
maybeKindUnif mKind derived base =
  case mKind of
    Nothing -> storeFact $ derived `typeUnion` base
    Just kind ->
      storeFacts
        [ derived `typeConstraint` base
        , derived `subConst` base
        , base `subConst` derived
        , getDataKind (getName kind) `kindConstraint` derived
        ]

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
     (WithTypeHole a b, HasPos a)
  => ProcedureHeader a
  -> Preprocessor (TypeHole, ProcedureHeader b)
preprocessProcedureHeader (ProcedureHeader mConv name formals mTypes) = do
  formals' <- doOutsideCtx $ preprocessT formals
  mTypes' <- doOutsideCtx $ traverse (traverse preprocess) mTypes
  let formalTypes = getTypeHoleId <$> formals'
  case mConv of
    Just (Foreign (StrLit conv))
      | conv == "C" -> do
        retType <- holeId <$> getCurrentReturn
        storeFacts $ regularExprConstraint <$> (retType : formalTypes)
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
  storeFacts
    [ constExprConstraint procedureType
    , int `functionKind` procedureType
    ]
  hole <- storeProc (getName name) fs procedureType
  return (hole, ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

-- TODO: consult conventions with the man
-- TODO: add handle (dependent on the context) to the node
instance Preprocess Procedure a b where
  preprocessImpl _ procedure@(Procedure header body) = do
    beginProc (getName procedure) $ localVariables header
    openProc $ localVariables body
    body' <- preprocess body
    header' <-
      preprocessFinalize (takeAnnot header) $
      preprocessProcedureHeader (unAnnot header)
    return (getTypeHole header', Procedure header' body')

-- TODO: ditto
instance Preprocess ProcedureDecl a b where
  preprocessImpl _ procedure@(ProcedureDecl header) = do
    beginProc (getName procedure) $ localVariables header
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
        storeFacts
          [ getTypeHoleId cond' `typeConstraint` ComplType BoolType
          , boolKind `minKindConstraint` getTypeHoleId cond'
          ]
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
        storeFacts
          [ addressKind `kindConstraint` holeId hole
          , holeId hole `typeConstraint` ComplType LabelType
          , constExprConstraint $ holeId hole
          ]
        purePreprocess hole label
      ContStmt {} -> undefined
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        let exprType = getTypeHoleId expr'
        storeFacts
          [ addressKind `minKindConstraint` exprType
          , exprType `typeConstraint` ComplType LabelType
          ]
        (EmptyTypeHole, ) . GotoStmt expr' <$> preprocessT mTargets
      CutToStmt {} -> undefined

doOutsideCtx :: Preprocessor a -> Preprocessor a
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
    storeFact . (`minKindConstraint` handleId handle) $ case lit of
      LitInt {} -> integerKind
      LitFloat {} -> floatKind
      LitChar {} -> integerKind -- TODO: check this one? but probably correctus
    storeFact . constExprConstraint $ handleId handle
    purePreprocess (SimpleTypeHole handle) lit

instance Preprocess Actual a b where
  preprocessImpl annot (Actual mKind expr) = do
    expr' <- preprocess expr
    handle <- freshASTTypeHandle annot Star
    maybeKindUnif mKind (handleId handle) (getTypeHoleId expr')
    return (SimpleTypeHole handle, Actual mKind expr')

unionizeTypes :: ToType a => [a] -> Preprocessor ()
unionizeTypes = \case
  [] -> return ()
  tVar : tVars -> traverse_ (storeFact . typeUnion tVar) tVars

instance Preprocess Init a b where
  preprocessImpl annot =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshASTTypeHandle annot Star
        let exprTypes = getTypeHoleId <$> exprs'
        storeFacts $ constExprConstraint <$> exprTypes
        storeFacts $ (`subType` handleId handle) <$> exprTypes
        return (SimpleTypeHole handle, ExprInit exprs')
      strInit@StrInit {} -> strInitCommon StringType strInit
      strInit@Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- freshTypeHelper Star
        storeFact $ handleId handle `typeConstraint` ComplType c
        purePreprocess (SimpleTypeHole handle) strInit

preprocessDatums :: (HasPos n, WithTypeHole n a) =>
  [TypeHole]
  -> [Annotation Datum n]
  -> Preprocessor [(TypeHole, Datum a)]
preprocessDatums _ [] = return [] -- TODO: continue from here
preprocessDatums cache ((Annot datum annot) : others) = case datum of
  DatumLabel name -> do
    hole <- uses currentContext head >>= \case
      StructCtx {} -> do
        hole <- lookupSIMem $ getName name
        mem <- lookupSMem $ getName name
        return $ MemberTypeHole (holeHandle hole) [holeHandle mem] []
      _ -> do
        tVar <- handleId <$> freshTypeHelper Star
        hole <- lookupVar $ getName name
        storeFacts
          [ addressKind `kindConstraint` holeId hole
          , holeId hole `typeConstraint`
            toType (AddrType $ VarType tVar)
          ]
        return hole
    purePreprocess hole datum <:> preprocessDatums (hole:cache) others
  DatumAlign _ -> do
    purePreprocess EmptyTypeHole datum <:> preprocessDatums [] others
  Datum type' mSize mInit -> do
    uses currentContext head >>= \case
      StructCtx _ structConstraint -> do
        tVars <- collectTVars
        handle <- freshASTTypeHandle annot Star
        pushFacts
        (hole, datum') <- goGeneral
        fs <- popTopFacts
        unionizeTypes $ handleId handle : (holeId <$> cache)
        let
          t = makeFunction [toType . AddrType $ snd structConstraint] . toType $ holeId hole
          cache' = cache <&> \case
            MemberTypeHole iMem [mem] [] -> (iMem, mem)
            _ -> undefined -- TODO: logic error
          hole' = MemberTypeHole handle `uncurry` unzip cache'
        storeFact $ forall tVars [handleId handle `typeUnion` t] fs
        ((hole', datum') :) <$> preprocessDatums [] others
      _ -> do
        result@(hole, _) <- goGeneral
        unionizeTypes $ holeId hole : (holeId <$> cache)
        (result:) <$> preprocessDatums [] others
    where
      goGeneral = do
        handle <- freshASTTypeHandle annot Star
        type'' <- preprocess type'
        let typeType = getTypeHoleId type''
        mSize' <- traverse preprocess mSize
        mInit' <- traverse preprocess mInit
        mInit' `for_` \init -> do
          let initType = getTypeHoleId init
          storeFacts
            [typeType `subType` initType, linkExprConstraint initType]
        storeFacts
          [ addressKind `kindConstraint` handleId handle
          , handleId handle `typeConstraint` AddrType (VarType typeType)
          ]
        return (SimpleTypeHole handle, Datum type'' mSize' mInit')

instance Preprocess Datum a b where
  preprocessImpl a datum = head <$> preprocessDatums [] [datum `Annot` a]

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
              storeFacts
                [ holeId scheme `instType` inst
                , handleId handle `typeUnion` AddrType inst
                , constExprConstraint (handleId handle)
                , constExprConstraint inst
                , addressKind `kindConstraint` handleId handle
                , tVarId inst `functionKind` inst
                ]
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
      MemberExpr expr field -> undefined
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
          then
            storeFacts
              [ leftType `typeConstraint` rightType
              , handleId handle `subConst` leftType
              , handleId handle `subConst` rightType
              , handleId handle `typeConstraint` ComplType BoolType
              , boolKind `kindConstraint` handleId handle
              ]
          else
            storeFacts
              [ handleId handle `subType` leftType
              , handleId handle `subType` rightType
              ]
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
            storeFacts
              [ typeType `subType` holeId litType
              , constExprConstraint $ holeId litType
              ]
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
        storeFacts
          [ tupleType `typeUnion` makeTuple (getTypeHoleId <$> actuals')
          , unstorableConstraint tupleType
          , handleId fType `typeUnion`
            makeFunction [handleId argType] (handleId retType)
          , handleId opScheme `typeUnion` getNamedOperator (getName name)
          , handleId opScheme `instType` handleId fType
          , handleId argType `subType` tupleType
          , handleId handle `subType` handleId retType
          ]
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
        storeFacts
          [ handleId tupleType `typeUnion`
            makeTuple [leftType, rightType]
          , handleId fType `typeUnion`
            makeFunction [handleId argType] (handleId retType)
          , handleId opScheme `typeUnion`
            getNamedOperator (getName name)
          , handleId opScheme `instType` handleId fType
          , handleId argType `subType` handleId tupleType
          , handleId handle `subType` handleId retType
          ]
        return
          ( SimpleTypeHole handle
          , InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHole n', c n')

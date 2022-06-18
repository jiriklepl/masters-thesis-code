{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: add kinds and constnesses where they make sense
-- TODO: all types of things inside procedures should be subtypes of the return type
module CMM.Inference.Preprocess where

import safe Control.Applicative (Applicative((<*), pure), liftA2)
import safe Control.Lens.Getter ((^.), uses)
import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_2)
import safe Control.Monad (Monad((>>=), return), (>=>), zipWithM, zipWithM_)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Foldable (for_, traverse_, Foldable (foldl'), concat)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$>), (<&>), (<$))
import safe Data.Int (Int)
import safe Data.List (elem, head, unzip, zip, unzip3)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), fromJust)
import safe Data.Monoid ((<>))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (Traversable(traverse), for)
import safe Data.Tuple (snd, uncurry)
import safe GHC.Err (undefined, error)
import safe Text.Show (Show(show))
import safe Data.Tuple.Extra (uncurry3)

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
  , Struct(Struct)
  , Targets
  , Type(TAuto, TBits, TName, TPar)
  , Unit(Unit), StackDecl (StackDecl)
  )
import safe CMM.AST.Annot as AST
  ( Annot
  , Annotation(Annot)
  , takeAnnot
  , unAnnot
  , withAnnot
  )
import safe CMM.AST.GetName as AST (GetName(getName))
import safe CMM.AST.Maps as AST (ASTmap(astMapM), Constraint, Space)
import safe CMM.AST.Variables as AST
  ( classVariables
  , globalVariables
  , instanceVariables
  , localVariables
  , structVariables
  )
import safe CMM.AST.Variables.State
  ( funcInstVariables
  , funcVariables
  , typeVariables, structMembers
  )
import safe CMM.Control.Applicative ((<:>))
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
  , forall
  , functionKind
  , instType
  , kindConstraint
  , linkExprConstraint
  , minKindConstraint
  , registerConstraint
  , regularExprConstraint
  , subConst
  , subType
  , typeConstraint
  , typeUnion
  , unstorableConstraint, NestedFact (Fact), classConstraint, classFact
  )
import safe CMM.Inference.HandleCounter (nextHandleCounter)
import safe CMM.Inference.Preprocess.Context (Context(StructCtx))
import safe CMM.Inference.Preprocess.HasTypeHole
  ( HasTypeHole(getTypeHole)
  , WithTypeHole(withTypeHole)
  , getTypeHoleId
  )
import safe CMM.Inference.Preprocess.State as Infer
  ( Preprocessor
  , beginProc
  , beginUnit
  , collectTVars
  , currentContext
  , endProc
  , freshNamedASTTypeHandle
  , freshTypeHelper
  , getCurrentReturn
  , lookupClass
  , lookupFIVar
  , lookupFVar
  , lookupSIMem
  , lookupSMem
  , lookupTCon
  , lookupTVar
  , lookupVar
  , openProc
  , popContext
  , popTopContext
  , popTopFacts
  , popTypeVariables
  , pushClass
  , pushContext
  , pushFacts
  , pushInstance
  , pushStruct
  , pushTypeVariables
  , storeCSymbol
  , storeFact
  , storeFacts
  , storeProc
  , storeTCon
  , storeVar, freshASTTypeHandle
  )
import safe CMM.Inference.Preprocess.TypeHole
  ( TypeHole(EmptyTypeHole, LVInstTypeHole, MemberTypeHole,
         SimpleTypeHole, MethodTypeHole, NamedTypeHole)
  , holeHandle
  )
import safe CMM.Inference.Type as Infer
  ( ToType(toType)
  , Type(ComplType, VarType), makeAppType, foldApp
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, LabelType, String16Type,
          StringType, TBitsType, TupleType), makeTuple, makeFunction
  )
import safe CMM.Inference.TypeHandle (handleId)
import safe CMM.Inference.TypeKind (TypeKind(Constraint, GenericType, Star))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar(tVarId))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Inference.State (fieldClassHelper)
import safe CMM.Inference.Refresh ( refreshNestedFact )

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
handleVars types = getTypeHoleId <$> types

instance Preprocess Unit a b where
  preprocessImpl _ unit@(Unit topLevels) = do
    let collector = globalVariables unit
        fVars = collector ^. funcVariables
        fIVars = collector ^. funcInstVariables
        sMems = collector ^. structMembers
    beginUnit collector
    storeFacts builtInTypeFacts
    -- TODO: rename this
    let storeFacts' var =
          storeFacts
            [ constExprConstraint var
            , tVarId (toTypeVar var) `functionKind` var -- TODO: think this through
            ]
    for_ (Map.keys fIVars) $ lookupFIVar >=> storeFacts'
    for_ (Map.keys fVars) $ lookupFVar >=> storeFacts'
    for_ (Map.keys sMems) $ lookupSMem >=> storeFacts'
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
  let keyType = getTypeHole key'
  value' <- preprocess value
  let valueType = getTypeHole value'
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
        handle <- lookupVar name
        expr' <- preprocess expr
        type'' <- preprocess type'
        storeFacts
          [ getTypeHole type'' `subType` handle
          , constExprConstraint handle
          , handle `subType` getTypeHole expr'
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
    pushStruct (getName paraName, hole) (getName paraName, constraint)
    datums' <- preprocessDatums datums
    (hole, Struct paraName' datums') <$ popContext <* popTypeVariables

instance Preprocess Class a b where
  preprocessImpl _ class'@(Class paraNames paraName methods) = do
    pushTypeVariables $ classVariables class' ^. typeVariables
    (constraints, paraNames') <-
      unzip <$> traverse (preprocessParaName lookupClass Constraint) paraNames
    (constraint, paraName') <-
      preprocessParaName lookupClass Constraint paraName
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
    (constraints, paraNames') <-
      unzip <$> traverse (preprocessParaName lookupClass Constraint) paraNames
    (constraint, paraName') <-
      preprocessParaName lookupClass Constraint paraName
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
    handle <- lookupTVar name
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
  handle <- freshNamedASTTypeHandle (getName name) annot kind
  let constraint =
        foldl'
          ((toType .) . AppType)
          (toType class')
          (toType . getTypeHole <$> params')
  storeFact $ handle `typeUnion` constraint
  return
    ( constraint
    , withTypeHole (SimpleTypeHole handle) annot `withAnnot`
      ParaName name' params')

instance Preprocess Import a b where
  preprocessImpl _ import'@Import {} = do
    handle <- lookupVar import'
    purePreprocess handle import'

instance Preprocess Export a b where
  preprocessImpl _ export@Export {} = do
    handle <- lookupVar export
    purePreprocess handle export

instance Preprocess AST.Type a b where
  preprocessImpl annot t = case t of
    TBits int -> do
      handle <- freshASTTypeHandle annot Star
      storeFact $ handle `typeUnion` ComplType (TBitsType int)
      purePreprocess (SimpleTypeHole handle) t
    TName name -> do
      hole <- lookupTCon name
      purePreprocess hole t
    TAuto Nothing -> do
      handle <- freshASTTypeHandle annot Star -- TODO:, really Star?
      purePreprocess (SimpleTypeHole handle) t
    TAuto (Just name) -> do
      hole <- lookupTVar name
      purePreprocess hole t
    TPar parType -> do
      parType' <- preprocess parType
      return (getTypeHole parType', TPar parType')

instance Preprocess ParaType a b where
  preprocessImpl annot (ParaType type' types') = do
    type'' <- preprocess type'
    types'' <- traverse preprocess types'
    handle <- freshASTTypeHandle annot GenericType -- TODO: determine the kind
    storeFact $ handle `typeUnion`
      foldl'
        ((toType .) . AppType)
        (toType $ getTypeHoleId type'')
        (toType . getTypeHoleId <$> types'')
    return (SimpleTypeHole handle, ParaType type'' types'')

maybeKindUnif ::
     (ToType t, ToType t', GetName n) => Maybe n -> t -> t' -> Preprocessor ()
maybeKindUnif mKind derived base =
  storeFacts $ case mKind of
    Nothing -> [derived `typeUnion` base]
    Just kind ->
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
          hole <- lookupVar name
          maybeKindUnif mKind hole typeType
          case mStrLit of
            Nothing -> return (withTypeHole hole <$> name, Nothing)
            Just (StrLit strLit) -> do
              storeFact $ strLit `registerConstraint` hole
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

preprocessHeader :: (WithTypeHole a b, HasPos a) =>
  Annot ProcedureHeader a
  -> Preprocessor (Annot ProcedureHeader b)
preprocessHeader header =
  preprocessFinalize (takeAnnot header) $
  preprocessProcedureHeaderImpl (unAnnot header)

-- TODO: consult conventions with man
preprocessProcedureHeaderImpl ::
     (WithTypeHole a b, HasPos a)
  => ProcedureHeader a
  -> Preprocessor (TypeHole, ProcedureHeader b)
preprocessProcedureHeaderImpl (ProcedureHeader mConv name formals mTypes) = do
  formals' <- doOutsideCtx $ preprocessT formals
  mTypes' <- doOutsideCtx $ traverse (traverse preprocess) mTypes
  let formalTypes = getTypeHole <$> formals'
  case mConv of
    Just (Foreign (StrLit conv))
      | conv == "C" -> do
        retType <- getTypeHole <$> getCurrentReturn
        storeFacts $ regularExprConstraint <$> (retType : formalTypes)
        storeCSymbol $ getName name
      | otherwise -> undefined
    Nothing -> return ()
  mTypes' `for_` \types -> do
    retHandle <- getCurrentReturn
    retType'@(~(TupleType retVars)) <-
      doOutsideCtx $ makeTuple <$>
      traverse (`freshASTTypeHandle` Star) (fromJust mTypes)
    storeFact $ retHandle `typeUnion` retType'
    zipWithM_ ((storeFact .) . typeUnion) (handleVars types) retVars
  (fs, retType) <- (_2 %~ VarType . toTypeVar) <$> endProc
  let argumentsType = VarType . toTypeVar <$> formalTypes
  let procedureType = makeFunction argumentsType retType
  int <- nextHandleCounter
  storeFacts
    [constExprConstraint procedureType, int `functionKind` procedureType]
  hole <- storeProc (getName name) fs procedureType
  return (hole, ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

-- TODO: consult conventions with the man
-- TODO: add handle (dependent on the context) to the node
instance Preprocess Procedure a b where
  preprocessImpl _ procedure@(Procedure header body) = do
    beginProc (getName procedure) $ localVariables header
    openProc $ localVariables body
    body' <- preprocess body
    header' <- preprocessHeader header
    return (getTypeHole header', Procedure header' body')

-- TODO: ditto
instance Preprocess ProcedureDecl a b where
  preprocessImpl _ procedure@(ProcedureDecl header) = do
    beginProc (getName procedure) $ localVariables header
    (EmptyTypeHole,) . ProcedureDecl <$> preprocessHeader header

instance Preprocess Formal a b where
  preprocessImpl _ (Formal mKind invar type' name) = do
    hole <- lookupVar name
    type'' <- preprocess type'
    maybeKindUnif mKind hole (getTypeHoleId type'')
    return (hole, Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess StackDecl a b where
  preprocessImpl _ (StackDecl datums) =
    (EmptyTypeHole,) . StackDecl <$> preprocessDatums datums

instance Preprocess Stmt a b where
  preprocessImpl _ =
    \case
      EmptyStmt -> purePreprocess EmptyTypeHole EmptyStmt
      IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        storeFacts
          [ getTypeHole cond' `typeConstraint` ComplType BoolType
          , boolKind `minKindConstraint` getTypeHole cond'
          ]
        (EmptyTypeHole, ) <$>
          liftA2 (IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        let scrutineeType = getTypeHole scrutinee'
        arms' <- preprocessT arms
        let armTypes = getTypeHole <$> arms'
        traverse_ (storeFact . subType scrutineeType) armTypes
        return (EmptyTypeHole, SwitchStmt scrutinee' arms')
      SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (EmptyTypeHole, SpanStmt key' value' body')
      AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        let exprTypes = getTypeHole <$> exprs'
        zipWithM_
          (\lvalue exprType -> storeFact $ getTypeHole lvalue `subType` exprType)
          lvalues'
          exprTypes
        return (EmptyTypeHole, AssignStmt lvalues' exprs')
      PrimOpStmt {} -> undefined
      CallStmt names mConv expr actuals mTargets annots -- TODO: this is just a placeholder
       -> do
        retTypes <-
          names
          `for`
            \name -> handleId <$> freshNamedASTTypeHandle (getName name) name Star
        argTypes <-
          zip [0 :: Int ..] actuals
          `for`
            \(num, actual) ->
               handleId <$>
               freshNamedASTTypeHandle (T.pack $ "actual" <> show num) actual Star
        names' <- preprocessT names
        expr' <- preprocess expr
        actuals' <- preprocessT actuals
        mTargets' <- preprocessT mTargets
        annots' <- preprocessT annots
        storeFact . typeUnion (getTypeHole expr') . AddrType $
          makeFunction (VarType <$> argTypes) (toType $ makeTuple retTypes)
        zipWithM_ ((storeFact .) . subType) (getTypeHole <$> names') retTypes
        zipWithM_ ((storeFact .) . subType) argTypes (getTypeHole <$> actuals')
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
          traverse (`freshASTTypeHandle` Star) actuals
        zipWithM_ ((storeFact .) . subType) retVars (handleVars actuals')
        getCurrentReturn >>= storeFact . (`typeUnion` retType)
        return (EmptyTypeHole, ReturnStmt mConv Nothing actuals')
      ReturnStmt {} -> undefined
      label@LabelStmt {} -> do
        hole <- lookupVar label
        storeFacts
          [ addressKind `kindConstraint` hole
          , hole `typeConstraint` ComplType LabelType
          , constExprConstraint hole
          ]
        purePreprocess hole label
      ContStmt {} -> undefined
      GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        let exprType = getTypeHole expr'
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
    nameType <- lookupVar name
    handle <- freshNamedASTTypeHandle (getName name) annot Star
    maybeKindUnif mKind handle nameType
    return (SimpleTypeHole handle, KindName mKind (preprocessTrivial name))

instance Preprocess Arm a b where
  preprocessImpl = undefined

instance Preprocess Targets a b where
  preprocessImpl = undefined

instance Preprocess Lit a b where
  preprocessImpl annot lit = do
    handle <- freshASTTypeHandle annot Star
    storeFact . (`minKindConstraint` handle) $
      case lit of
        LitInt {} -> integerKind
        LitFloat {} -> floatKind
        LitChar {} -> integerKind -- TODO: check this one? but probably correct
    storeFact $ constExprConstraint handle
    purePreprocess (SimpleTypeHole handle) lit

instance Preprocess Actual a b where
  preprocessImpl annot (Actual mKind expr) = do
    expr' <- preprocess expr
    handle <- freshASTTypeHandle annot Star
    maybeKindUnif mKind handle (getTypeHole expr')
    return (SimpleTypeHole handle, Actual mKind expr')

unionizeTypes :: ToType a => [a] -> Preprocessor ()
unionizeTypes =
  \case
    [] -> return ()
    tVar:tVars -> traverse_ (storeFact . typeUnion tVar) tVars

instance Preprocess Init a b where
  preprocessImpl annot =
    \case
      ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- freshASTTypeHandle annot Star
        let exprTypes = getTypeHole <$> exprs'
        storeFacts $ constExprConstraint <$> exprTypes
        storeFacts $ (`subType` handle) <$> exprTypes
        return (SimpleTypeHole handle, ExprInit exprs')
      strInit@StrInit {} -> strInitCommon StringType strInit
      strInit@Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- freshTypeHelper Star
        storeFact $ handle `typeConstraint` ComplType c
        purePreprocess (SimpleTypeHole handle) strInit

preprocessDatums :: (WithTypeHole a b, HasPos a) => [Annot Datum a] -> Preprocessor [Annot Datum b]
preprocessDatums datums = do
  datumResults <- preprocessDatumsImpl [] datums
  zipWithM
    preprocessFinalize
    (takeAnnot <$> datums)
    (return <$> datumResults)

preprocessDatumsImpl ::
     (HasPos a, WithTypeHole a b)
  => [TypeHole]
  -> [Annotation Datum a]
  -> Preprocessor [(TypeHole, Datum b)]
preprocessDatumsImpl _ [] = return [] -- TODO: continue from here
preprocessDatumsImpl cache ((Annot datum annot):others) =
  case datum of
    DatumLabel name -> do
      hole <-
        uses currentContext head >>= \case
          StructCtx {} -> do
            hole <- lookupSIMem name
            mem <- lookupSMem name
            let name' = fieldClassHelper $ getName name
            classHole <- lookupClass name'
            return $ MemberTypeHole (holeHandle hole) [holeHandle classHole `NamedTypeHole` name'] [holeHandle mem] []
          _ -> do
            tVar <- handleId <$> freshTypeHelper Star
            hole <- lookupVar name
            storeFacts
              [ addressKind `kindConstraint` hole
              , hole `typeUnion` AddrType (VarType tVar)
              ]
            return hole
      purePreprocess hole datum <:> preprocessDatumsImpl (hole : cache) others
    DatumAlign _ -> do
      purePreprocess EmptyTypeHole datum <:> preprocessDatumsImpl [] others
    Datum type' mSize mInit -> do
      uses currentContext head >>= \case
        StructCtx (_, sHole) structConstraint -> do
          tVars <- collectTVars
          -- handle <- freshASTTypeHandle annot Star
          pushFacts
          (hole, datum') <- goGeneral
          fs <- popTopFacts
          -- unionizeTypes $ handle : (holeHandle <$> cache)
          let t =
                makeFunction [toType . AddrType $ snd structConstraint] $
                toType hole
              funcFact h = Fact $ tVarId (toTypeVar h) `functionKind` t
              constExprFact = Fact $ constExprConstraint t
              cache' =
                cache <&> \case
                  MemberTypeHole iMem [hole''] [mem] [] -> (hole'', iMem, mem)
                  _ -> undefined -- TODO: logic error
              hole' = MemberTypeHole (holeHandle sHole) `uncurry3` unzip3 cache'
              scheme (MemberTypeHole h [NamedTypeHole classHandle name] _ _ ) = do
                method <- refreshNestedFact . forall tVars [h `typeUnion` t] $
                  classC : funcFact h : constExprFact : fs
                fact <- refreshNestedFact $ forall tVars [classF] []
                return [method, fact]
                where
                  classC = Fact $ classConstraint name constraint
                  classF = classFact name constraint
                  constraint = foldApp [toType classHandle, snd structConstraint, toType hole]
              scheme _ = undefined -- TODO: logic error
          schemes <- traverse scheme cache
          storeFacts $ concat schemes
          ((hole', datum') :) <$> preprocessDatumsImpl [] others
        _ -> do
          result@(hole, _) <- goGeneral
          unionizeTypes $ hole : cache
          (result :) <$> preprocessDatumsImpl [] others
      where goGeneral = do
              handle <- freshASTTypeHandle annot Star
              type'' <- preprocess type'
              let typeType = getTypeHoleId type''
              mSize' <- traverse preprocess mSize
              mInit' <- traverse preprocess mInit
              mInit' `for_` \init -> do
                let initType = getTypeHole init
                storeFacts
                  [typeType `subType` initType, linkExprConstraint initType]
              storeFacts
                [ addressKind `kindConstraint` handle
                , handle `typeConstraint` AddrType (VarType typeType)
                ]
              return (SimpleTypeHole handle, Datum type'' mSize' mInit')

instance Preprocess Datum a b where
  preprocessImpl a datum = head <$> preprocessDatumsImpl [] [datum `Annot` a]

instance Preprocess Size a b where
  preprocessImpl _ =
    \case
      Size (Just expr) -> do
        expr' <- preprocess expr
        let hole = getTypeHole expr'
        storeFact $ constExprConstraint hole
        return (hole, Size $ Just expr')
      size -> purePreprocess EmptyTypeHole size

instance Preprocess LValue a b where
  preprocessImpl annot =
    \case
      lvName@LVName {} -> do
        lookupFVar lvName >>= \case
          EmptyTypeHole ->
            lookupVar lvName >>= (`purePreprocess` lvName)
          scheme -> do
            inst <- freshNamedASTTypeHandle (getName lvName) annot Star
            handle <- freshNamedASTTypeHandle (getName lvName) annot Star
            storeFacts
              [ scheme `instType` inst
              , handle `typeUnion` AddrType inst
              , constExprConstraint handle
              , constExprConstraint inst
              , addressKind `kindConstraint` handle
              , tVarId (toTypeVar inst) `functionKind` inst
              ]
            purePreprocess (LVInstTypeHole handle scheme) lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      LVRef Nothing expr mAsserts -> do
        expr' <- preprocess expr
        handle <- freshASTTypeHandle annot Star
        let mAsserts' = (withTypeHole EmptyTypeHole <$>) <$> mAsserts
        storeFacts
          [ getTypeHole expr' `typeUnion` AddrType handle
          , addressKind `minKindConstraint` getTypeHole expr'
          ]
        return (SimpleTypeHole handle, LVRef Nothing expr' mAsserts')
      LVRef (Just type') expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHole EmptyTypeHole <$>) <$> mAsserts
        storeFact $ addressKind `minKindConstraint` getTypeHole expr'
        return (getTypeHole type'', LVRef (Just type'') expr' mAsserts')

instance Preprocess Expr a b where
  preprocessImpl annot = -- TODO: this is just ugly
    \case
      MemberExpr struct field ->
        lookupSMem field >>= \case
          EmptyTypeHole -> undefined -- TODO: very bad
          scheme -> do
            struct' <- preprocess struct
            inst <- freshNamedASTTypeHandle (getName field) annot Star
            handle <- freshNamedASTTypeHandle (getName field) annot Star
            argType <- freshNamedASTTypeHandle (getName field) annot Star
            retType <- freshNamedASTTypeHandle (getName field) annot Star
            storeFacts
              [ scheme `instType` inst
              , inst `typeUnion` makeFunction [argType] retType
              , constExprConstraint $ AddrType inst
              , addressKind `kindConstraint` AddrType inst
              , constExprConstraint inst
              , tVarId (toTypeVar inst) `functionKind` inst
              , argType `subType` getTypeHole struct'
              , addressKind `kindConstraint` getTypeHole struct'
              , retType `subType` handle
              , addressKind `kindConstraint` handle
              ]
            return (MethodTypeHole handle (holeHandle scheme) inst, MemberExpr struct' $ preprocessTrivial field)
      ParExpr expr -> ParExpr `preprocessInherit` expr
      LVExpr lvalue -> LVExpr `preprocessInherit` lvalue
      BinOpExpr op left right -- TODO: implement correctly, this is just a placeholder
        -> do
        handle <- freshTypeHelper Star
        operator <- freshTypeHelper Star
        left' <- preprocess left
        let leftType = getTypeHole left'
        right' <- preprocess right
        let rightType = getTypeHole right'
        storeFact $ operator `typeUnion`
          makeFunction [holeHandle leftType, holeHandle rightType] handle
        storeFact $ tVarId (toTypeVar operator) `functionKind` operator
        storeFacts $ if op `elem` [EqOp, NeqOp, GtOp, LtOp, GeOp, LeOp]
          then
            [ leftType `typeConstraint` rightType
            , handle `subConst` leftType
            , handle `subConst` rightType
            , handle `typeConstraint` ComplType BoolType
            , boolKind `kindConstraint` handle
            ]
          else
            [handle `subType` leftType, handle `subType` rightType]
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
            let typeType = getTypeHole type''
            storeFacts [typeType `subType` litType, constExprConstraint litType]
            return type''
        return (litType, LitExpr lit' mType')
      PrefixExpr name actuals -- TODO: fix this
        -> do
        handle <- freshTypeHelper Star
        tupleType <- freshTypeHelper Star
        argType <- freshTypeHelper Star
        retType <- freshTypeHelper Star
        fType <- freshTypeHelper Star
        opScheme <- freshTypeHelper Star
        actuals' <- preprocessT actuals
        storeFacts
          [ tupleType `typeUnion` makeTuple (getTypeHole <$> actuals')
          , unstorableConstraint tupleType
          , fType `typeUnion` makeFunction [argType] retType
          , opScheme `typeUnion` getNamedOperator (getName name)
          , opScheme `instType` fType
          , argType `subType` tupleType
          , handle `subType` retType
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
        let leftType = getTypeHole left'
            rightType = getTypeHole left'
        storeFacts
          [ tupleType `typeUnion` makeTuple [leftType, rightType]
          , fType `typeUnion` makeFunction [argType] retType
          , opScheme `typeUnion` getNamedOperator (getName name)
          , opScheme `instType` fType
          , argType `subType` tupleType
          , handle `subType` retType
          ]
        return
          ( SimpleTypeHole handle
          , InfixExpr (preprocessTrivial name) left' right')
    where
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHole n', c n')

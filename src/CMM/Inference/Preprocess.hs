{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Inference.Preprocess where

import safe Control.Applicative (liftA2)
import safe Control.Lens ( (^.), uses, (%~), (<>=), _2 )
import safe Control.Monad ((>=>), zipWithM, zipWithM_)
import safe Data.Foldable (for_, traverse_)
import safe Data.Functor ((<&>), void)
import safe qualified Data.Map as Map
import safe Data.Maybe (fromJust)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (for)
import safe Data.Tuple.Extra (uncurry3)

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot as AST
  ( Annot
  , Annotation(Annot)
  , takeAnnot
  , unAnnot
  , withAnnot
  )
import safe CMM.AST.GetConv (GetConv(getConv))
import safe CMM.AST.GetName as AST (GetName(getName))
import safe CMM.AST.Maps as AST (ASTmap(astMapM), Constraint, Space)
import safe qualified CMM.Data.Trilean as T
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
  , structMembers
  , CollectorState
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
import safe CMM.Inference.Fact as Fact
  ( NestedFact(Fact)
  , classConstraint
  , classFact
  , constExprConstraint
  , factComment
  , forall
  , instType
  , kindConstraint
  , kindEquality
  , linkExprConstraint
  , minKindConstraint
  , registerConstraint
  , regularExprConstraint
  , subConst
  , subType
  , typingEquality
  , typeEquality
  , unstorableConstraint, classFunDeps
  )
import safe CMM.Inference.GetParent (makeAdoption)
import safe CMM.Inference.Preprocess.Context (Context(StructCtx, ctxConstraint, ctxElab))
import safe CMM.Inference.Preprocess.State as State
  ( Preprocessor)
import safe qualified CMM.Inference.Preprocess.State as State
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration)
  , Elaboration(EmptyElaboration, LVInstElaboration, MemberElaboration,
         MethodElaboration, SimpleElaboration)
  , eHandle
  )
import safe CMM.Inference.Preprocess.WithElaboration
  ( WithElaboration(withElaboration)
  , withEmptyElaboration
  )
import safe CMM.Inference.Refresh (Refresh(refresh), refreshNestedFact)
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type as Infer
  ( ToType(toType)
  , Type(ComplType)
  , foldApp
  , makeAddrType
  , makeTBitsType, makeBoolType, makeLabelType, makeVoidType
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(String16Type, StringType, TupleType)
  , makeFunction
  , makeTuple
  )
import safe CMM.Inference.TypeHandle ()
import safe CMM.Inference.TypeKind (TypeKind(Constraint, Star))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar))
import safe CMM.Inference.Utils (fieldClassHelper)
import safe CMM.Parser.GetPos (GetPos)
import safe CMM.Utils (backQuote, logicError)
import safe CMM.Err.State ( HasErrorState(errorState) )
import CMM.Parser.ASTError (registerASTError)
import safe CMM.Inference.Preprocess.Error
    ( PreprocessError(NotImplemented, UndefinedForeign, LVNotFound) )
import safe CMM.AST.Wrap ( MakeWrapped(makeWrapped) )

class Preprocess n a b where
  preprocess ::
       (WithElaboration a b, GetPos a) => Annot n a -> Preprocessor (Annot n b)
  preprocess =
    \case
      Annot n a -> preprocessFinalize a $ preprocessImpl a n
  preprocessImpl ::
       (WithElaboration a b, GetPos a) => a -> n a -> Preprocessor (Elaboration, n b)

preprocessTrivial :: (Functor n, WithElaboration a b) => n a -> n b
preprocessTrivial = fmap withEmptyElaboration

preprocessFinalize ::
     (Functor m, WithElaboration a b) => a -> m (Elaboration, n b) -> m (Annot n b)
preprocessFinalize a preprocessed =
  preprocessed <&> \(handle, n') -> withElaboratedAnnot handle a n'

data PreprocessHint =
  PreprocessHint

type instance Constraint PreprocessHint a b =
     (WithElaboration a b, GetPos a)

type instance Space PreprocessHint = Preprocess'

class Preprocess' a b n where
  preprocess' :: (WithElaboration a b, GetPos a) => n a -> Preprocessor (n b)

instance WithElaboration a b => Preprocess' a b AST.Name where
  preprocess' = return . preprocessTrivial

instance Preprocess n a b => Preprocess' a b (Annot n) where
  preprocess' = preprocess

pass ::
     (ASTmap PreprocessHint n a b, WithElaboration a b, GetPos a)
  => n a
  -> Preprocessor (n b)
pass = astMapM PreprocessHint preprocess'

instance {-# OVERLAPPABLE #-} ASTmap PreprocessHint n a b =>
                              Preprocess n a b where
  preprocessImpl _ = fmap (EmptyElaboration, ) . pass

preprocessT ::
     (Preprocess n a b, WithElaboration a b, Traversable t, GetPos a)
  => t (Annot n a)
  -> Preprocessor (t (Annot n b))
preprocessT = traverse preprocess

withElaboratedAnnot :: WithElaboration a b => Elaboration -> a -> n b -> Annot n b
withElaboratedAnnot = (withAnnot .) . withElaboration

purePreprocess ::
     (WithElaboration a b, Functor n)
  => Elaboration
  -> n a
  -> Preprocessor (Elaboration, n b)
purePreprocess hole = return . (hole, ) . (withElaboration hole <$>)

instance Preprocess AST.Unit a b where
  preprocessImpl _ unit@(AST.Unit topLevels) = do
    State.storeFacts . factComment $ "START Preprocessing unit"
    let collector = globalVariables unit
        fVars = collector ^. funcVariables
        fIVars = collector ^. funcInstVariables
        sMems = collector ^. structMembers
    errorState <>= collector ^. errorState
    State.beginUnit collector
    State.storeFacts . factComment $ "Adding built-ins "
    State.storeFacts builtInTypeFacts
    let storeFacts' var =
          State.storeFacts
            [ constExprConstraint var
            , unstorableConstraint var
            ]
    for_ (Map.keys fIVars) $ State.lookupFIVar >=> storeFacts'
    for_ (Map.keys fVars) $ State.lookupFVar >=> storeFacts'
    for_ (Map.keys sMems) $ State.lookupSMem >=> storeFacts'
    topLevels' <- preprocessT topLevels
    State.storeFacts . factComment $ "END Preprocessing unit"
    return (EmptyElaboration, AST.Unit topLevels')

instance Preprocess AST.Section a b where
  preprocessImpl _ =
    ((EmptyElaboration, ) <$>) . \case
      AST.SecDecl decl -> AST.SecDecl <$> preprocess decl
      AST.SecProcedure procedure -> AST.SecProcedure <$> preprocess procedure
      AST.SecDatum datum -> AST.SecDatum <$> preprocess datum
      AST.SecSpan key value sectionItems -> do
        (key', value') <- preprocessSpanCommon key value
        sectionItems' <- preprocessT sectionItems
        return $ AST.SecSpan key' value' sectionItems'

preprocessSpanCommon ::
     (WithElaboration a b, GetPos a)
  => Annot AST.Expr a
  -> Annot AST.Expr a
  -> Preprocessor (Annot AST.Expr b, Annot AST.Expr b)
preprocessSpanCommon key value = do
  key' <- preprocess key
  value' <- preprocess value
  State.storeFacts
    [constExprConstraint key', linkExprConstraint value', value' `subType` key']
  return (key', value')

instance Preprocess AST.Decl a b where
  preprocessImpl _ =
    fmap (EmptyElaboration, ) . \case
      AST.ImportDecl imports -> AST.ImportDecl <$> preprocessT imports
      AST.ExportDecl exports -> AST.ExportDecl <$> preprocessT exports
      AST.RegDecl invar registers -> AST.RegDecl invar <$> preprocess registers
      AST.PragmaDecl name pragma ->
        AST.PragmaDecl (preprocessTrivial name) <$> preprocess pragma
      AST.TargetDecl targetDirectives -> AST.TargetDecl <$> preprocessT targetDirectives
      -- the constant is typed implicitly
      AST.ConstDecl Nothing name expr -> do
        expr' <- preprocess expr
        State.storeVar (getName name) (toType expr')
        return $ AST.ConstDecl Nothing (preprocessTrivial name) expr'
      -- the constant is typed explicitly
      AST.ConstDecl (Just type') name expr -> do
        handle <- State.lookupVar name
        expr' <- preprocess expr
        type'' <- preprocess type'
        State.storeFacts
          [ type'' `subType` handle
          , constExprConstraint handle
          , handle `subType` expr'
          ]
        return $ AST.ConstDecl (Just type'') (preprocessTrivial name) expr'
      AST.TypedefDecl type' names -> do
        type'' <- preprocess type'
        let typeType = toType type''
        traverse_ (`State.storeTCon` typeType) (getName <$> names)
        return $ AST.TypedefDecl type'' (preprocessTrivial <$> names)

instance Preprocess AST.Struct a b where
  preprocessImpl _ struct@(AST.Struct paraName datums) = do
    State.storeFacts . factComment $
      "START Preprocessing struct " <> backQuote (getName paraName)
    tVar <- State.lookupTCon (getName paraName) <&> toTypeVar
    State.pushParent tVar
    State.pushTypeVariables $ structVariables struct
    (constraint, paraName') <- preprocessParaName State.lookupTCon Star paraName
    let hole = getElaboration paraName'
    State.pushStruct (getName paraName, hole) (getName paraName, constraint)
    datums' <- preprocessDatums datums
    State.storeFacts . factComment $
      "END Preprocessing struct " <> backQuote (getName paraName)
    (hole, AST.Struct paraName' datums') <$ State.popContext <* State.popTypeVariables <*
      State.popParent

preprocessClassCommon ::
     ( PreprocessParam param1 a1 b1
     , PreprocessParam param2 a2 b2
     , WithElaboration a1 b1
     , WithElaboration a2 b2
     , WithElaboration a3 b3
     , Traversable t1
     , GetPos a1
     , GetPos a2
     , GetPos a3
     , Preprocess n a3 b3
     )
  => (Text -> Elaboration -> (Text, Infer.Type) -> [(Text, Infer.Type)] -> Preprocessor ())
  -> ([Annot (AST.ParaName param1) b1] -> Annot (AST.ParaName param2) b2 -> t1 (Annot n b3) -> t2)
  -> [Annot (AST.ParaName param1) a1]
  -> Annot (AST.ParaName param2) a2
  -> t1 (Annot n a3)
  -> CollectorState
  -> Preprocessor (Elaboration, t2)
preprocessClassCommon pushWhat constr paraNames paraName methods collector = do
  State.freshStar >>= State.pushParent . toTypeVar
  State.pushTypeVariables collector
  (constraints, paraNames') <-
    unzip <$> traverse (preprocessParaName State.lookupClass Constraint) paraNames
  (constraint, paraName') <- preprocessParaName State.lookupClass Constraint paraName
  pushWhat
    (getName paraName)
    (getElaboration paraName')
    (getName paraName, constraint)
    (fmap getName paraNames `zip` constraints)
  constr' <- preprocessT methods <&> constr paraNames' paraName'
  (getElaboration paraName', constr') <$
    State.popContext <*
    State.popTypeVariables <*
    State.popParent

preprocessFunDep :: Annot (AST.ParaName AST.Name) a
  -> Annot AST.FunDep a -> [T.Trilean]
preprocessFunDep (AST.ParaName _ params `Annot` _) = \case
  AST.FunDep {AST.fdFrom, AST.fdTo} `Annot` _ -> params'
    where
      params' = goParam . getName <$> params
      goParam param
        | param `Set.member` from = T.False
        | param `Set.member` to = T.True
        | otherwise = T.Unknown
      from = Set.fromList $ getName <$> fdFrom
      to = Set.fromList $ getName <$> fdTo

preprocessFunDeps :: Annot (AST.ParaName AST.Name) a
  -> Annot AST.FunDeps a -> Preprocessor ()
preprocessFunDeps paraName = \case
  AST.FunDeps funDeps `Annot` _ ->
      State.storeFact $ classFunDeps (getName paraName) funDeps'
      where funDeps' = preprocessFunDep paraName <$> funDeps

instance Preprocess AST.Class a b where
  preprocessImpl _ class'@(AST.Class paraNames paraName mFunDeps methods) = do
    State.storeFacts . factComment $
      "START Preprocessing class " <> backQuote name
    preprocessFunDeps paraName `traverse_` mFunDeps
    let
      constr pNames pName ms = AST.Class pNames pName (fmap (withElaboration EmptyElaboration) <$> mFunDeps) ms
    result <-
      preprocessClassCommon State.pushClass constr paraNames paraName methods $
      classVariables class'
    State.storeFacts . factComment $
      "END Preprocessing class " <> backQuote name
    return result
    where
      name = getName paraName

instance Preprocess AST.Instance a b where
  preprocessImpl _ instance'@(AST.Instance paraNames paraName methods) = do
    State.storeFacts . factComment $
      "START Preprocessing instance " <> backQuote (getName paraName)
    result <-
      preprocessClassCommon State.pushInstance constr paraNames paraName methods $
      instanceVariables instance'
    State.storeFacts . factComment $
      "END Preprocessing instance " <> backQuote (getName paraName)
    return result
    where constr pNames pName ms = AST.Instance pNames pName ms

class Preprocess param a b =>
      PreprocessParam param a b
  where
  preprocessParam ::
       (WithElaboration a b, GetPos a)
    => Annot param a
    -> Preprocessor (Annot param b)

instance PreprocessParam AST.Name a b where
  preprocessParam name = do
    handle <- State.lookupTVar name
    return $ withElaboration handle <$> name

instance PreprocessParam AST.Type a b where
  preprocessParam = preprocess

preprocessParaName ::
     (WithElaboration a b, PreprocessParam param a b, GetPos a)
  => (Text -> Preprocessor Elaboration)
  -> TypeKind
  -> Annot (AST.ParaName param) a
  -> Preprocessor (Infer.Type, Annot (AST.ParaName param) b)
preprocessParaName looker kind (Annot (AST.ParaName name params) annot) = do
  let name' = preprocessTrivial name
  params' <- traverse preprocessParam params
  class' <- looker $ getName name'
  handle <- State.freshNamedASTTypeHandle (getName name) annot kind
  let constraint = foldApp $ toType class' : fmap toType params'
  State.storeFact $ handle `typeEquality` constraint
  return
    ( constraint
    , withElaboration (SimpleElaboration handle) annot `withAnnot`
      AST.ParaName name' params')

instance Preprocess AST.Import a b where
  preprocessImpl _ import'@AST.Import {} = do
    handle <- State.lookupVar import'
    purePreprocess handle import'

instance Preprocess AST.Export a b where
  preprocessImpl _ export@AST.Export {} = do
    handle <- State.lookupVar export
    purePreprocess handle export

instance Preprocess AST.Type a b where
  preprocessImpl annot t =
    case t of
      AST.TBits int -> trivialCase $ makeTBitsType int
      AST.TName name -> do
        hole <- State.lookupTCon name
        purePreprocess hole t
      AST.TAuto Nothing -> do
        handle <- State.freshASTGeneric annot
        purePreprocess (SimpleElaboration handle) t
      AST.TAuto (Just name) -> do
        hole <- State.lookupTVar name
        purePreprocess hole t
      AST.TPar parType -> do
        parType' <- preprocess parType
        return (getElaboration parType', AST.TPar parType')
      AST.TPtr t' -> do
        ptr <- State.freshASTStar annot
        t'' <- preprocess t'
        State.storeFact $ ptr `typeEquality` makeAddrType t''
        return (SimpleElaboration ptr, AST.TPtr t'')
      AST.TVoid -> trivialCase makeVoidType
      AST.TBool -> trivialCase makeBoolType
      AST.TLabel -> trivialCase makeLabelType
      where
        trivialCase type' = do
          handle <- State.freshASTStar annot
          State.storeFact $ handle `typeEquality` type'
          purePreprocess (SimpleElaboration handle) t

instance Preprocess AST.ParaType a b where
  preprocessImpl annot (AST.ParaType type' types') = do
    type'' <- preprocess type'
    types'' <- traverse preprocess types'
    handle <- State.freshASTGeneric annot
    State.storeFact . typeEquality handle . foldApp $ toType type'' : fmap toType types''
    return (SimpleElaboration handle, AST.ParaType type'' types'')

maybeKindUnif ::
     (ToType t, ToType t', GetName n) => Maybe n -> t -> t' -> Preprocessor ()
maybeKindUnif mKind derived base =
  State.storeFacts $
  case mKind of
    Nothing ->
      [ derived `kindEquality` base
      , base `subConst` derived
      , base `typingEquality` derived
      ]
    Just kind ->
      [ derived `typingEquality` base
      , base `subConst` derived
      , getDataKind (getName kind) `kindConstraint` derived
      ]

instance Preprocess AST.Registers a b where
  preprocessImpl _ (AST.Registers mKind type' nameStrLits) = do
    type'' <- preprocess type'
    let go (name, mStrLit) = do
          hole <- State.lookupVar name
          maybeKindUnif mKind hole type''
          case mStrLit of
            Nothing -> return (withElaboration hole <$> name, Nothing)
            Just (AST.StrLit strLit) -> do
              State.storeFact $ strLit `registerConstraint` hole
              return (withElaboration hole <$> name, Just (AST.StrLit strLit))
    nameStrLits' <- traverse go nameStrLits
    return (EmptyElaboration, AST.Registers mKind type'' nameStrLits')

class FormalNames a where
  formalNames :: a -> [Text]

instance FormalNames (n a) => FormalNames (Annot n a) where
  formalNames = formalNames . unAnnot

instance FormalNames (AST.Formal a) where
  formalNames = pure . getName

instance FormalNames (AST.ProcedureHeader a) where
  formalNames (AST.ProcedureHeader _ _ formals _) = getName <$> formals

instance FormalNames (AST.ProcedureDecl a) where
  formalNames (AST.ProcedureDecl header) = formalNames header

instance FormalNames (AST.Procedure a) where
  formalNames (AST.Procedure header _) = formalNames header

preprocessHeader ::
     (WithElaboration a b, GetPos a)
  => Annot AST.ProcedureHeader a
  -> Preprocessor (Annot AST.ProcedureHeader b)
preprocessHeader header =
  preprocessFinalize (takeAnnot header) $
  preprocessProcedureHeaderImpl header

preprocessProcedureHeaderImpl ::
     (WithElaboration a b, GetPos a)
  => Annot AST.ProcedureHeader a
  -> Preprocessor (Elaboration, AST.ProcedureHeader b)
preprocessProcedureHeaderImpl (AST.ProcedureHeader mConv name formals mTypes `Annot` pos) = do
  formals' <- doOutsideCtx $ preprocessT formals
  mTypes' <- doOutsideCtx $ traverse preprocessT mTypes
  case mConv of
    Just (AST.Foreign (AST.StrLit conv))
      | conv == "C" -> do
        State.storeFacts $ regularExprConstraint <$> formals'
        State.storeCSymbol $ getName name
      | otherwise -> registerASTError pos $ UndefinedForeign conv
    Nothing -> return ()
  mTypes' `for_` \types -> do
    retHandle <- State.getCurrentReturn
    retType'@(~(TupleType retVars)) <-
      doOutsideCtx $ makeTuple <$> traverse State.freshASTStar (fromJust mTypes)
    State.storeFact $ retHandle `typeEquality` retType'
    case mConv of
      Just (AST.Foreign (AST.StrLit conv))
        | conv == "C" -> do State.storeFacts $ regularExprConstraint <$> retVars
        | otherwise -> registerASTError pos $ UndefinedForeign conv
      Nothing -> return ()
    zipWithM_ ((State.storeFact .) . typeEquality) types retVars
  (fs, retType) <- (_2 %~ toType) <$> State.endProc
  let procedureType = fmap toType formals' `makeFunction` retType
  State.storeFacts
    [constExprConstraint procedureType, unstorableConstraint procedureType]
  hole <- State.storeProc (getName name) fs procedureType
  return (hole, AST.ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

preprocessProcedureCommon ::
     (GetName proc, GetPos a, GetConv proc)
  => proc
  -> Annot AST.ProcedureHeader a
  -> Preprocessor ()
preprocessProcedureCommon procedure header = do
  hole <- State.lookupCtxFVar $ getName procedure
  adoption <- makeAdoption hole
  let hole' = adoption hole
  State.pushParent $ toTypeVar hole'
  State.beginProc
    (getName procedure)
    hole'
    (localVariables header)
    (getConv procedure)

instance Preprocess AST.Procedure a b where
  preprocessImpl _ procedure@(AST.Procedure header body) = do
    State.storeFacts . factComment $
      "START Preprocessing procedure " <> backQuote (getName header)
    preprocessProcedureCommon procedure header
    State.openProc $ localVariables body
    body' <- preprocess body
    header' <- preprocessHeader header
    State.storeFacts . factComment $
      "END Preprocessing procedure " <> backQuote (getName header)
    (getElaboration header', AST.Procedure header' body') <$ State.popParent

instance Preprocess AST.ProcedureDecl a b where
  preprocessImpl _ procedure@(AST.ProcedureDecl header) = do
    preprocessProcedureCommon procedure header
    ((EmptyElaboration, ) . AST.ProcedureDecl <$> preprocessHeader header) <* State.popParent

instance Preprocess AST.Formal a b where
  preprocessImpl _ (AST.Formal mKind invar type' name) = do
    hole <- State.lookupVar name
    type'' <- preprocess type'
    maybeKindUnif mKind hole type''
    return (hole, AST.Formal mKind invar type'' (preprocessTrivial name))

instance Preprocess AST.SemiFormal a b where
  preprocessImpl _ (AST.SemiFormal mKind type') = do
    handle <- State.freshStar
    type'' <- preprocess type'
    maybeKindUnif mKind handle type''
    return (SimpleElaboration handle, AST.SemiFormal mKind type'')

instance Preprocess AST.StackDecl a b where
  preprocessImpl _ (AST.StackDecl datums) =
    (EmptyElaboration, ) . AST.StackDecl <$> preprocessDatums datums

instance Preprocess AST.Stmt a b where
  preprocessImpl pos stmt = case stmt of
      AST.EmptyStmt -> purePreprocess EmptyElaboration AST.EmptyStmt
      AST.IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        State.storeFacts
          [ cond' `typingEquality` makeBoolType
          , boolKind `minKindConstraint` cond'
          ]
        (EmptyElaboration, ) <$>
          liftA2 (AST.IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      AST.SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        arms' <- preprocessT arms
        traverse_ (State.storeFact . subType scrutinee') arms'
        return (EmptyElaboration, AST.SwitchStmt scrutinee' arms')
      AST.SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (EmptyElaboration, AST.SpanStmt key' value' body')
      AST.AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        zipWithM_ ((State.storeFact .) . subType) lvalues' exprs'
        return (EmptyElaboration, AST.AssignStmt lvalues' exprs')
      AST.PrimOpStmt {} -> notImplemented pos stmt
      AST.CallStmt names mConv expr actuals mTargets annots
       -> do
        retTypes <- names `for` \name -> State.freshNamedASTStar (getName name) name
        argTypes <-
          zip [0 :: Int ..] actuals `for` \(num, actual) ->
            State.freshNamedASTStar (T.pack $ "actual" <> show num) actual
        names' <- preprocessT names
        expr' <- preprocess expr
        actuals' <- preprocessT actuals
        mTargets' <- preprocessT mTargets
        annots' <- preprocessT annots
        State.storeFact . typeEquality expr' . makeAddrType $
          makeFunction (toType <$> argTypes) (toType $ makeTuple retTypes)
        zipWithM_ ((State.storeFact .) . subType) names' retTypes
        zipWithM_ ((State.storeFact .) . subType) argTypes actuals'
        return
          ( EmptyElaboration
          , AST.CallStmt names' mConv expr' actuals' mTargets' annots')
      AST.JumpStmt {} -> notImplemented pos stmt
      AST.ReturnStmt mConv Nothing actuals
       -> do
        actuals' <- preprocessT actuals
        retType@(~(TupleType retVars)) <-
          doOutsideCtx $ makeTuple <$> traverse State.freshASTStar actuals
        zipWithM_ ((State.storeFact .) . subType) retVars actuals'
        State.getCtxMConv >>= \case
          Just (AST.Foreign (AST.StrLit conv))
            | conv == "C" -> do State.storeFacts $ regularExprConstraint <$> retVars
            | otherwise -> registerASTError pos $ UndefinedForeign conv
          Nothing -> return ()
        State.getCurrentReturn >>= State.storeFact . (`typeEquality` retType)
        return (EmptyElaboration, AST.ReturnStmt mConv Nothing actuals')
      AST.ReturnStmt {} -> notImplemented pos stmt
      label@AST.LabelStmt {} -> do
        hole <- State.lookupVar label
        State.storeFacts
          [ addressKind `kindConstraint` hole
          , hole `typingEquality` makeLabelType
          , constExprConstraint hole
          ]
        purePreprocess hole label
      AST.ContStmt {} -> notImplemented pos stmt
      AST.GotoStmt expr mTargets
       -> do
        expr' <- preprocess expr
        State.storeFacts
          [ addressKind `minKindConstraint` expr'
          , expr' `typingEquality` makeLabelType
          ]
        (EmptyElaboration, ) . AST.GotoStmt expr' <$> preprocessT mTargets
      AST.CutToStmt {} -> notImplemented pos stmt

notImplemented :: (GetPos a, MakeWrapped n, WithElaboration a b, Functor n) =>
  a -> n a -> Preprocessor (Elaboration, n b)
notImplemented pos node = do
  registerASTError pos . NotImplemented . makeWrapped $ void node
  return . (EmptyElaboration,) $  preprocessTrivial node

doOutsideCtx :: Preprocessor a -> Preprocessor a
doOutsideCtx action = State.popTopContext >>= (action <*) . State.pushContext

instance Preprocess AST.KindName a b where
  preprocessImpl annot (AST.KindName mKind name) = do
    nameType <- State.lookupVar name
    handle <- State.freshNamedASTStar (getName name) annot
    maybeKindUnif mKind handle nameType
    return (SimpleElaboration handle, AST.KindName mKind (preprocessTrivial name))

instance Preprocess AST.Arm a b where
  preprocessImpl = notImplemented

instance Preprocess AST.Targets a b where
  preprocessImpl = notImplemented

instance Preprocess AST.Lit a b where
  preprocessImpl annot lit = do
    handle <- State.freshASTStar annot
    State.storeFact . (`minKindConstraint` handle) $
      case lit of
        AST.LitInt {} -> integerKind
        AST.LitFloat {} -> floatKind
        AST.LitChar {} -> integerKind
    State.storeFact $ constExprConstraint handle
    purePreprocess (SimpleElaboration handle) lit

instance Preprocess AST.Actual a b where
  preprocessImpl annot (AST.Actual mKind expr) = do
    expr' <- preprocess expr
    handle <- State.freshASTStar annot
    maybeKindUnif mKind handle expr'
    return (SimpleElaboration handle, AST.Actual mKind expr')

unionizeTypes :: ToType a => [a] -> Preprocessor ()
unionizeTypes =
  \case
    [] -> return ()
    tVar:tVars -> traverse_ (State.storeFact . typeEquality tVar) tVars

instance Preprocess AST.Init a b where
  preprocessImpl annot =
    \case
      AST.ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- State.freshASTStar annot
        State.storeFacts $ constExprConstraint <$> exprs'
        State.storeFacts $ (`subType` handle) <$> exprs'
        return (SimpleElaboration handle, AST.ExprInit exprs')
      strInit@AST.StrInit {} -> strInitCommon StringType strInit
      strInit@AST.Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon :: TypeCompl Type -> AST.Init a -> Preprocessor (Elaboration, AST.Init b)
      strInitCommon c strInit = do
        handle <- State.freshStar
        State.storeFact $ handle `typingEquality` ComplType c
        purePreprocess (SimpleElaboration handle) strInit

preprocessDatums ::
     (WithElaboration a b, GetPos a)
  => [Annot AST.Datum a]
  -> Preprocessor [Annot AST.Datum b]
preprocessDatums datums = do
  datumResults <- preprocessDatumsImpl [] datums
  zipWithM preprocessFinalize (takeAnnot <$> datums) (return <$> datumResults)

preprocessDatumsImpl ::
     (GetPos a, WithElaboration a b)
  => [Elaboration]
  -> [Annotation AST.Datum a]
  -> Preprocessor [(Elaboration, AST.Datum b)]
preprocessDatumsImpl _ [] = return []
preprocessDatumsImpl cache ((Annot datum annot):others) =
  case datum of
    AST.DatumLabel name -> do
      hole <-
        uses State.currentContext head >>= \case
          StructCtx {} -> do
            hole <- State.lookupSIMem name
            mem <- State.lookupSMem name
            let name' = fieldClassHelper $ getName name
            classHole <- State.lookupClass name'
            return $
              MemberElaboration
                (eHandle hole)
                [(name', eHandle classHole)]
                [eHandle mem]
                []
          _ -> do
            tVar <- State.freshStar
            hole <- State.lookupVar name
            State.storeFacts
              [ addressKind `kindConstraint` hole
              , hole `typeEquality` makeAddrType tVar
              ]
            return hole
      purePreprocess hole datum <:> preprocessDatumsImpl (hole : cache) others
    AST.DatumAlign _ -> do
      purePreprocess EmptyElaboration datum <:> preprocessDatumsImpl [] others
    AST.Datum new type' mSize mInit -> do
      uses State.currentContext head >>= \case
        StructCtx {ctxElab, ctxConstraint = (_,structType)} -> do
          tVars <- State.collectTVars
          State.pushFacts
          (hole, datum') <- goGeneral
          fs <- State.popTopFacts
          let structPtr = makeAddrType structType
              t = makeFunction [structPtr] $ toType hole
              funcFact = Fact $ unstorableConstraint t
              constExprFact = Fact $ constExprConstraint t
              structAddrFact =
                Fact $ addressKind `kindConstraint` structPtr
              fieldAddrFact =
                Fact $ addressKind `kindConstraint` makeAddrType hole
              cache' =
                cache <&> \case
                  MemberElaboration iMem [hole''] [mem] [] -> (hole'', iMem, mem)
                  _ -> logicError
              hole' = MemberElaboration (eHandle ctxElab) `uncurry3` unzip3 cache'
              scheme (MemberElaboration h [(name, classHandle)] _ _) = do
                method <-
                  refreshNestedFact . forall tVars [h `typeEquality` t] $
                  structAddrFact :
                  fieldAddrFact : funcFact : constExprFact : fs
                subst <- refresh tVars
                fact <-
                  refreshNestedFact $
                  forall (apply subst `Set.map` tVars) [subst `apply` classF] []
                return [method, fact]
                where
                  classF = classFact name constraint
                  constraint =
                    foldApp
                      [ toType classHandle
                      , structPtr
                      , toType hole
                      ]
              scheme _ = logicError
          schemes <- traverse scheme cache
          State.storeFacts $ concat schemes
          ((hole', datum') :) <$> preprocessDatumsImpl [] others
        _ -> do
          result@(hole, _) <- goGeneral
          unionizeTypes $ hole : cache
          (result :) <$> preprocessDatumsImpl [] others
      where goGeneral = do
              handle <- State.freshASTStar annot
              type'' <- preprocess type'
              mSize' <- traverse preprocess mSize
              mInit' <- traverse preprocess mInit
              mInit' `for_` \init' -> do
                State.storeFacts [type'' `subType` init', linkExprConstraint init']
              State.storeFacts
                [ addressKind `kindConstraint` handle
                , handle `typingEquality` makeAddrType type''
                ]
              return (SimpleElaboration handle, AST.Datum new type'' mSize' mInit')

instance Preprocess AST.Datum a b where
  preprocessImpl a datum = head <$> preprocessDatumsImpl [] [datum `Annot` a]

instance Preprocess AST.Size a b where
  preprocessImpl _ =
    \case
      AST.Size (Just expr) -> do
        expr' <- preprocess expr
        State.storeFact $ constExprConstraint expr'
        return (getElaboration expr', AST.Size $ Just expr')
      size -> purePreprocess EmptyElaboration size

instance Preprocess AST.LValue a b where
  preprocessImpl annot =
    \case
      lvName@AST.LVName {} -> do
        State.lookupFVar lvName >>= \case
          EmptyElaboration -> State.lookupVar lvName >>= \case
              EmptyElaboration -> do
                let name = getName lvName
                registerASTError annot $ LVNotFound  name
                handle <- State.freshNamedASTStar name annot
                purePreprocess (SimpleElaboration handle) lvName
              hole -> hole `purePreprocess` lvName
          scheme -> do
            inst <- State.freshNamedASTStar (getName lvName) annot
            handle <- State.freshNamedASTStar (getName lvName) annot
            State.storeFacts
              [ scheme `instType` inst
              , handle `typeEquality` makeAddrType inst
              , constExprConstraint handle
              , constExprConstraint inst
              , addressKind `kindConstraint` handle
              , unstorableConstraint inst
              ]
            purePreprocess (handle `LVInstElaboration` eHandle scheme) lvName
      AST.LVRef Nothing expr mAsserts -> do
        expr' <- preprocess expr
        handle <- State.freshASTStar annot
        let mAsserts' = (withElaboration EmptyElaboration <$>) <$> mAsserts
        State.storeFacts
          [ expr' `typeEquality` makeAddrType handle
          , handle `subConst` expr'
          , addressKind `minKindConstraint` expr'
          ]
        return (SimpleElaboration handle, AST.LVRef Nothing expr' mAsserts')
      AST.LVRef (Just type') expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withElaboration EmptyElaboration <$>) <$> mAsserts
        State.storeFact $ addressKind `minKindConstraint` expr'
        return (getElaboration type'', AST.LVRef (Just type'') expr' mAsserts')

instance Preprocess AST.Expr a b where
  preprocessImpl annot
   =
    \case
      AST.MemberExpr struct field ->
        State.lookupSMem field >>= \case
          EmptyElaboration -> logicError
          scheme -> do
            struct' <- preprocess struct
            inst <- makeHandle
            handle <- makeHandle
            argType <- makeHandle
            retType <- makeHandle
            let name = fieldClassHelper $ getName field
            classHole <- State.lookupClass name
            State.storeFacts
              [ scheme `instType` inst
              , inst `typeEquality` makeFunction [argType] retType
              , classConstraint name $
                foldApp [toType classHole, toType argType, toType retType]
              , constExprConstraint inst
              , addressKind `kindConstraint` struct'
              , handle `subConst` struct'
              , retType `subType` handle
              , unstorableConstraint inst
              , addressKind `kindConstraint` handle
              , argType `subType` struct'
              ]
            return
              ( MethodElaboration handle (eHandle scheme) inst
              , AST.MemberExpr struct' $ preprocessTrivial field)
            where makeHandle = State.freshNamedASTStar (getName field) annot
      AST.ParExpr expr -> AST.ParExpr `preprocessInherit` expr
      AST.LVExpr lvalue -> AST.LVExpr `preprocessInherit` lvalue
      AST.BinOpExpr op left right
       -> do
        handle <- State.freshStar
        operator <- State.freshStar
        left' <- preprocess left
        right' <- preprocess right
        State.storeFact $
          operator `typeEquality`
          makeFunction [toType left', toType right'] (toType handle)
        State.storeFact $ unstorableConstraint operator
        State.storeFacts $
          if op `elem` [AST.EqOp, AST.NeqOp, AST.GtOp, AST.LtOp, AST.GeOp, AST.LeOp]
            then [ left' `typingEquality` right'
                 , handle `subConst` left'
                 , handle `subConst` right'
                 , handle `typingEquality` makeBoolType
                 , boolKind `kindConstraint` handle
                 ]
            else [handle `subType` left', handle `subType` right']
        return (SimpleElaboration handle, AST.BinOpExpr op left' right')
      AST.NegExpr expr -> AST.NegExpr `preprocessInherit` expr
      AST.ComExpr expr -> AST.ComExpr `preprocessInherit` expr
      AST.LitExpr lit mType -> do
        lit' <- preprocess lit
        mType' <-
          for mType $ \type' -> do
            type'' <- preprocess type'
            State.storeFacts [type'' `subType` lit', constExprConstraint lit']
            return type''
        return (getElaboration lit', AST.LitExpr lit' mType')
      AST.PrefixExpr name actuals -> do
        (handle, tupleType) <- fixCommon name
        actuals' <- preprocessT actuals
        State.storeFact $ tupleType `typeEquality` makeTuple actuals'
        return
          (SimpleElaboration handle, AST.PrefixExpr (preprocessTrivial name) actuals')
      AST.InfixExpr name left right -> do
        (handle, tupleType) <- fixCommon name
        left' <- preprocess left
        right' <- preprocess right
        State.storeFact $ tupleType `typeEquality` makeTuple [left', right']
        return
          ( SimpleElaboration handle
          , AST.InfixExpr (preprocessTrivial name) left' right')
    where
      fixCommon name = do
        handle <- State.freshStar
        tupleType <- State.freshStar
        argType <- State.freshStar
        retType <- State.freshStar
        fType <- State.freshStar
        opScheme <- State.freshStar
        State.storeFacts
          [ fType `typeEquality` makeFunction [argType] retType
          , opScheme `typeEquality` getNamedOperator (getName name)
          , opScheme `instType` fType
          , argType `subType` tupleType
          , handle `subType` retType
          , unstorableConstraint tupleType
          ]
        return (handle, tupleType)
      preprocessInherit :: Preprocess m a b => (Annot m b -> n b) -> Annot m a -> Preprocessor (Elaboration, n b)
      preprocessInherit c n = do
        n' <- preprocess n
        return (getElaboration n', c n')

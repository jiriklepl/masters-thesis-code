{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: add kinds and constnesses where they make sense
-- TODO: all types of things inside procedures should be subtypes of the return type
module CMM.Inference.Preprocess where

import safe Control.Applicative (Applicative((<*), pure), liftA2)
import safe Control.Lens.Getter ((^.), uses)
import safe Control.Lens.Setter ((%~), (<>=))
import safe Control.Lens.Tuple (_2)
import safe Control.Monad (Monad((>>=), return), (>=>), zipWithM, zipWithM_)
import safe Data.Bool (otherwise)
import safe Data.Eq (Eq((==)))
import safe Data.Foldable (concat, for_, traverse_)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$), (<$>), (<&>))
import safe Data.Int (Int)
import safe Data.List (elem, head, unzip, unzip3, zip)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), fromJust)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Data.Traversable (Traversable(traverse), for)
import safe Data.Tuple.Extra (uncurry3)
import safe GHC.Err (undefined)
import safe Prettyprinter ((<>))
import safe Text.Show (Show(show))

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
  , functionKind
  , instType
  , kindConstraint
  , kindUnion
  , linkExprConstraint
  , minKindConstraint
  , registerConstraint
  , regularExprConstraint
  , subConst
  , subType
  , typeConstraint
  , typeUnion
  , unstorableConstraint
  )
import safe CMM.Inference.GetParent (makeAdoption)
import safe CMM.Inference.HandleCounter (nextHandleCounter)
import safe CMM.Inference.Preprocess.Context (Context(StructCtx, ctxConstraint, ctxHole))
import safe CMM.Inference.Preprocess.State as State
  ( Preprocessor)
import safe qualified CMM.Inference.Preprocess.State as State
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , TypeHole(EmptyTypeHole, LVInstTypeHole, MemberTypeHole,
         MethodTypeHole, SimpleTypeHole)
  , holeHandle
  )
import safe CMM.Inference.Preprocess.WithTypeHole
  ( WithTypeHole(withTypeHole)
  , withEmptyTypeHole
  )
import safe CMM.Inference.Refresh (Refresher(refresher), refreshNestedFact)
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type as Infer
  ( ToType(toType)
  , Type(ComplType)
  , foldApp
  , makeAddrType
  , makeTBitsType
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(BoolType, LabelType, String16Type, StringType, TupleType)
  , makeFunction
  , makeTuple
  )
import safe CMM.Inference.TypeHandle ()
import safe CMM.Inference.TypeKind (TypeKind(Constraint, Star))
import safe CMM.Inference.TypeVar (ToTypeVar(toTypeVar), TypeVar(tVarId))
import safe CMM.Inference.Utils (fieldClassHelper)
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Utils (backQuote)
import safe CMM.Err.State ( HasErrorState(errorState) )

-- TODO: check everywhere whether propagating types correctly (via subtyping)
-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
class Preprocess n a b where
  preprocess ::
       (WithTypeHole a b, HasPos a) => Annot n a -> Preprocessor (Annot n b)
  preprocess =
    \case
      Annot n a -> preprocessFinalize a $ preprocessImpl a n
  preprocessImpl ::
       (WithTypeHole a b, HasPos a) => a -> n a -> Preprocessor (TypeHole, n b)

preprocessTrivial :: (Functor n, WithTypeHole a b) => n a -> n b
preprocessTrivial = fmap withEmptyTypeHole

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

instance WithTypeHole a b => Preprocess' a b AST.Name where
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
    -- TODO: rename this
    let storeFacts' var =
          State.storeFacts
            [ constExprConstraint var
            , tVarId (toTypeVar var) `functionKind` var -- TODO: think this through
            ]
    for_ (Map.keys fIVars) $ State.lookupFIVar >=> storeFacts'
    for_ (Map.keys fVars) $ State.lookupFVar >=> storeFacts'
    for_ (Map.keys sMems) $ State.lookupSMem >=> storeFacts'
    topLevels' <- preprocessT topLevels
    State.storeFacts . factComment $ "END Preprocessing unit"
    return (EmptyTypeHole, AST.Unit topLevels')

instance Preprocess AST.Section a b where
  preprocessImpl _ =
    ((EmptyTypeHole, ) <$>) . \case
      AST.SecDecl decl -> AST.SecDecl <$> preprocess decl
      AST.SecProcedure procedure -> AST.SecProcedure <$> preprocess procedure
      AST.SecDatum datum -> AST.SecDatum <$> preprocess datum
      AST.SecSpan key value sectionItems -> do
        (key', value') <- preprocessSpanCommon key value
        sectionItems' <- preprocessT sectionItems
        return $ AST.SecSpan key' value' sectionItems'

preprocessSpanCommon ::
     (WithTypeHole a b, HasPos a)
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
    fmap (EmptyTypeHole, ) . \case
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

-- TODO: continue from here
instance Preprocess AST.Struct a b where
  preprocessImpl _ struct@(AST.Struct paraName datums) = do
    State.storeFacts . factComment $
      "START Preprocessing struct " <> backQuote (getName paraName)
    tVar <- State.lookupTCon (getName paraName) <&> toTypeVar
    State.pushParent tVar
    State.pushTypeVariables $ structVariables struct
    (constraint, paraName') <- preprocessParaName State.lookupTCon Star paraName
    let hole = getTypeHole paraName'
    State.pushStruct (getName paraName, hole) (getName paraName, constraint)
    datums' <- preprocessDatums datums
    State.storeFacts . factComment $
      "END Preprocessing struct " <> backQuote (getName paraName)
    (hole, AST.Struct paraName' datums') <$ State.popContext <* State.popTypeVariables <*
      State.popParent

preprocessClassCommon ::
     ( PreprocessParam param1 a1 b1
     , PreprocessParam param2 a2 b2
     , WithTypeHole a1 b1
     , WithTypeHole a2 b2
     , WithTypeHole a3 b3
     , Traversable t1
     , HasPos a1
     , HasPos a2
     , HasPos a3
     , Preprocess n a3 b3
     )
  => (Text -> TypeHole -> (Text, Infer.Type) -> [(Text, Infer.Type)] -> Preprocessor ())
  -> ([Annot (AST.ParaName param1) b1] -> Annot (AST.ParaName param2) b2 -> t1 (Annot n b3) -> t2)
  -> [Annot (AST.ParaName param1) a1]
  -> Annot (AST.ParaName param2) a2
  -> t1 (Annot n a3)
  -> CollectorState
  -> Preprocessor (TypeHole, t2)
preprocessClassCommon pushWhat constr paraNames paraName methods collector = do
  State.freshStar >>= State.pushParent . toTypeVar
  State.pushTypeVariables collector
  (constraints, paraNames') <-
    unzip <$> traverse (preprocessParaName State.lookupClass Constraint) paraNames
  (constraint, paraName') <- preprocessParaName State.lookupClass Constraint paraName
  pushWhat
    (getName paraName)
    (getTypeHole paraName')
    (getName paraName, constraint)
    (fmap getName paraNames `zip` constraints)
  (getTypeHole paraName', ) . constr paraNames' paraName' <$>
    preprocessT methods <*
    State.popContext <*
    State.popTypeVariables <*
    State.popParent

instance Preprocess AST.Class a b where
  preprocessImpl _ class'@(AST.Class paraNames paraName methods) = do
    State.storeFacts . factComment $
      "START Preprocessing class " <> backQuote (getName paraName)
    result <-
      preprocessClassCommon State.pushClass AST.Class paraNames paraName methods $
      classVariables class'
    State.storeFacts . factComment $
      "END Preprocessing class " <> backQuote (getName paraName)
    return result

instance Preprocess AST.Instance a b where
  preprocessImpl _ instance'@(AST.Instance paraNames paraName methods) = do
    State.storeFacts . factComment $
      "START Preprocessing instance " <> backQuote (getName paraName)
    result <-
      preprocessClassCommon State.pushInstance AST.Instance paraNames paraName methods $
      instanceVariables instance'
    State.storeFacts . factComment $
      "END Preprocessing instance " <> backQuote (getName paraName)
    return result

class Preprocess param a b =>
      PreprocessParam param a b
  where
  preprocessParam ::
       (WithTypeHole a b, HasPos a)
    => Annot param a
    -> Preprocessor (Annot param b)

instance PreprocessParam AST.Name a b where
  preprocessParam name = do
    handle <- State.lookupTVar name
    return $ withTypeHole handle <$> name

instance PreprocessParam AST.Type a b where
  preprocessParam = preprocess

preprocessParaName ::
     (WithTypeHole a b, PreprocessParam param a b, HasPos a)
  => (Text -> Preprocessor TypeHole)
  -> TypeKind
  -> Annot (AST.ParaName param) a
  -> Preprocessor (Infer.Type, Annot (AST.ParaName param) b)
preprocessParaName looker kind (Annot (AST.ParaName name params) annot) = do
  let name' = preprocessTrivial name
  params' <- traverse preprocessParam params
  class' <- looker $ getName name'
  handle <- State.freshNamedASTTypeHandle (getName name) annot kind
  let constraint = foldApp $ toType class' : fmap toType params'
  State.storeFact $ handle `typeUnion` constraint
  return
    ( constraint
    , withTypeHole (SimpleTypeHole handle) annot `withAnnot`
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
      AST.TBits int -> do
        handle <- State.freshASTStar annot
        State.storeFact $ handle `typeUnion` makeTBitsType int
        purePreprocess (SimpleTypeHole handle) t
      AST.TName name -> do
        hole <- State.lookupTCon name
        purePreprocess hole t
      AST.TAuto Nothing -> do
        handle <- State.freshASTGeneric annot
        purePreprocess (SimpleTypeHole handle) t
      AST.TAuto (Just name) -> do
        hole <- State.lookupTVar name
        purePreprocess hole t
      AST.TPar parType -> do
        parType' <- preprocess parType
        return (getTypeHole parType', AST.TPar parType')

instance Preprocess AST.ParaType a b where
  preprocessImpl annot (AST.ParaType type' types') = do
    type'' <- preprocess type'
    types'' <- traverse preprocess types'
    handle <- State.freshASTGeneric annot
    State.storeFact . typeUnion handle . foldApp $ toType type'' : fmap toType types''
    return (SimpleTypeHole handle, AST.ParaType type'' types'')

maybeKindUnif ::
     (ToType t, ToType t', GetName n) => Maybe n -> t -> t' -> Preprocessor ()
maybeKindUnif mKind derived base =
  State.storeFacts $
  case mKind of
    Nothing ->
      [ derived `kindUnion` base
      , base `subConst` derived
      , base `typeConstraint` derived
      ]
    Just kind ->
      [ derived `typeConstraint` base
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
            Nothing -> return (withTypeHole hole <$> name, Nothing)
            Just (AST.StrLit strLit) -> do
              State.storeFact $ strLit `registerConstraint` hole
              return (withTypeHole hole <$> name, Just (AST.StrLit strLit))
    nameStrLits' <- traverse go nameStrLits
    return (EmptyTypeHole, AST.Registers mKind type'' nameStrLits')

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
     (WithTypeHole a b, HasPos a)
  => Annot AST.ProcedureHeader a
  -> Preprocessor (Annot AST.ProcedureHeader b)
preprocessHeader header =
  preprocessFinalize (takeAnnot header) $
  preprocessProcedureHeaderImpl (unAnnot header)

-- TODO: consult conventions with man
preprocessProcedureHeaderImpl ::
     (WithTypeHole a b, HasPos a)
  => AST.ProcedureHeader a
  -> Preprocessor (TypeHole, AST.ProcedureHeader b)
preprocessProcedureHeaderImpl (AST.ProcedureHeader mConv name formals mTypes) = do
  formals' <- doOutsideCtx $ preprocessT formals
  mTypes' <- doOutsideCtx $ traverse preprocessT mTypes
  case mConv of
    Just (AST.Foreign (AST.StrLit conv))
      | conv == "C" -> do
        State.storeFacts $ regularExprConstraint <$> formals'
        State.storeCSymbol $ getName name
      | otherwise -> undefined -- TODO: UB
    Nothing -> return ()
  mTypes' `for_` \types -> do
    retHandle <- State.getCurrentReturn
    retType'@(~(TupleType retVars)) <-
      doOutsideCtx $ makeTuple <$> traverse State.freshASTStar (fromJust mTypes)
    State.storeFact $ retHandle `typeUnion` retType'
    case mConv of
      Just (AST.Foreign (AST.StrLit conv))
        | conv == "C" -> do State.storeFacts $ regularExprConstraint <$> retVars
        | otherwise -> undefined -- TODO: UB
      Nothing -> return ()
    zipWithM_ ((State.storeFact .) . typeUnion) types retVars
  (fs, retType) <- (_2 %~ toType) <$> State.endProc
  let procedureType = fmap toType formals' `makeFunction` retType
  int <- nextHandleCounter
  State.storeFacts
    [constExprConstraint procedureType, int `functionKind` procedureType]
  hole <- State.storeProc (getName name) fs procedureType
  return (hole, AST.ProcedureHeader mConv (preprocessTrivial name) formals' mTypes')

preprocessProcedureCommon ::
     (GetName proc, HasPos a, GetConv proc)
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

-- TODO: consult conventions with the man
-- TODO: add handle (dependent on the context) to the node
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
    (getTypeHole header', AST.Procedure header' body') <$ State.popParent

-- TODO: ditto
instance Preprocess AST.ProcedureDecl a b where
  preprocessImpl _ procedure@(AST.ProcedureDecl header) = do
    preprocessProcedureCommon procedure header
    ((EmptyTypeHole, ) . AST.ProcedureDecl <$> preprocessHeader header) <* State.popParent

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
    return (SimpleTypeHole handle, AST.SemiFormal mKind type'')

instance Preprocess AST.StackDecl a b where
  preprocessImpl _ (AST.StackDecl datums) =
    (EmptyTypeHole, ) . AST.StackDecl <$> preprocessDatums datums

instance Preprocess AST.Stmt a b where
  preprocessImpl _ =
    \case
      AST.EmptyStmt -> purePreprocess EmptyTypeHole AST.EmptyStmt
      AST.IfStmt cond thenBody mElseBody -> do
        cond' <- preprocess cond
        State.storeFacts
          [ cond' `typeConstraint` ComplType BoolType
          , boolKind `minKindConstraint` cond'
          ]
        (EmptyTypeHole, ) <$>
          liftA2 (AST.IfStmt cond') (preprocess thenBody) (preprocessT mElseBody)
      AST.SwitchStmt scrutinee arms -> do
        scrutinee' <- preprocess scrutinee
        arms' <- preprocessT arms
        traverse_ (State.storeFact . subType scrutinee') arms'
        return (EmptyTypeHole, AST.SwitchStmt scrutinee' arms')
      AST.SpanStmt key value body -> do
        (key', value') <- preprocessSpanCommon key value
        body' <- preprocess body
        return (EmptyTypeHole, AST.SpanStmt key' value' body')
      AST.AssignStmt lvalues exprs -> do
        lvalues' <- preprocessT lvalues
        exprs' <- preprocessT exprs
        zipWithM_ ((State.storeFact .) . subType) lvalues' exprs'
        return (EmptyTypeHole, AST.AssignStmt lvalues' exprs')
      AST.PrimOpStmt {} -> undefined
      AST.CallStmt names mConv expr actuals mTargets annots -- TODO: this is just a placeholder
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
        State.storeFact . typeUnion expr' . makeAddrType $
          makeFunction (toType <$> argTypes) (toType $ makeTuple retTypes)
        zipWithM_ ((State.storeFact .) . subType) names' retTypes
        zipWithM_ ((State.storeFact .) . subType) argTypes actuals'
        return
          ( EmptyTypeHole
          , AST.CallStmt names' mConv expr' actuals' mTargets' annots')
      AST.JumpStmt {} -> undefined
      AST.ReturnStmt mConv Nothing actuals
      -- TODO: consult conventions with man
       -> do
        actuals' <- preprocessT actuals
        retType@(~(TupleType retVars)) <-
          doOutsideCtx $ makeTuple <$> traverse State.freshASTStar actuals
        zipWithM_ ((State.storeFact .) . subType) retVars actuals'
        State.getCtxMConv >>= \case
          Just (AST.Foreign (AST.StrLit conv))
            | conv == "C" -> do State.storeFacts $ regularExprConstraint <$> retVars
            | otherwise -> undefined -- TODO: UB
          Nothing -> return ()
        State.getCurrentReturn >>= State.storeFact . (`typeUnion` retType)
        return (EmptyTypeHole, AST.ReturnStmt mConv Nothing actuals')
      AST.ReturnStmt {} -> undefined
      label@AST.LabelStmt {} -> do
        hole <- State.lookupVar label
        State.storeFacts
          [ addressKind `kindConstraint` hole
          , hole `typeConstraint` ComplType LabelType
          , constExprConstraint hole
          ]
        purePreprocess hole label
      AST.ContStmt {} -> undefined
      AST.GotoStmt expr mTargets -- TODO: check if cosher
       -> do
        expr' <- preprocess expr
        State.storeFacts
          [ addressKind `minKindConstraint` expr'
          , expr' `typeConstraint` ComplType LabelType
          ]
        (EmptyTypeHole, ) . AST.GotoStmt expr' <$> preprocessT mTargets
      AST.CutToStmt {} -> undefined

doOutsideCtx :: Preprocessor a -> Preprocessor a
doOutsideCtx action = State.popTopContext >>= (action <*) . State.pushContext

-- TODO: this seems wrong
instance Preprocess AST.KindName a b where
  preprocessImpl annot (AST.KindName mKind name) = do
    nameType <- State.lookupVar name
    handle <- State.freshNamedASTStar (getName name) annot
    maybeKindUnif mKind handle nameType
    return (SimpleTypeHole handle, AST.KindName mKind (preprocessTrivial name))

instance Preprocess AST.Arm a b where
  preprocessImpl = undefined

instance Preprocess AST.Targets a b where
  preprocessImpl = undefined

instance Preprocess AST.Lit a b where
  preprocessImpl annot lit = do
    handle <- State.freshASTStar annot
    State.storeFact . (`minKindConstraint` handle) $
      case lit of
        AST.LitInt {} -> integerKind
        AST.LitFloat {} -> floatKind
        AST.LitChar {} -> integerKind -- TODO: check this one? but probably correct
    State.storeFact $ constExprConstraint handle
    purePreprocess (SimpleTypeHole handle) lit

instance Preprocess AST.Actual a b where
  preprocessImpl annot (AST.Actual mKind expr) = do
    expr' <- preprocess expr
    handle <- State.freshASTStar annot
    maybeKindUnif mKind handle expr'
    return (SimpleTypeHole handle, AST.Actual mKind expr')

unionizeTypes :: ToType a => [a] -> Preprocessor ()
unionizeTypes =
  \case
    [] -> return ()
    tVar:tVars -> traverse_ (State.storeFact . typeUnion tVar) tVars

instance Preprocess AST.Init a b where
  preprocessImpl annot =
    \case
      AST.ExprInit exprs -> do
        exprs' <- preprocessT exprs
        handle <- State.freshASTStar annot
        State.storeFacts $ constExprConstraint <$> exprs'
        State.storeFacts $ (`subType` handle) <$> exprs'
        return (SimpleTypeHole handle, AST.ExprInit exprs')
      strInit@AST.StrInit {} -> strInitCommon StringType strInit
      strInit@AST.Str16Init {} -> strInitCommon String16Type strInit
    where
      strInitCommon c strInit = do
        handle <- State.freshStar
        State.storeFact $ handle `typeConstraint` ComplType c
        purePreprocess (SimpleTypeHole handle) strInit

preprocessDatums ::
     (WithTypeHole a b, HasPos a)
  => [Annot AST.Datum a]
  -> Preprocessor [Annot AST.Datum b]
preprocessDatums datums = do
  datumResults <- preprocessDatumsImpl [] datums
  zipWithM preprocessFinalize (takeAnnot <$> datums) (return <$> datumResults)

preprocessDatumsImpl ::
     (HasPos a, WithTypeHole a b)
  => [TypeHole]
  -> [Annotation AST.Datum a]
  -> Preprocessor [(TypeHole, AST.Datum b)]
preprocessDatumsImpl _ [] = return [] -- TODO: continue from here
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
              MemberTypeHole
                (holeHandle hole)
                [(name', holeHandle classHole)]
                [holeHandle mem]
                []
          _ -> do
            tVar <- State.freshStar
            hole <- State.lookupVar name
            State.storeFacts
              [ addressKind `kindConstraint` hole
              , hole `typeUnion` makeAddrType tVar
              ]
            return hole
      purePreprocess hole datum <:> preprocessDatumsImpl (hole : cache) others
    AST.DatumAlign _ -> do
      purePreprocess EmptyTypeHole datum <:> preprocessDatumsImpl [] others
    AST.Datum type' mSize mInit -> do
      uses State.currentContext head >>= \case
        StructCtx {ctxHole, ctxConstraint = (_,structType)} -> do
          tVars <- State.collectTVars
          -- handle <- freshASTStar annot
          State.pushFacts
          (hole, datum') <- goGeneral
          fs <- State.popTopFacts
          -- unionizeTypes $ handle : (holeHandle <$> cache)
          let structPtr = makeAddrType structType
              t = makeFunction [structPtr] $ toType hole
              funcFact h = Fact $ tVarId (toTypeVar h) `functionKind` t
              constExprFact = Fact $ constExprConstraint t
              structAddrFact =
                Fact $ addressKind `kindConstraint` structPtr
              fieldAddrFact =
                Fact $ addressKind `kindConstraint` makeAddrType hole
              cache' =
                cache <&> \case
                  MemberTypeHole iMem [hole''] [mem] [] -> (hole'', iMem, mem)
                  _ -> undefined -- TODO: logic error
              hole' = MemberTypeHole (holeHandle ctxHole) `uncurry3` unzip3 cache'
              scheme (MemberTypeHole h [(name, classHandle)] _ _) = do
                method <-
                  refreshNestedFact . forall tVars [h `typeUnion` t] $
                  structAddrFact :
                  fieldAddrFact : funcFact h : constExprFact : fs
                subst <- refresher tVars
                fact <-
                  refreshNestedFact $
                  forall (apply subst `Set.map` tVars) [subst `apply` classF] []
                return [method, fact]
                  -- classC = Fact $ classConstraint name constraint
                where
                  classF = classFact name constraint
                  constraint =
                    foldApp
                      [ toType classHandle
                      , structPtr
                      , toType hole
                      ]
              scheme _ = undefined -- TODO: logic error
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
              mInit' `for_` \init -> do
                State.storeFacts [type'' `subType` init, linkExprConstraint init]
              State.storeFacts
                [ addressKind `kindConstraint` handle
                , handle `typeConstraint` makeAddrType type''
                ]
              return (SimpleTypeHole handle, AST.Datum type'' mSize' mInit')

instance Preprocess AST.Datum a b where
  preprocessImpl a datum = head <$> preprocessDatumsImpl [] [datum `Annot` a]

instance Preprocess AST.Size a b where
  preprocessImpl _ =
    \case
      AST.Size (Just expr) -> do
        expr' <- preprocess expr
        State.storeFact $ constExprConstraint expr'
        return (getTypeHole expr', AST.Size $ Just expr')
      size -> purePreprocess EmptyTypeHole size

instance Preprocess AST.LValue a b where
  preprocessImpl annot =
    \case
      lvName@AST.LVName {} -> do
        State.lookupFVar lvName >>= \case
          EmptyTypeHole -> State.lookupVar lvName >>= (`purePreprocess` lvName)
          scheme -> do
            inst <- State.freshNamedASTStar (getName lvName) annot
            handle <- State.freshNamedASTStar (getName lvName) annot
            State.storeFacts
              [ scheme `instType` inst
              , handle `typeUnion` makeAddrType inst
              , constExprConstraint handle
              , constExprConstraint inst
              , addressKind `kindConstraint` handle
              , tVarId (toTypeVar inst) `functionKind` inst
              ]
            purePreprocess (handle `LVInstTypeHole` holeHandle scheme) lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
      AST.LVRef Nothing expr mAsserts -> do
        expr' <- preprocess expr
        handle <- State.freshASTStar annot
        let mAsserts' = (withTypeHole EmptyTypeHole <$>) <$> mAsserts
        State.storeFacts
          [ expr' `typeUnion` makeAddrType handle
          , handle `subConst` expr'
          , addressKind `minKindConstraint` expr'
          ]
        return (SimpleTypeHole handle, AST.LVRef Nothing expr' mAsserts')
      AST.LVRef (Just type') expr mAsserts -> do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHole EmptyTypeHole <$>) <$> mAsserts
        State.storeFact $ addressKind `minKindConstraint` expr'
        return (getTypeHole type'', AST.LVRef (Just type'') expr' mAsserts')

instance Preprocess AST.Expr a b where
  preprocessImpl annot -- TODO: this is just ugly
   =
    \case
      AST.MemberExpr struct field ->
        State.lookupSMem field >>= \case
          EmptyTypeHole -> undefined -- TODO: very bad
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
              , inst `typeUnion` makeFunction [argType] retType
              , classConstraint name $
                foldApp [toType classHole, toType argType, toType retType]
              , constExprConstraint inst
              , addressKind `kindConstraint` struct'
              , handle `subConst` struct'
              , retType `subType` handle
              , tVarId (toTypeVar inst) `functionKind` inst
              , addressKind `kindConstraint` handle
              , argType `subType` struct'
              ]
            return
              ( MethodTypeHole handle (holeHandle scheme) inst
              , AST.MemberExpr struct' $ preprocessTrivial field)
            where makeHandle = State.freshNamedASTStar (getName field) annot
      AST.ParExpr expr -> AST.ParExpr `preprocessInherit` expr
      AST.LVExpr lvalue -> AST.LVExpr `preprocessInherit` lvalue
      AST.BinOpExpr op left right -- TODO: implement correctly, this is just a placeholder
       -> do
        handle <- State.freshStar
        operator <- State.freshStar
        left' <- preprocess left
        right' <- preprocess right
        State.storeFact $
          operator `typeUnion`
          makeFunction [toType left', toType right'] (toType handle)
        State.storeFact $ tVarId (toTypeVar operator) `functionKind` operator
        State.storeFacts $
          if op `elem` [AST.EqOp, AST.NeqOp, AST.GtOp, AST.LtOp, AST.GeOp, AST.LeOp]
            then [ left' `typeConstraint` right'
                 , handle `subConst` left'
                 , handle `subConst` right'
                 , handle `typeConstraint` ComplType BoolType
                 , boolKind `kindConstraint` handle
                 ]
            else [handle `subType` left', handle `subType` right']
      -- TODO: add constraint dependent on the operator
        return (SimpleTypeHole handle, AST.BinOpExpr op left' right')
      AST.NegExpr expr -> AST.NegExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      AST.ComExpr expr -> AST.ComExpr `preprocessInherit` expr -- TODO: add constraint dependent on the operator
      AST.LitExpr lit mType -> do
        lit' <- preprocess lit
        mType' <-
          for mType $ \type' -> do
            type'' <- preprocess type'
            State.storeFacts [type'' `subType` lit', constExprConstraint lit']
            return type''
        return (getTypeHole lit', AST.LitExpr lit' mType')
      -- TODO: fix this
      AST.PrefixExpr name actuals -> do
        (handle, tupleType) <- fixCommon name
        actuals' <- preprocessT actuals
        State.storeFact $ tupleType `typeUnion` makeTuple actuals'
        return
          (SimpleTypeHole handle, AST.PrefixExpr (preprocessTrivial name) actuals')
      AST.InfixExpr name left right -> do
        (handle, tupleType) <- fixCommon name
        left' <- preprocess left
        right' <- preprocess right
        State.storeFact $ tupleType `typeUnion` makeTuple [left', right']
        return
          ( SimpleTypeHole handle
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
          [ fType `typeUnion` makeFunction [argType] retType
          , opScheme `typeUnion` getNamedOperator (getName name)
          , opScheme `instType` fType
          , argType `subType` tupleType
          , handle `subType` retType
          , unstorableConstraint tupleType
          ]
        return (handle, tupleType)
      preprocessInherit c n = do
        n' <- preprocess n
        return (getTypeHole n', c n')

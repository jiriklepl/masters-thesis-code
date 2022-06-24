{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: add the overload resolution for instances to monomorphization
module CMM.Monomorphize where

import safe Prelude ()

import safe Control.Applicative (Applicative(pure), liftA2, liftA3)
import safe Control.Lens.Getter ((^.), uses)
import safe Control.Lens.Setter ((%~))
import safe Control.Lens.Tuple (_1, _2)
import safe Control.Lens.Zoom (zoom)
import safe Control.Monad (Monad((>>=), return), sequence, zipWithM_)
import safe Control.Monad.State (State)
import safe Data.Bifunctor (first)
import safe Data.Bool (otherwise)
import safe Data.Either (Either(Left, Right))
import safe Data.Eq (Eq)
import safe Data.Foldable (fold, or)
import safe Data.Function (($), (&), (.), const)
import safe Data.Functor ((<$), (<$>), (<&>), fmap)
import safe Data.List (concatMap, head, null)
import safe qualified Data.Map as Map
import safe Data.Maybe (Maybe(Just, Nothing), catMaybes, maybe)
import safe Data.Monoid ((<>), mappend, mempty)
import safe Data.Ord (Ord((>=), compare), Ordering(EQ, GT, LT))
import safe qualified Data.PartialOrd as PartialOrd
import safe qualified Data.Set as Set
import safe Data.Traversable (for, traverse)
import safe Data.Tuple (uncurry)
import safe GHC.Err (error, undefined)
import safe Text.Show (Show(show))

import safe CMM.AST
  ( Actual(Actual)
  , Asserts
  , Body(Body)
  , BodyItem(BodyDecl, BodyStackDecl, BodyStmt)
  , CallAnnot
  , Datum(Datum)
  , Decl(ConstDecl, ExportDecl, ImportDecl, PragmaDecl, RegDecl,
     TargetDecl, TypedefDecl)
  , Export
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, MemberExpr,
     NegExpr, ParExpr, PrefixExpr)
  , Import
  , Instance(Instance)
  , KindName
  , LValue(LVName, LVRef)
  , Lit
  , Procedure(Procedure)
  , ProcedureHeader
  , Section(SecDatum, SecDecl, SecProcedure, SecSpan)
  , StackDecl(StackDecl)
  , Stmt(AssignStmt, CallStmt, ContStmt, CutToStmt, EmptyStmt,
     GotoStmt, IfStmt, JumpStmt, LabelStmt, PrimOpStmt, ReturnStmt,
     SpanStmt, SwitchStmt)
  , Struct(Struct)
  , Targets
  , TopLevel(TopClass, TopDecl, TopInstance, TopProcedure, TopSection,
         TopStruct)
  , Unit(Unit)
  )
import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.AST.Utils (addTopLevels)
import safe CMM.Control.Applicative (liftA5)
import safe CMM.Control.Monad ((>>@=))
import safe CMM.Data.Bounds.Impl (Bounds(Bounds))
import safe CMM.Data.Either (oneRight)
import safe CMM.Data.Nullable (nullVal)
import safe CMM.Data.Tuple (submergeTuple)
import safe CMM.Inference (simplify)
import safe CMM.Inference.Constness (Constness(LinkExpr))
import safe CMM.Inference.Fact ()
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.Preprocess.HasTypeHandle
  ( HasTypeHandle
  , getTypeHandleId
  )
import safe CMM.Inference.Preprocess.TypeHole
  ( HasTypeHole(getTypeHole)
  , TypeHole(LVInstTypeHole, MemberTypeHole, MethodTypeHole,
         SimpleTypeHole)
  , getTypeHoleId
  , holeHandle
  )
import safe CMM.Inference.State
  ( Inferencer
  , InferencerState
  , fromOldName
  , getHandle
  , getTyping
  , readConstingBounds
  , readKindingBounds
  , reconstructOld
  , schemes
  , tryGetHandle
  )
import safe CMM.Inference.Subst (Subst, apply)
import safe CMM.Inference.Type as Type
  ( Type(ComplType, ErrorType, VarType)
  , makeAddrType
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType, AppType, BoolType, ConstType, FunctionType,
          LabelType, LamType, String16Type, StringType, TBitsType, TupleType,
          VoidType)
  )
import safe CMM.Inference.TypeHandle (consting, kinding, typing)
import safe CMM.Inference.TypeVar as Type (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Inference.Unify (instantiateFrom)
import safe CMM.Monomorphize.Polytypeness
  ( Polytypeness(Absurd, Mono, Poly)
  , constAbsurdity
  , constPolymorphism
  , kindAbsurdity
  , kindPolymorphism
  , typePolymorphism
  )
import safe CMM.Monomorphize.Schematized
  ( Schematized(FuncScheme, StructScheme)
  , schematized2topLevel
  )
import safe CMM.Monomorphize.State
  ( MonomorphizeState
  , Monomorphizer
  , PolyData(getPolyData)
  , PolyGenerate(getPolyGenerate)
  , PolyMethods(getPolyMethods)
  , addData
  , addGenerate
  , addMethod
  , addPolyScheme
  , isMemorized
  , memorizeStrong
  , polyData
  , polyGenerate
  , polyMethods
  , polySchemes
  , unPolyGenerate
  )
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Utils (backQuote)

type InferMonomorphizer a = State (InferencerState, MonomorphizeState a)

useInferencer :: Inferencer b -> InferMonomorphizer a b
useInferencer = zoom _1

useMonomorphizer :: Monomorphizer a b -> InferMonomorphizer a b
useMonomorphizer = zoom _2

data MonomorphizeError =
  FooError
  deriving (Show, Eq)

class Monomorphize n a where
  monomorphize ::
       (HasPos a, HasTypeHole a)
    => Subst Type.Type
    -> n a
    -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (n a))

typingPolytypeness :: Type.Type -> Polytypeness
typingPolytypeness t =
  if null free
    then Mono
    else typePolymorphism t free
  where
    free = freeTypeVars t

constnessPolytypeness :: TypeVar -> Inferencer Polytypeness
constnessPolytypeness tVar = go <$> readConstingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      case low `compare` high of
        LT ->
          if low >= LinkExpr
            then Mono
            else constPolymorphism tVar bounds
        EQ -> Mono
        GT -> constAbsurdity tVar bounds

kindingPolytypeness :: TypeVar -> Inferencer Polytypeness
kindingPolytypeness tVar = go <$> readKindingBounds tVar
  where
    go bounds@(low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else kindPolymorphism tVar bounds
        else kindAbsurdity tVar bounds

typePolytypeness :: Subst Type.Type -> TypeVar -> Inferencer Polytypeness
typePolytypeness subst tVar =
  reconstructOld tVar >>= simplify . apply subst >>= tryGetHandle >>= \case
    Just handle ->
      mappend (typingPolytypeness $ apply subst handle ^. typing) <$>
      liftA2
        mappend
        (kindingPolytypeness $ apply subst handle ^. kinding)
        (constnessPolytypeness $ apply subst handle ^. consting)
    Nothing ->
      error $
      "(internal logic error) Type variable " <>
      backQuote (show tVar) <> " not registered by the inferencer."

getTypeHandleIdPolytypeness ::
     HasTypeHandle a => Subst Type.Type -> a -> Inferencer Polytypeness
getTypeHandleIdPolytypeness subst = typePolytypeness subst . getTypeHandleId

monomorphizeTrivial ::
     (Monomorphize p a, HasPos a, HasTypeHole a)
  => Subst Type.Type
  -> a
  -> (p a -> n a)
  -> p a
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Annot n a))
monomorphizeTrivial subst annot constr =
  monomorphizeMaybe subst Nothing $ Just . withAnnot annot . constr

monomorphizeTrivials ::
     (Monomorphize p a, HasPos a, HasTypeHole a)
  => Subst Type.Type
  -> a
  -> ([p a] -> n a)
  -> [p a]
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Annot n a))
monomorphizeTrivials = ((. (Just .)) .) . monomorphizeMaybes

ensureJustMonomorphized ::
     a1
  -> a1 `Either` Maybe (Annot n a2) -> InferMonomorphizer a2 (a1 `Either` Maybe (Annot n a2))
ensureJustMonomorphized err mono = do
  return $ do
    mono' <- mono
    case mono' of
      Nothing -> Left err
      Just _ -> mono

ensuredJustMonomorphize ::
     (Monomorphize (Annotation n) a, HasPos a, HasTypeHole a)
  => MonomorphizeError
  -> Subst Type
  -> Annotation n a
  -> InferMonomorphizer a (Either MonomorphizeError (Maybe (Annot n a)))
ensuredJustMonomorphize err subst n =
  monomorphize subst n >>= ensureJustMonomorphized err

monomorphizeMaybes ::
     (Monomorphize p a, HasPos a, HasTypeHole a)
  => Subst Type.Type
  -> a
  -> ([p a] -> Maybe (n a))
  -> [p a]
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Annot n a))
monomorphizeMaybes subst annot constr pars = do
  sequence <$>
    traverse (monomorphize subst) pars >>@= \pars' ->
      return . (withAnnot annot <$>) . constr $ catMaybes pars'

monomorphizeMaybe ::
     (Monomorphize p a, HasPos a, HasTypeHole a)
  => Subst Type.Type
  -> Maybe (Annot n a)
  -> (p a -> Maybe (Annot n a))
  -> p a
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Annot n a))
monomorphizeMaybe subst a f =
  monomorphizeMaybeWithFailure subst (Right a) (Right . f)

monomorphizeMaybeWithFailure ::
     (Monomorphize p a, HasPos a, HasTypeHole a)
  => Subst Type.Type
  -> MonomorphizeError `Either` Maybe (Annot n a) -> (p a -> MonomorphizeError `Either` Maybe (Annot n a)) -> p a -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Annot n a))
monomorphizeMaybeWithFailure subst a f par =
  monomorphize subst par >>@= maybe a f

eliminateClasses :: [Annot TopLevel a] -> [Annot TopLevel a]
eliminateClasses [] = []
eliminateClasses (topLevel:topLevels) =
  case topLevel of
    TopClass {} `Annot` _ -> go
    TopInstance (Instance _ _ methods `Annot` a) `Annot` _ ->
      (withAnnot a . TopProcedure <$> methods) <> go
    _ -> topLevel : go
  where
    go = eliminateClasses topLevels

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Unit) a where
  monomorphize subst =
    \case
      Unit topLevels `Annot` a -> do
        monomorphizeTrivials subst a Unit (eliminateClasses topLevels) >>= go
        where go =
                \case
                  Left err -> return $ Left err
                  Right mNode -> do
                    more <-
                      useMonomorphizer . uses polyGenerate $
                      concatMap (submergeTuple . (_2 %~ Set.toList)) .
                      Map.toList . getPolyGenerate
                    more' <- uncurry monomorphizePolyType `traverse` more
                    sequence more' & \case
                      Left err -> return $ Left err
                      Right [] -> succeed mNode
                      Right mores -> do
                        let topLevels' =
                              schematized2topLevel <$> catMaybes mores
                            unit = addTopLevels topLevels'
                        useMonomorphizer unPolyGenerate
                        go . Right $ unit <$> mNode

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot TopLevel) a where
  monomorphize subst (topLevel `Annot` a) =
    case topLevel of
      TopProcedure procedure ->
        monomorphizeTrivial subst a TopProcedure procedure
      TopDecl decl ->
        monomorphizeMaybeWithFailure
          subst
          (Left undefined)
          (Right . Just . withAnnot a . TopDecl)
          decl
      TopSection _ _ -> undefined
      -- sequence <$> traverse monomorphize items >>@= \items' -> do
      --   let
      --     generate' = foldGetGenerate items'
      --     schemes' = foldGetSchemes items'
      --     node' = withAnnot a . TopSection name . catMaybes $ view node <$> items'
      --   return $ monomorphized (Just node') generate' schemes'
      TopClass {} -> succeed nullVal
      TopInstance {} -> succeed nullVal
      TopStruct struct -> monomorphizeTrivial subst a TopStruct struct

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Section) a where
  monomorphize subst (section `Annot` a) =
    case section of
      SecDecl decl -> monomorphizeTrivial subst a SecDecl decl
      SecProcedure procedure ->
        monomorphizeTrivial subst a SecProcedure procedure
      SecDatum datum -> monomorphizeTrivial subst a SecDatum datum
      SecSpan {} -> undefined

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Decl) a where
  monomorphize subst (decl `Annot` a) =
    case decl of
      ImportDecl imports -> monomorphizeTrivials subst a ImportDecl imports
      ExportDecl exports -> monomorphizeTrivials subst a ExportDecl exports
      ConstDecl {} -> undefined
      TypedefDecl {} -> undefined
      RegDecl {} -> succeed . Just $ withAnnot a decl
      PragmaDecl {} -> undefined
      TargetDecl {} -> undefined

instance Monomorphize (Annot AST.Class) a where
  monomorphize _ _ = succeed nullVal

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Instance) a where
  monomorphize subst =
    \case
      Instance _ _ methods `Annot` a ->
        monomorphizeMaybes subst a (const Nothing) methods

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Struct) a where
  monomorphize subst (struct `Annot` a) =
    case struct of
      Struct name datums -> do
        handle <- useInferencer . fmap toTypeVar . getHandle $ getTypeHoleId a
        schemeName <- useInferencer . fromOldName $ getTypeHoleId a
        useInferencer (uses schemes (schemeName `Map.lookup`)) >>= \case
          Nothing -> undefined
          Just scheme ->
            useMonomorphizer $
            addPolyScheme handle scheme (StructScheme $ withAnnot a struct)
        useInferencer
          (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
          Mono -> do
            datums' <- traverse (ensuredJustMonomorphize undefined subst) datums
            return $ do
              datums'' <- sequence datums'
              return $ withAnnot a . Struct name <$> sequence datums''
          Poly {} -> monomorphizeMaybes subst a (const Nothing) datums
          Absurd absurdity -> error $ show absurdity -- TODO: logic error

instance Monomorphize (Annot Import) a where
  monomorphize = undefined

instance Monomorphize (Annot Export) a where
  monomorphize = undefined

instance HasTypeHole a => Monomorphize (Annot Datum) a where
  monomorphize subst annotated@(datum `Annot` a) =
    case datum of
      Datum {} -> do
        case getTypeHole a of
          MemberTypeHole struct _ insts schemes' ->
            useInferencer
              (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
              Absurd absurdity -> error $ show absurdity
              _ -> do
                useMonomorphizer $
                  zipWithM_
                    (addData $ toTypeVar struct)
                    (toTypeVar <$> schemes')
                    (toTypeVar <$> insts)
                success
          SimpleTypeHole handle ->
            useInferencer (getTypeHandleIdPolytypeness subst handle) >>= \case
              Mono -> success
              Poly {} -> undefined
              Absurd absurdity -> error $ show absurdity -- TODO
          _ -> undefined -- TODO this
      _ -> success
    where
      success = succeed $ Just annotated

succeed :: Applicative f => a -> f (Either err a)
succeed = pure . Right

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Procedure) a where
  monomorphize subst (procedure@(Procedure header body) `Annot` a) = do
    schemeName <- useInferencer . fromOldName $ getTypeHoleId a
    useInferencer (uses schemes (schemeName `Map.lookup`)) >>= \case
      Nothing -> undefined -- TODO: logic error
      Just scheme ->
        useMonomorphizer $
        addPolyScheme
          (toTypeVar $ getTypeHole a)
          scheme
          (FuncScheme $ withAnnot a procedure)
    case getTypeHole a of
      SimpleTypeHole {} -> return ()
      MethodTypeHole inst scheme _ ->
        useMonomorphizer $ addMethod (toTypeVar scheme) (toTypeVar inst)
      _ -> undefined -- TODO: logic error
    useInferencer
      (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
      Mono -> do
        handle <- useInferencer $ getHandle schemeName
        hole <- return . toTypeVar $ getTypeHole a
        inst <-
          useInferencer $
          reconstructOld (toTypeVar handle) >>=
          simplify . makeAddrType . apply subst >>=
          fmap toTypeVar . getHandle
        useMonomorphizer $
          case getTypeHole a of
            SimpleTypeHole _ -> memorizeStrong hole inst
            MethodTypeHole _ scheme _ -> memorizeStrong (toTypeVar scheme) inst
            _ -> undefined -- TODO: logic error
        header' <- ensuredJustMonomorphize undefined subst header
        body' <- ensuredJustMonomorphize undefined subst body
        return $ do
          header'' <- header'
          body'' <- body'
          return $ withAnnot a <$> liftA2 Procedure header'' body''
      Poly polyWhat
        | null subst -> succeed nullVal
        | otherwise -> error $ show polyWhat
      Absurd {} -> return undefined

instance Monomorphize (Annot ProcedureHeader) a where
  monomorphize _ header = succeed $ Just header

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Body) a where
  monomorphize subst =
    \case
      Body items `Annot` a -> monomorphizeTrivials subst a Body items

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot BodyItem) a where
  monomorphize subst (bodyItem `Annot` a) =
    case bodyItem of
      BodyDecl decl ->
        monomorphizeTrivial subst a BodyDecl decl >>=
        ensureJustMonomorphized undefined
      BodyStackDecl decl ->
        monomorphizeTrivial subst a BodyStackDecl decl >>=
        ensureJustMonomorphized undefined
      BodyStmt stmt ->
        monomorphizeTrivial subst a BodyStmt stmt >>=
        ensureJustMonomorphized undefined

instance Monomorphize (Annot StackDecl) a where
  monomorphize subst =
    \case
      StackDecl items `Annot` a -> monomorphizeTrivials subst a StackDecl items

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Actual) a where
  monomorphize subst =
    \case
      Actual mKind expr `Annot` a ->
        monomorphizeTrivial subst a (Actual mKind) expr

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Stmt) a where
  monomorphize subst annotated@(stmt `Annot` a) =
    case stmt of
      EmptyStmt -> succeed $ Just annotated
      IfStmt cond tBody eBody -> do
        cond' <- ensuredJustMonomorphize undefined subst cond
        tBody' <- ensuredJustMonomorphize undefined subst tBody
        eBody' <- traverse (ensuredJustMonomorphize undefined subst) eBody
        return $ do
          cond'' <- cond'
          tBody'' <- tBody'
          eBody'' <- sequence eBody'
          return $ annotate <$> liftA3 IfStmt cond'' tBody'' (sequence eBody'')
      SwitchStmt {} -> undefined
      SpanStmt {} -> undefined
      AssignStmt lValues exprs -> do
        lValues' <- traverse (ensuredJustMonomorphize undefined subst) lValues
        exprs' <- traverse (ensuredJustMonomorphize undefined subst) exprs
        return $ do
          lValues'' <- sequence lValues'
          exprs'' <- sequence exprs'
          return $
            annotate <$>
            liftA2 AssignStmt (sequence lValues'') (sequence exprs'')
      PrimOpStmt {} -> undefined
      CallStmt rets mConv func actuals mTargs callAnnots -> do
        rets' <- traverse (ensuredJustMonomorphize undefined subst) rets
        func' <- ensuredJustMonomorphize undefined subst func
        actuals' <- traverse (ensuredJustMonomorphize undefined subst) actuals
        mTargs' <- traverse (ensuredJustMonomorphize undefined subst) mTargs
        callAnnots' <-
          traverse (ensuredJustMonomorphize undefined subst) callAnnots
        return $ do
          rets'' <- sequence rets'
          func'' <- func'
          mTargs'' <- sequence mTargs'
          actuals'' <- sequence actuals'
          callAnnots'' <- sequence callAnnots'
          return $
            annotate <$>
            liftA5
              (`CallStmt` mConv)
              (sequence rets'')
              func''
              (sequence actuals'')
              (sequence mTargs'')
              (sequence callAnnots'')
      JumpStmt {} -> undefined
      ReturnStmt mConv Nothing actuals -> do
        actuals' <- traverse (ensuredJustMonomorphize undefined subst) actuals
        return $ do
          actuals'' <- sequence actuals'
          return $ annotate . ReturnStmt mConv Nothing <$> sequence actuals''
      ReturnStmt _ (Just _) _ -> undefined
      LabelStmt {} ->
        useInferencer
          (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
          Mono -> succeed $ Just annotated
          Poly {} -> return $ Left undefined -- TODO: local label cannot be a polytype
          Absurd {} -> return $ Left undefined -- TODO: local label cannot have an illegal type
      ContStmt _ _ -> undefined
      GotoStmt expr targets -> do
        expr' <- ensuredJustMonomorphize undefined subst expr
        targets' <- traverse (ensuredJustMonomorphize undefined subst) targets
        return $ do
          expr'' <- expr'
          targets'' <- sequence targets'
          return $ annotate <$> liftA2 GotoStmt expr'' (sequence targets'')
      CutToStmt {} -> undefined
    where
      annotate = withAnnot a

instance Monomorphize (Annot KindName) a where
  monomorphize _ (kindName `Annot` a) = succeed . Just $ withAnnot a kindName

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot LValue) a where
  monomorphize subst =
    \case
      annotated@(lValue `Annot` a) ->
        useInferencer
          (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
          Poly {}
          -- error . show $ () <$ lValue -- TODO: lValue cannot be a polytype
           -> do
            succeed . Just $ annotated
          Absurd absurdity -> error $ show absurdity -- TODO: lValue cannot have an illegal type
          Mono ->
            case lValue of
              LVName _ ->
                case getTypeHole a of
                  SimpleTypeHole {} -> succeed . Just $ annotated
                  LVInstTypeHole handle scheme ->
                    useInferencer
                      (getTypeHandleIdPolytypeness
                         subst
                         (holeHandle $ getTypeHole scheme)) >>= \case
                      Absurd {} -> return $ Left undefined
                      _ -> do
                        inst <-
                          useInferencer $
                          reconstructOld (toTypeVar handle) >>=
                          simplify . apply subst >>=
                          fmap toTypeVar . getHandle
                        useMonomorphizer $ addGenerate (toTypeVar scheme) inst
                        succeed . Just $ annotated
                  _ -> return $ Left undefined
              LVRef mType expr mAsserts -> do
                mType' <-
                  traverse (ensuredJustMonomorphize undefined subst) mType
                expr' <- ensuredJustMonomorphize undefined subst expr
                mAsserts' <-
                  traverse (ensuredJustMonomorphize undefined subst) mAsserts
                return $ do
                  mType'' <- sequence mType'
                  expr'' <- expr'
                  mAsserts'' <- sequence mAsserts'
                  return $
                    withAnnot a <$>
                    liftA3 LVRef (sequence mType'') expr'' (sequence mAsserts'')

instance Monomorphize (Annot Targets) a where
  monomorphize = undefined

instance (HasPos a, HasTypeHole a) => Monomorphize (Annot Expr) a where
  monomorphize subst (expr `Annot` a) =
    case expr of
      LitExpr lit mType -> do
        lit' <- monomorphize subst lit
        mType' <- traverse (monomorphize subst) mType
        return $ do
          lit'' <- lit'
          mType'' <- sequence mType'
          return $ withAnnot a <$> liftA2 LitExpr lit'' (sequence mType'')
      LVExpr lValue -> monomorphizeTrivial subst a LVExpr lValue
      ParExpr expr' -> monomorphizeTrivial subst a ParExpr expr'
      BinOpExpr op left right -> do
        left' <- monomorphize subst left
        right' <- monomorphize subst right
        return $ do
          left'' <- left'
          right'' <- right'
          return $ withAnnot a <$> liftA2 (BinOpExpr op) left'' right''
      ComExpr expr' -> monomorphizeTrivial subst a ComExpr expr'
      NegExpr expr' -> monomorphizeTrivial subst a NegExpr expr'
      InfixExpr {} -> undefined
      PrefixExpr {} -> undefined
      MemberExpr expr' field ->
        case getTypeHole a of
          MethodTypeHole _ scheme inst ->
            useInferencer (getTypeHandleIdPolytypeness subst scheme) >>= \case
              Absurd {} -> undefined -- TODO: logic error
              _ -> do
                expr'' <- monomorphize subst expr'
                inst' <-
                  useInferencer $
                  reconstructOld (toTypeVar inst) >>= simplify . apply subst >>=
                  fmap toTypeVar . getHandle
                useMonomorphizer $
                  addGenerate (toTypeVar scheme) (toTypeVar inst')
                return $ do
                  expr''' <- expr''
                  return $ withAnnot a . (`MemberExpr` field) <$> expr'''
          _ -> undefined -- TODO: logic error

instance Monomorphize (Annot Lit) a where
  monomorphize _ = succeed . Just

instance Monomorphize (Annot AST.Type) a where
  monomorphize subst =
    \case
      type' `Annot` a ->
        useInferencer
          (getTypeHandleIdPolytypeness subst (holeHandle $ getTypeHole a)) >>= \case
          Mono -> do
            type'' <-
              useInferencer (reconstructOld (getTypeHoleId a)) >>=
              instantiateType
            return $ Just (withAnnot a type') <$ type''
          Poly {} -> return $ Left undefined -- TODO: the type has to be concrete
          Absurd {} -> return $ Left undefined -- TODO: all types have to make sense

instantiateType ::
     Type.Type -> InferMonomorphizer a (MonomorphizeError `Either` ())
instantiateType =
  \case
    ErrorType {} -> return $ Left undefined -- TODO: encountered error type
    VarType {} -> return $ Left undefined -- TODO: type is not a monotype
    ComplType type' ->
      case type' of
        TupleType args -> fmap fold . sequence <$> traverse instantiateType args
        FunctionType args ret ->
          fmap fold . sequence <$> traverse instantiateType (ret : args)
        AppType t t' ->
          liftA2 (liftA2 mappend) (instantiateType t) (instantiateType t')
        AddrType t -> instantiateType t
        LamType {} -> undefined
        ConstType {} -> undefined
        StringType -> succeed mempty
        String16Type -> succeed mempty
        LabelType -> succeed mempty
        TBitsType {} -> succeed mempty
        BoolType -> succeed mempty
        VoidType -> succeed mempty

monomorphizePolyType ::
     (HasPos a, HasTypeHole a)
  => TypeVar
  -> TypeVar
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Schematized a))
monomorphizePolyType scheme inst =
  go [monomorphizeMethod scheme inst, monomorphizeField scheme inst]
  where
    go =
      \case
        action:actions ->
          action >>= \case
            Nothing -> go actions
            Just result -> return result
        [] -> fallback
    fallback = monomorphizeMethodInner scheme inst

monomorphizeMethod ::
     (HasPos a, HasTypeHole a)
  => TypeVar
  -> TypeVar
  -> InferMonomorphizer a (Maybe (MonomorphizeError `Either` Maybe (Schematized a)))
monomorphizeMethod scheme inst = do
  useMonomorphizer (uses polyMethods $ Map.lookup scheme . getPolyMethods) >>= \case
    Nothing -> return Nothing
    Just set -> do
      isDone <- fmap or $ recall `traverse` Set.toList set
      if isDone
        then return . Just $ Right Nothing
        else Just <$> go set
  where
    recall item = do
      inst' <-
        useInferencer $
        reconstructOld (toTypeVar item) >>= simplify . makeAddrType >>=
        fmap toTypeVar . getHandle
      useMonomorphizer $ isMemorized scheme inst'
    go set = do
      set' <- Set.toList set `for` \item -> monomorphizeMethodInner item inst
      return $ first (error . show) $ oneRight set'

monomorphizeField ::
     (HasPos a, HasTypeHole a)
  => TypeVar
  -> TypeVar
  -> InferMonomorphizer a (Maybe (MonomorphizeError `Either` Maybe (Schematized a)))
monomorphizeField scheme inst =
  useMonomorphizer (uses polyData $ Map.lookup scheme . getPolyData) >>= \case
    Nothing -> return Nothing
    Just map -> do
      isDone <- fmap or $ recall `traverse` Map.keys map
      if isDone
        then return . Just $ Right Nothing
        else Just <$> go map
  where
    recall item = do
      inst' <-
        useInferencer $
        reconstructOld (toTypeVar item) >>= simplify >>=
        fmap toTypeVar . getHandle
      useMonomorphizer $ isMemorized scheme inst'
    go map = do
      map' <-
        Map.toList map `for` \(item, struct) ->
          monomorphizeFieldInner item inst struct
      return $ first head $ oneRight map'

monomorphizeFieldInner ::
     (HasPos a, HasTypeHole a)
  => TypeVar
  -> TypeVar
  -> TypeVar
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Schematized a))
monomorphizeFieldInner scheme inst struct = do
  scheme' <- useInferencer $ fromOldName scheme >>= getTyping
  inst' <- useInferencer $ fromOldName inst >>= getTyping
  case inst' `instantiateFrom` scheme' of
    Left err -> return $ Left undefined -- TODO
    Right _ -> do
      scheme'' <- useInferencer $ reconstructOld $ toTypeVar scheme
      inst'' <- useInferencer $ reconstructOld $ toTypeVar inst
      case inst'' `instantiateFrom` scheme'' of
        Left err ->
          return $ Left $ error $ "\n" <> show scheme'' <> "\n" <> show inst'' -- TODO: logic error
        Right (subst, _) -> do
          struct' <- useInferencer . fmap toTypeVar $ getHandle struct
          useMonomorphizer (uses polySchemes $ Map.lookup struct') >>= \case
            Nothing -> error $ show struct' -- TODO: logic error
            Just (_, schematized) ->
              case schematized of
                FuncScheme _ -> undefined -- TODO: logic error
                StructScheme structure ->
                  ensuredJustMonomorphize undefined subst structure <&>
                  fmap (fmap StructScheme)

monomorphizeMethodInner ::
     (HasPos a, HasTypeHole a)
  => TypeVar
  -> TypeVar
  -> InferMonomorphizer a (MonomorphizeError `Either` Maybe (Schematized a))
monomorphizeMethodInner scheme inst = do
  scheme' <- useInferencer $ fromOldName scheme >>= getTyping
  inst' <- useInferencer $ fromOldName inst >>= getTyping
  case inst' `instantiateFrom` ComplType (AddrType scheme') of
    Left err -> do
      return $ Left $ error $ "\n" <> show scheme' <> "\n" <> show inst' -- TODO
    Right _ -> do
      scheme'' <- useInferencer $ reconstructOld scheme
      inst'' <- useInferencer $ reconstructOld inst
      case inst'' `instantiateFrom` ComplType (AddrType scheme'') of
        Left err ->
          return $ Left $ error $ "\n" <> show scheme'' <> "\n" <> show inst'' -- TODO: logic error
        Right (subst, _) ->
          useMonomorphizer (uses polySchemes $ Map.lookup scheme) >>= \case
            Nothing -> error $ show scheme
            Just (_, schematized) ->
              case schematized of
                FuncScheme procedure ->
                  ensuredJustMonomorphize undefined subst procedure <&>
                  fmap (fmap FuncScheme)
                StructScheme _ -> undefined -- TODO: logic error

instance Monomorphize (Annot CallAnnot) a where
  monomorphize = undefined

instance Monomorphize (Annot Asserts) a where
  monomorphize = undefined

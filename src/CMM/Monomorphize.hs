{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.Monomorphize where

import safe Control.Applicative (liftA2, liftA3)
import safe Control.Lens ( uses, _1, _2, zoom )
import safe Control.Monad (zipWithM_)
import safe Control.Monad.State (State)
import safe Data.Bifunctor (first)
import safe Data.Functor ((<&>), void)
import safe qualified Data.Map as Map
import safe Data.Maybe (catMaybes)
import safe qualified Data.Set as Set
import safe Data.Traversable (for)
import safe Data.Either (fromRight)

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot, takeAnnot), withAnnot, mapNode)
import safe CMM.AST.Utils (addTopLevels)
import safe CMM.Control.Applicative (liftA5)
import safe CMM.Control.Monad ((>>@=))
import safe CMM.Data.Either (oneRight)
import safe CMM.Data.Nullable (nullVal)
import safe CMM.Data.Tuple (submergeTuple)
import safe CMM.Inference (simplify)
import safe CMM.Inference.Preprocess.Elaboration
  ( HasElaboration(getElaboration)
  , Elaboration(LVInstElaboration, MemberElaboration, MethodElaboration,
         SimpleElaboration)
  , getElaborationId
  )
import safe qualified CMM.Inference.State as State
import safe CMM.Inference.State (InferencerState, lookupScheme)
import safe CMM.Inference.Subst (Subst, apply)
import safe CMM.Inference.Type as Type
  ( Type(ComplType)
  , makeAddrType
  )
import safe CMM.Inference.TypeCompl
  ( TypeCompl(AddrType)
  )
import safe CMM.Inference.TypeVar as Type (ToTypeVar(toTypeVar), TypeVar)
import safe CMM.Inference.Unify (instantiateFrom, unify)
import safe CMM.Monomorphize.Polytypeness
  ( Polytypeness(Absurd, Mono, Poly)
  , typePolytypeness, reconstructType, reconstructHole
  )
import safe CMM.Monomorphize.Schematized
  ( Schematized(ProcedureScheme, StructScheme)
  , schematized2topLevel
  )
import safe CMM.Parser.GetPos (GetPos, SourcePos)
import safe CMM.Err.Error
    ( Error, )
import safe qualified CMM.Err.Error as Error
import safe CMM.Monomorphize.Error
    ( MonomorphizeError(IllegalScheme, ReachedMaxWaves,
                        CannotInstantiate),
      illegalNothing,
      illegalPolyType,
      illegalHole,
      isNotScheme,
      makeError,
      absurdType, voidWrapped )
import safe CMM.AST.Wrap
    ( ASTWrapper, MakeWrapped, makeWrapped )
import safe CMM.Inference.Unify.Error ( UnificationError )

import safe qualified CMM.Monomorphize.State as State
import safe CMM.Monomorphize.State.Impl
    ( Monomorphizer, MonomorphizeState )
import safe CMM.Inference.Fact (Scheme((:.)), Qual ((:=>)), FlatFact (Equality, TypingEquality), FlatFacts)
import safe CMM.Utils (logicError, notYetImplemented)

type InferMonomorphizer a = State (InferencerState, MonomorphizeState a)

returnError :: (GetPos n, Monad m) => n -> MonomorphizeError -> m (Either Error b)
returnError n = return . Left  . makeError n

failure :: Monad m => a -> m (Either a b)
failure = return . Left

useInferencer :: State.Inferencer b -> InferMonomorphizer a b
useInferencer = zoom _1

useMonomorphizer :: Monomorphizer a b -> InferMonomorphizer a b
useMonomorphizer = zoom _2

reThrow :: Monad m => Either Error a -> (a -> m (Either Error b)) -> m (Either Error b)
reThrow = \case
  Left err -> const $ failure err
  Right x -> ($ x)

reThrowM :: Monad m =>
  m (Either Error a)
  -> (a -> m (Either Error b)) -> m (Either Error b)
reThrowM f a = do
  f' <- f
  f' `reThrow` a

class Monomorphize n a where
  monomorphize ::
       (GetPos a, HasElaboration a)
    => Subst Type.Type
    -> n a
    -> InferMonomorphizer a (Error `Either` Maybe (n a))

monomorphizeTrivial ::
     (Monomorphize p a, GetPos a, HasElaboration a)
  => Subst Type.Type
  -> a
  -> (p a -> n a)
  -> p a
  -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
monomorphizeTrivial subst annot constr =
  monomorphizeMaybe subst Nothing $ Just . withAnnot annot . constr

monomorphizeTrivials ::
     (Monomorphize p a, GetPos a, HasElaboration a)
  => Subst Type.Type
  -> a
  -> ([p a] -> n a)
  -> [p a]
  -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
monomorphizeTrivials = ((. (Just .)) .) . monomorphizeMaybes

ensureJustMonomorphized :: ( GetPos a, MakeWrapped n) =>Annot n a
  -> Error `Either` Maybe (Annot n a) -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
ensureJustMonomorphized poly mono = do
  return $ do
    mono' <- mono
    case mono' of
      Nothing -> Left $ illegalNothing poly
      Just _ -> mono

ensuredJustMonomorphize ::
     ( Monomorphize (Annotation n) a, GetPos a, HasElaboration a, MakeWrapped n)
  =>Subst Type
  -> Annotation n a
  -> InferMonomorphizer a (Either Error (Maybe (Annot n a)))
ensuredJustMonomorphize subst n =
  monomorphize subst n >>= ensureJustMonomorphized n

monomorphizeMaybes ::
     (Monomorphize p a, GetPos a, HasElaboration a)
  => Subst Type.Type
  -> a
  -> ([p a] -> Maybe (n a))
  -> [p a]
  -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
monomorphizeMaybes subst annot constr pars = do
  sequence <$>
    traverse (monomorphize subst) pars >>@= \pars' ->
      return . (withAnnot annot <$>) . constr $ catMaybes pars'

monomorphizeMaybe ::
     (Monomorphize p a, GetPos a, HasElaboration a)
  => Subst Type.Type
  -> Maybe (Annot n a)
  -> (p a -> Maybe (Annot n a))
  -> p a
  -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
monomorphizeMaybe subst a f =
  monomorphizeMaybeWithFailure subst (Right a) (Right . f)

monomorphizeMaybeWithFailure ::
     (Monomorphize p a, GetPos a, HasElaboration a)
  => Subst Type.Type
  -> Error `Either` Maybe (Annot n a) -> (p a -> Error `Either` Maybe (Annot n a)) -> p a -> InferMonomorphizer a (Error `Either` Maybe (Annot n a))
monomorphizeMaybeWithFailure subst a f par =
  monomorphize subst par >>@= maybe a f

eliminateClasses :: [Annot AST.TopLevel a] -> [Annot AST.TopLevel a]
eliminateClasses [] = []
eliminateClasses (topLevel:topLevels) =
  case topLevel of
    AST.TopClass {} `Annot` _ -> go
    AST.TopInstance (AST.Instance _ _ methods `Annot` a) `Annot` _ ->
      (withAnnot a . AST.TopProcedure <$> methods) <> go
    _ -> topLevel : go
  where
    go = eliminateClasses topLevels

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Unit) a where
  monomorphize subst unit@(AST.Unit topLevels `Annot` a) =
    monomorphizeTrivials subst a AST.Unit (eliminateClasses topLevels) `reThrowM` \case
      Nothing ->  failure $ illegalNothing unit
      Just node ->  fmap Just <$> cycleWaves node
cycleWaves :: (GetPos a, HasElaboration a) => Annot AST.Unit a
  -> InferMonomorphizer a (Either Error (Annot AST.Unit a))
cycleWaves node = do
  waves <- useMonomorphizer State.incWaves
  more <-
    useMonomorphizer . uses State.polyGenerate $
    concatMap submergeTuple  .
    Map.toList . State.getPolyGenerate
  more' <- uncurry monomorphizePolyType `traverse` more
  sequence more' `reThrow` \case
    [] -> succeed node
    mores
      | otherwise -> do
        useMonomorphizer State.getMaxWaves >>= \maxWaves ->
          if waves < maxWaves
            then do
              useMonomorphizer State.unPolyGenerate
              cycleWaves $ unit node
            else
              returnError (head topLevels') $ ReachedMaxWaves waves
      where
        topLevels' =
          schematized2topLevel  <$> catMaybes mores
        unit = addTopLevels topLevels'

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.TopLevel) a where
  monomorphize subst (topLevel `Annot` a) =
    case topLevel of
      AST.TopProcedure procedure ->
        monomorphizeTrivial subst a AST.TopProcedure procedure
      AST.TopDecl decl ->
        monomorphizeMaybeWithFailure
          subst
          (Left $ illegalNothing decl)
          (Right . Just . withAnnot a . AST.TopDecl)
          decl
      AST.TopSection name items ->
        sequence <$> traverse (monomorphize subst) items >>@=
          return . Just . withAnnot a . AST.TopSection name . catMaybes
      AST.TopClass {} -> succeed nullVal
      AST.TopInstance {} -> succeed nullVal
      AST.TopStruct struct -> monomorphizeTrivial subst a AST.TopStruct struct

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Section) a where
  monomorphize subst (section `Annot` a) =
    case section of
      AST.SecDecl decl -> monomorphizeTrivial subst a AST.SecDecl decl
      AST.SecProcedure procedure ->
        monomorphizeTrivial subst a AST.SecProcedure procedure
      AST.SecDatum datum -> monomorphizeTrivial subst a AST.SecDatum datum
      AST.SecSpan {} -> notYetImplemented

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Decl) a where
  monomorphize subst (decl `Annot` a) =
    case decl of
      AST.ImportDecl imports -> monomorphizeTrivials subst a AST.ImportDecl imports
      AST.ExportDecl exports -> monomorphizeTrivials subst a AST.ExportDecl exports
      AST.ConstDecl {} -> notYetImplemented
      AST.TypedefDecl {} -> notYetImplemented
      AST.RegDecl bool registers -> do
        monomorphizeTrivial subst a (AST.RegDecl bool) registers
      AST.PragmaDecl {} -> notYetImplemented
      AST.TargetDecl {} -> notYetImplemented

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Registers) a where
  monomorphize subst (AST.Registers mKind t nameStrs `Annot` a) = do
    t' <- monomorphize subst t
    b <- useInferencer $ reconstructHole subst a
    let (names, strs) = unzip nameStrs
    names' <- traverse (monomorphize subst) names
    return $ do
      t'' <- t'
      names'' <- sequence names'
      return $ withAnnot b <$> liftA2 (AST.Registers mKind) t'' ((`zip` strs) <$> sequence names'')

instance HasElaboration a => Monomorphize (Annot AST.Name) a where
  monomorphize subst (AST.Name name `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    succeed $ Just . withAnnot b $ AST.Name name

instance Monomorphize (Annot AST.Class) a where
  monomorphize _ _ = succeed nullVal

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Instance) a where
  monomorphize subst =
    \case
      AST.Instance _ _ methods `Annot` a ->
        monomorphizeMaybes subst a (const Nothing) methods

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Struct) a where
  monomorphize subst annotated@(struct `Annot` a) =
    case struct of
      AST.Struct name datums -> do
        props <- useInferencer . fmap toTypeVar . State.getProps $ getElaborationId a
        schemeName <- useInferencer . State.fromOldName $ getElaborationId a
        addScheme <- useInferencer (lookupScheme schemeName) >>= \case
          Nothing -> failure $ isNotScheme annotated
          Just scheme ->
            useMonomorphizer . fmap Right $
            State.addPolyScheme props scheme (StructScheme $ withAnnot a struct)
        addScheme `reThrow` \_ ->
          useInferencer
          (typePolytypeness subst $ getElaboration a) >>= \case
          Mono -> do
            datums' <- traverse (ensuredJustMonomorphize subst) datums
            b <- useInferencer $ reconstructHole subst a
            return $ do
              datums'' <- sequence datums'
              return $ withAnnot b . AST.Struct name <$> sequence datums''
          Poly {} -> monomorphizeMaybes subst a (const Nothing) datums
          Absurd absurdity -> failure $ absurdity `absurdType` annotated

instance Monomorphize (Annot AST.Import) a where
  monomorphize = notYetImplemented

instance Monomorphize (Annot AST.Export) a where
  monomorphize = notYetImplemented

instance HasElaboration a => Monomorphize (Annot AST.Datum) a where
  monomorphize subst annotated@(datum `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    case datum of
      AST.Datum new type' size init' -> do
        case getElaboration a of
          MemberElaboration struct _ insts schemes' ->
            useInferencer
              (typePolytypeness subst (getElaboration a)) >>= \case
              Absurd absurdity -> failure $ absurdity `absurdType` annotated
              _ -> do
                useMonomorphizer $
                  zipWithM_
                    (State.addField $ toTypeVar struct)
                    (toTypeVar <$> schemes')
                    (toTypeVar <$> insts)
                succeed . Just $ datum `Annot` b
          SimpleElaboration props ->
            useInferencer (typePolytypeness subst props) >>= \case
              Mono -> do
                type'' <- ensuredJustMonomorphize subst type'
                size' <- traverse (ensuredJustMonomorphize subst) size
                init'' <- traverse (ensuredJustMonomorphize subst) init'
                return $ do
                  type''' <- type''
                  size'' <- sequence size'
                  init''' <- sequence init''
                  return $ withAnnot b <$> liftA3 (AST.Datum new) type''' (sequence size'') (sequence init''')
              Poly polyWhat -> failure $ polyWhat `illegalPolyType` annotated
              Absurd absurdity -> failure $ absurdity `absurdType` annotated
          hole -> failure $ illegalHole hole annotated
      _ -> succeed . Just $ datum `Annot` b

instance HasElaboration a => Monomorphize (Annot AST.Init) a where
  monomorphize subst (init' `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    case init' of
      AST.StrInit strLit -> succeed . Just . withAnnot b $ AST.StrInit strLit
      AST.Str16Init strLit -> succeed . Just . withAnnot b $ AST.Str16Init strLit
      AST.ExprInit exprs -> do
        exprs' <- traverse (ensuredJustMonomorphize subst) exprs
        return $ do
          exprs'' <- sequence exprs'
          return $ withAnnot b . AST.ExprInit <$> sequence exprs''

instance HasElaboration a => Monomorphize (Annot AST.Size) a where
  monomorphize subst (AST.Size expr `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    expr' <- traverse (ensuredJustMonomorphize subst) expr
    return $ do
      expr'' <- sequence expr'
      return $ withAnnot b . AST.Size <$> sequence expr''


succeed :: Applicative f => a -> f (Either err a)
succeed = pure . Right

unifFacts :: FlatFacts -> Subst Type -> Subst Type
unifFacts [] subst = subst
unifFacts (fact : others) subst = case fact of
  t `Equality` t' -> go t t'
  t `TypingEquality` t' -> go t t'
  _ -> unifFacts others subst
  where
    go t t' = unifFacts others . (`apply` subst) . fst$ fromRight logicError (apply subst t `unify` apply subst t')

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Procedure) a where
  monomorphize subst annotated@(procedure@(AST.Procedure header body) `Annot` a) = do
    schemeName <- useInferencer . State.fromOldName $ getElaborationId a
    props <- useInferencer $ State.getProps schemeName
    inst <- useInferencer $ reconstructType subst props
    subst' <- useInferencer (lookupScheme schemeName) >>= \case
      Nothing -> logicError
      Just scheme@(_ :. facts :=> t) -> do
        let subst' = either logicError ((`apply` subst) . fst) (t `unify` inst)
        useMonomorphizer  $
          State.addPolyScheme
            (toTypeVar $ getElaboration a)
            scheme
            (ProcedureScheme $ withAnnot a procedure)
        return $ unifFacts facts subst'
    addMethod <- case getElaboration a of
      SimpleElaboration {} -> succeed ()
      MethodElaboration inst' scheme _ ->
        useMonomorphizer . fmap Right $ State.addMethod (toTypeVar scheme) (toTypeVar inst')
      hole -> failure $ hole `illegalHole` annotated
    addMethod `reThrow` \_ -> useInferencer
      (typePolytypeness subst' (getElaboration a)) >>= \case
      Mono -> do
        hole <- return . toTypeVar $ getElaboration a
        waves <- useMonomorphizer State.getWaves
        stored <- useMonomorphizer $
          case getElaboration a of
            SimpleElaboration _ -> Right <$> State.tryStore hole inst
            MethodElaboration _ scheme _ -> if waves > 0
              then Right <$> State.tryStore (toTypeVar scheme) inst
              else return $ Right False
            hole' -> failure $ hole' `illegalHole` annotated
        stored `reThrow` \case
          True -> do
            addMemory <- useMonomorphizer $
              case getElaboration a of
                SimpleElaboration _ -> Right <$> State.memorize hole inst
                MethodElaboration _ scheme _ -> Right <$> State.memorize (toTypeVar scheme) inst
                hole' -> failure $ hole' `illegalHole` annotated
            addMemory `reThrow` \_ -> do
              header' <- ensuredJustMonomorphize subst' header
              body' <- ensuredJustMonomorphize subst' body
              b <- useInferencer $ reconstructHole subst' a
              return $ do
                header'' <- header'
                body'' <- body'
                return $ withAnnot b <$> liftA2 AST.Procedure header'' body''
          False -> succeed nullVal
      Poly {} -> succeed nullVal
      Absurd absurdity -> failure $ absurdity `absurdType` annotated

instance Monomorphize (Annot AST.ProcedureHeader) a where
  monomorphize subst (AST.ProcedureHeader mConv name formals semis `Annot` a) = do
    formals' <- traverse (monomorphize subst) formals
    semis' <- traverse (traverse $ monomorphize subst) semis
    b <- useInferencer $ reconstructHole subst a
    return $ do
      formals'' <- sequence formals'
      semis''  <- traverse sequence semis'
      return $ withAnnot b <$> liftA2 (AST.ProcedureHeader mConv name) (sequence formals'') (traverse sequence semis'')

instance Monomorphize (Annot AST.Formal) a where
  monomorphize subst (AST.Formal mKind bool t name `Annot` a) =
    monomorphizeTrivial subst a (\t' -> AST.Formal mKind bool t' name) t

instance Monomorphize (Annot AST.SemiFormal) a where
  monomorphize subst (AST.SemiFormal mKind t `Annot` a) =
    monomorphizeTrivial subst a (AST.SemiFormal mKind) t

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Body) a where
  monomorphize subst =
    \case
      AST.Body items `Annot` a -> monomorphizeTrivials subst a AST.Body items

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.BodyItem) a where
  monomorphize subst annotated@(bodyItem `Annot` a) =
    case bodyItem of
      AST.BodyDecl decl ->
        monomorphizeTrivial subst a AST.BodyDecl decl >>=
        ensureJustMonomorphized annotated
      AST.BodyStackDecl decl ->
        monomorphizeTrivial subst a AST.BodyStackDecl decl >>=
        ensureJustMonomorphized annotated
      AST.BodyStmt stmt ->
        monomorphizeTrivial subst a AST.BodyStmt stmt >>=
        ensureJustMonomorphized annotated

instance Monomorphize (Annot AST.StackDecl) a where
  monomorphize subst =
    \case
      AST.StackDecl items `Annot` a -> monomorphizeTrivials subst a AST.StackDecl items

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Actual) a where
  monomorphize subst =
    \case
      AST.Actual mKind expr `Annot` a -> do
        b <- useInferencer $ reconstructHole subst a
        monomorphizeTrivial subst b (AST.Actual mKind) expr

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Stmt) a where
  monomorphize subst (stmt `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    let
      annotate = withAnnot b
      annotated = annotate stmt
    case stmt of
      AST.EmptyStmt -> succeed $ Just annotated
      AST.IfStmt cond tBody eBody -> do
        cond' <- ensuredJustMonomorphize subst cond
        tBody' <- ensuredJustMonomorphize subst tBody
        eBody' <- traverse (ensuredJustMonomorphize subst) eBody
        return $ do
          cond'' <- cond'
          tBody'' <- tBody'
          eBody'' <- sequence eBody'
          return $ annotate <$> liftA3 AST.IfStmt cond'' tBody'' (sequence eBody'')
      AST.SwitchStmt {} -> notYetImplemented
      AST.SpanStmt {} -> notYetImplemented
      AST.AssignStmt lValues exprs -> do
        lValues' <- traverse (ensuredJustMonomorphize subst) lValues
        exprs' <- traverse (ensuredJustMonomorphize subst) exprs
        return $ do
          lValues'' <- sequence lValues'
          exprs'' <- sequence exprs'
          return $
            annotate <$>
            liftA2 AST.AssignStmt (sequence lValues'') (sequence exprs'')
      AST.PrimOpStmt {} -> notYetImplemented
      AST.CallStmt rets mConv func actuals mTargs callAnnots -> do
        rets' <- traverse (ensuredJustMonomorphize subst) rets
        func' <- ensuredJustMonomorphize subst func
        actuals' <- traverse (ensuredJustMonomorphize subst) actuals
        mTargs' <- traverse (ensuredJustMonomorphize subst) mTargs
        callAnnots' <-
          traverse (ensuredJustMonomorphize subst) callAnnots
        return $ do
          rets'' <- sequence rets'
          func'' <- func'
          mTargs'' <- sequence mTargs'
          actuals'' <- sequence actuals'
          callAnnots'' <- sequence callAnnots'
          return $
            annotate <$>
            liftA5
              (`AST.CallStmt` mConv)
              (sequence rets'')
              func''
              (sequence actuals'')
              (sequence mTargs'')
              (sequence callAnnots'')
      AST.JumpStmt {} -> notYetImplemented
      AST.ReturnStmt mConv Nothing actuals -> do
        actuals' <- traverse (ensuredJustMonomorphize subst) actuals
        return $ do
          actuals'' <- sequence actuals'
          return $ annotate . AST.ReturnStmt mConv Nothing <$> sequence actuals''
      AST.ReturnStmt _ (Just _) _ -> notYetImplemented
      AST.LabelStmt {} ->
        useInferencer
          (typePolytypeness subst (getElaboration a)) >>= \case
          Mono -> succeed $ Just annotated
          Poly polyWhat -> failure $ polyWhat `illegalPolyType` annotated
          Absurd absurdity -> failure $ absurdity `absurdType` annotated
      AST.ContStmt {} -> notYetImplemented
      AST.DroppedStmt {} -> succeed $ Just annotated
      AST.GotoStmt expr targets -> do
        expr' <- ensuredJustMonomorphize subst expr
        targets' <- traverse (ensuredJustMonomorphize subst) targets
        return $ do
          expr'' <- expr'
          targets'' <- sequence targets'
          return $ annotate <$> liftA2 AST.GotoStmt expr'' (sequence targets'')
      AST.CutToStmt {} -> notYetImplemented

instance Monomorphize (Annot AST.KindName) a where
  monomorphize subst (kindName `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    succeed . Just $ withAnnot b kindName

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.LValue) a where
  monomorphize subst (lValue `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    let annotated = withAnnot b lValue
    useInferencer
      (typePolytypeness subst (getElaboration a)) >>= \case
      Absurd absurdity -> failure $ absurdity `absurdType` annotated
      _ ->
        case lValue of
          AST.LVName _ ->
            case getElaboration a of
              SimpleElaboration {} -> succeed $ Just annotated
              LVInstElaboration props scheme ->
                useInferencer
                  (typePolytypeness
                      subst
                      scheme) >>= \case
                  Absurd absurdity -> failure $ absurdity `absurdType` annotated
                  _ -> do
                    inst <-
                      useInferencer $
                      State.reconstructOld (toTypeVar props) >>=
                      simplify . apply subst >>=
                      fmap toTypeVar . State.getProps
                    instType <- useInferencer $ reconstructType subst props
                    useMonomorphizer $ State.addGenerate (mapNode makeWrapped annotated) (toTypeVar scheme) inst instType
                    succeed $ Just annotated
              hole -> failure $ hole `illegalHole` annotated
          AST.LVRef mType expr mAsserts -> do
            mType' <-
              traverse (ensuredJustMonomorphize subst) mType
            expr' <- ensuredJustMonomorphize subst expr
            mAsserts' <-
              traverse (ensuredJustMonomorphize subst) mAsserts
            return $ do
              mType'' <- sequence mType'
              expr'' <- expr'
              mAsserts'' <- sequence mAsserts'
              return $
                withAnnot b <$>
                liftA3 AST.LVRef (sequence mType'') expr'' (sequence mAsserts'')

instance Monomorphize (Annot AST.Targets) a where
  monomorphize = notYetImplemented

instance (GetPos a, HasElaboration a) => Monomorphize (Annot AST.Expr) a where
  monomorphize subst (expr `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    let annotated = withAnnot b expr
    case expr of
      AST.LitExpr lit mType -> do
        lit' <- monomorphize subst lit
        mType' <- traverse (monomorphize subst) mType
        return $ do
          lit'' <- lit'
          mType'' <- sequence mType'
          return $ withAnnot b <$> liftA2 AST.LitExpr lit'' (sequence mType'')
      AST.LVExpr lValue -> monomorphizeTrivial subst b AST.LVExpr lValue
      AST.ParExpr expr' -> monomorphizeTrivial subst b AST.ParExpr expr'
      AST.BinOpExpr op left right -> do
        left' <- monomorphize subst left
        right' <- monomorphize subst right
        return $ do
          left'' <- left'
          right'' <- right'
          return $ withAnnot b <$> liftA2 (AST.BinOpExpr op) left'' right''
      AST.ComExpr expr' -> monomorphizeTrivial subst b AST.ComExpr expr'
      AST.NegExpr expr' -> monomorphizeTrivial subst b AST.NegExpr expr'
      AST.InfixExpr {} -> notYetImplemented
      AST.PrefixExpr {} -> notYetImplemented
      AST.MemberExpr expr' field -> do
        case getElaboration a of
          MethodElaboration _ scheme inst ->
            useInferencer (typePolytypeness subst scheme) >>= \case
              Absurd absurdity -> failure $ absurdity `absurdType` annotated
              _ -> do
                expr'' <- monomorphize subst expr'
                inst' <-
                  useInferencer $
                  State.reconstructOld (toTypeVar inst) >>= simplify . apply subst >>=
                  fmap toTypeVar . State.getProps
                instType <-
                  useInferencer $
                  State.reconstructOld (toTypeVar inst) <&> apply subst
                useMonomorphizer $
                  State.addGenerate (mapNode makeWrapped annotated) (toTypeVar scheme) (toTypeVar inst') instType
                return $ do
                  expr''' <- expr''
                  return $ withAnnot b . (`AST.MemberExpr` field) <$> expr'''
          hole -> failure $ hole `illegalHole` annotated

instance Monomorphize (Annot AST.Lit) a where
  monomorphize _ = succeed . Just

instance Monomorphize (Annot AST.Type) a where
  monomorphize subst (type' `Annot` a) = do
    b <- useInferencer $ reconstructHole subst a
    succeed $ Just (withAnnot b type')


monomorphizePolyType ::
     (GetPos a, HasElaboration a)
  => TypeVar
  -> (TypeVar, Annot ASTWrapper SourcePos)
  -> InferMonomorphizer a (Error `Either` Maybe (Schematized a))
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
    fallback = recallMethod scheme (fst inst) >>= \case
       False -> monomorphizeMethodInner scheme inst
       True -> return $ Right Nothing


-- recallMethod
recallMethod :: TypeVar
  -> TypeVar
  -> InferMonomorphizer a Bool
recallMethod scheme item = do
  inst' <-
    useInferencer $
    State.reconstructOld (toTypeVar item) <&> makeAddrType
  useMonomorphizer $ State.isMemorized scheme inst'

monomorphizeMethod ::
     (GetPos a, HasElaboration a)
  => TypeVar
  -> (TypeVar, Annot ASTWrapper SourcePos)
  -> InferMonomorphizer a (Maybe (Error `Either` Maybe (Schematized a)))
monomorphizeMethod scheme inst = do
  useMonomorphizer (uses State.polyMethods $ Map.lookup scheme . State.getPolyMethods) >>= \case
    Nothing -> return Nothing
    Just set -> do
      isDone <- fmap or $ recallMethod scheme `traverse` Set.toList set
      if isDone
        then return . Just $ Right Nothing
        else Just <$> go set
  where
    go set = do
      set' <- Set.toList set `for` \item -> monomorphizeMethodInner item inst
      return $ first (error $ show (fmap (fmap (fmap void)) set')) $ oneRight set' -- TODO: Ambiguity + Nihility

recallField :: TypeVar
  -> TypeVar
  -> InferMonomorphizer a Bool
recallField scheme item = do
  inst' <-
    useInferencer $
    State.reconstructOld (toTypeVar item)
  useMonomorphizer $ State.isMemorized scheme inst'

monomorphizeField ::
     (GetPos a, HasElaboration a)
  => TypeVar
  -> (TypeVar, Annot ASTWrapper SourcePos)
  -> InferMonomorphizer a (Maybe (Error `Either` Maybe (Schematized a)))
monomorphizeField scheme inst =
  useMonomorphizer (uses State.polyData $ Map.lookup scheme . State.getPolyData) >>= \case
    Nothing -> return Nothing
    Just map' -> do
      isDone <- fmap or $ recallField scheme `traverse` Map.keys map'
      if isDone
        then return . Just $ Right Nothing
        else Just <$> go map'
  where
    go map' = do
      map'' <-
        Map.toList map' `for` \(item, struct) ->
          monomorphizeFieldInner item inst struct
      return $ first head $ oneRight map'' -- TODO: Ambiguity + Nihility

instantiationError :: Monad m =>
  Type
  -> Type
  -> Annotation ASTWrapper a
  -> [UnificationError]
  -> m (Either Error b)
instantiationError scheme inst instWrapper err =
  failure . Error.makeError .
    CannotInstantiate scheme inst err $ voidWrapped instWrapper

monomorphizeFieldInner ::
     (GetPos a, HasElaboration a)
  => TypeVar
  -> (TypeVar, Annot ASTWrapper SourcePos)
  -> TypeVar
  -> InferMonomorphizer a (Error `Either` Maybe (Schematized a))
monomorphizeFieldInner scheme (inst, instWrapper) struct = do
  scheme' <- useInferencer $ State.fromOldName scheme >>= State.getTyping
  inst' <- useInferencer $ State.fromOldName inst >>= State.getTyping
  case inst' `instantiateFrom` scheme' of
    Left err -> instantiationError scheme' inst' instWrapper err
    Right _ -> do
      scheme'' <- useInferencer . State.reconstructOld $ toTypeVar scheme
      inst'' <- useInferencer . State.reconstructOld $ toTypeVar inst
      case inst'' `instantiateFrom` scheme'' of
        Left err -> instantiationError scheme'' inst'' instWrapper err
        Right (subst, _) -> do
          struct' <- useInferencer . fmap toTypeVar $ State.getProps struct
          useMonomorphizer (uses State.polySchemes $ Map.lookup struct') >>= \case
            Nothing -> failure $ isNotScheme instWrapper
            Just (_, schematized) ->
              case schematized of
                ProcedureScheme {} -> illegalScheme schematized instWrapper
                StructScheme structure ->
                  ensuredJustMonomorphize subst structure <&>
                  fmap (fmap StructScheme)

monomorphizeMethodInner ::
     (GetPos a, HasElaboration a)
  => TypeVar
  -> (TypeVar, Annot ASTWrapper SourcePos)
  -> InferMonomorphizer a (Error `Either` Maybe (Schematized a))
monomorphizeMethodInner scheme (inst, instWrapper) = do
  scheme' <- useInferencer $ State.fromOldName scheme >>= State.getTyping
  inst' <- useInferencer $ State.fromOldName inst >>= State.getTyping
  case inst' `instantiateFrom` ComplType (AddrType scheme') of
    Left err -> instantiationError scheme' inst' instWrapper err
    Right _ -> do
      scheme'' <- useInferencer $ State.reconstructOld scheme
      inst'' <- useInferencer $ State.reconstructOld inst
      case inst'' `instantiateFrom` ComplType (AddrType scheme'') of
        Left err -> instantiationError scheme'' inst'' instWrapper err
        Right (subst, _) ->
          useMonomorphizer (uses State.polySchemes $ Map.lookup scheme) >>= \case
            Nothing -> failure $ isNotScheme instWrapper
            Just (_, schematized) ->
              case schematized of
                ProcedureScheme procedure -> do
                  monomorphize subst procedure <&> fmap (fmap ProcedureScheme)
                StructScheme {} -> illegalScheme schematized instWrapper

illegalScheme :: (Monad m, GetPos a1) =>
  Schematized a2 -> Annotation ASTWrapper a1 -> m (Either Error b)
illegalScheme schematized instWrapper =
  failure . makeError (takeAnnot instWrapper) . IllegalScheme (void schematized) $ voidWrapped instWrapper

instance Monomorphize (Annot AST.CallAnnot) a where
  monomorphize = notYetImplemented

instance Monomorphize (Annot AST.Asserts) a where
  monomorphize = notYetImplemented

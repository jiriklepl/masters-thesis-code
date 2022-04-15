{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

-- TODO: add the overload resolution for instances to monomorphization
module CMM.Monomorphize where

import safe Control.Applicative (Applicative(liftA2), Const, liftA3)
import safe Control.Lens.Getter ((^.), uses, view)
import safe Control.Lens.Setter ((%~), (?~))
import safe Control.Lens.Tuple (_2)
import safe Control.Monad.IO.Class (MonadIO)
import safe Data.Foldable (fold)
import safe Data.Function ((&))
import safe Data.Functor ((<&>))
import safe qualified Data.Map as Map
import safe Data.Maybe (catMaybes)
import safe qualified Data.PartialOrd as PartialOrd
import safe qualified Data.Set as Set
import safe Data.Void (Void)

import safe CMM.AST
  ( Actual(..)
  , Asserts
  , Body(..)
  , BodyItem(..)
  , CallAnnot
  , Datum
  , Decl(..)
  , Export
  , Expr(..)
  , Import
  , Instance(..)
  , KindName
  , LValue(..)
  , Lit
  , Procedure(..)
  , ProcedureHeader
  , Section(..)
  , StackDecl
  , Stmt(..)
  , Struct(..)
  , Targets
  , TopLevel(..)
  , Unit(..)
  )
import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.AST.Utils (addTopLevels)
import safe CMM.Control.Applicative (liftA5)
import safe CMM.Control.Monad ((>>@=))
import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Data.Nullable (nullVal)
import safe CMM.Data.Tuple (submergeTuple)
import safe CMM.Inference (simplify)
import safe CMM.Inference.FreeTypeVars (freeTypeVars)
import safe CMM.Inference.Preprocess.State
  ( HasTypeHandle(getTypeHandle)
  , getTypeHandleId
  )
import safe CMM.Inference.State
  ( MonadInferencer
  , fromOldName
  , readConstingBounds
  , readKindingBounds
  , reconstruct
  , reconstructOld
  , schemes
  , tryGetHandle
  )
import safe CMM.Inference.Subst (Subst, apply)
import safe CMM.Inference.Type as Type (Type(..))
import safe CMM.Inference.TypeCompl (TypeCompl(..))
import safe CMM.Inference.TypeHandle
  ( TypeHandle
  , consting
  , handleId
  , kinding
  , typing
  )
import safe CMM.Inference.TypeVar as Type (TypeVar)
import safe CMM.Inference.Unify (unify)
import safe CMM.Monomorphize.Monomorphized
  ( Monomorphized
  , PolyGenerate(getPolyGenerate)
  , addGenerate
  , foldGetGenerate
  , foldGetSchemes
  , getNode
  , monomorphized
  , monomorphizedTrivial
  , node
  , polyGenerate
  , polySchemes
  , unNode
  , unPolyGenerate
  , withMaybeNode
  , withNode
  )
import safe CMM.Monomorphize.PolyKind (PolyKind(..))
import safe CMM.Monomorphize.Schematized (Schematized(..), schematized2topLevel)
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Utils (backQuote)

data MonomorphizeError =
  FooError
  deriving (Show, Eq)

class Monomorphize n n' a | n -> n' where
  monomorphize ::
       (HasPos a, HasTypeHandle a, MonadInferencer m)
    => Subst Type.Type
    -> n a
    -> m (MonomorphizeError `Either` n' a)

class MonomorphizeImpl n n' a | n -> n' where
  monomorphizeImpl ::
       (HasPos a, HasTypeHandle a, MonadInferencer m)
    => Subst Type.Type
    -> a
    -> n a
    -> m (MonomorphizeError `Either` Monomorphized (Annot n') a)

instance {-# OVERLAPPABLE #-} MonomorphizeImpl n n' a =>
                              Monomorphize (Annot n) (Monomorphized (Annot n')) a where
  monomorphize subst (Annot n a) = monomorphizeImpl subst a n

typingPolyKind :: Type.Type -> PolyKind
typingPolyKind t =
  if null $ freeTypeVars t
    then Mono
    else Poly

constnessPolyKind :: MonadInferencer m => TypeVar -> m PolyKind
constnessPolyKind tVar = go <$> readConstingBounds tVar
  where
    go (low `Bounds` high) =
      case low `compare` high of
        LT -> Poly
        EQ -> Mono
        GT -> Absurd

kindingPolyKind :: MonadInferencer m => TypeVar -> m PolyKind
kindingPolyKind tVar = go <$> readKindingBounds tVar
  where
    go (low `Bounds` high) =
      if low PartialOrd.<= high
        then if high PartialOrd.<= low
               then Mono
               else Poly
        else Absurd

typePolyKind :: MonadInferencer m => Subst Type.Type -> TypeVar -> m PolyKind
typePolyKind subst tVar =
  fromOldName tVar >>= reconstruct >>= simplify . apply subst >>= tryGetHandle >>= \case
    Just handle ->
      mappend (typingPolyKind $ apply subst handle ^. typing) <$>
      liftA2
        mappend
        (kindingPolyKind $ apply subst handle ^. kinding)
        (constnessPolyKind $ apply subst handle ^. consting)
    Nothing ->
      error $
      "(internal logic error) Type variable " <>
      backQuote (show tVar) <> " not registered by the inferencer."

getTypeHandleIdPolyKind ::
     (MonadInferencer m, HasTypeHandle a) => Subst Type.Type -> a -> m PolyKind
getTypeHandleIdPolyKind subst = typePolyKind subst . getTypeHandleId

monomorphizeTrivial ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized p') a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> a
  -> (p' a -> n a)
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeTrivial subst annot constr =
  monomorphizeMaybe subst Nothing $ Just . withAnnot annot . constr

monomorphizeEmpty ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized (Annot p')) a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeEmpty subst = monomorphizeMaybe subst Nothing $ const Nothing

monomorphizeTrivials ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized p') a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> a
  -> ([p' a] -> n a)
  -> [p a]
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeTrivials = ((. (Just .)) .) . monomorphizeMaybes

ensureJustMonomorphized ::
     Monad m
  => a1
  -> Either a1 (Monomorphized n a2)
  -> m (Either a1 (Monomorphized n a2))
ensureJustMonomorphized err mono = do
  return $ do
    mono' <- mono
    case mono' ^. node of
      Nothing -> Left err
      Just _ -> mono

ensuredJustMonomorphize ::
     ( Monomorphize n1 (Monomorphized n2) a
     , HasPos a
     , HasTypeHandle a
     , MonadInferencer m
     , MonadIO m
     )
  => MonomorphizeError
  -> Subst Type.Type
  -> n1 a
  -> m (Either MonomorphizeError (Monomorphized n2 a))
ensuredJustMonomorphize err subst n =
  monomorphize subst n >>= ensureJustMonomorphized err

monomorphizeMaybes ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized p') a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> a
  -> ([p' a] -> Maybe (n a))
  -> [p a]
  -> m (Either MonomorphizeError (Monomorphized (Annotation n) a))
monomorphizeMaybes subst annot constr pars =
  sequence <$>
  traverse (monomorphize subst) pars >>@= \pars' -> do
    let generate' = foldGetGenerate pars'
        schemes' = foldGetSchemes pars'
        node' = (withAnnot annot <$>) . constr . catMaybes $ view node <$> pars'
    return $ monomorphized node' generate' schemes'

monomorphizeMaybe ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized p') a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> Maybe (Annot n a)
  -> (p' a -> Maybe (Annot n a))
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeMaybe subst a f =
  monomorphizeMaybeWithFailure subst (Right a) (Right . f)

monomorphizeMaybeWithFailure ::
     ( MonadInferencer m
     , MonadIO m
     , Monomorphize p (Monomorphized p') a
     , HasPos a
     , HasTypeHandle a
     )
  => Subst Type.Type
  -> Either MonomorphizeError (Maybe (Annot n a))
  -> (p' a -> Either MonomorphizeError (Maybe (Annot n a)))
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeMaybeWithFailure subst a f par =
  monomorphize subst par >>@= \par' ->
    case par' ^. node of
      Nothing -> (`withMaybeNode` par') <$> a
      Just n -> (`withMaybeNode` par') <$> f n

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Unit Unit a where
  monomorphizeImpl subst a (Unit topLevels) = do
    monomorphizeTrivials subst a Unit topLevels >>= go
    where
      go =
        \case
          Left err -> return $ Left err
          Right mono -> do
            let more =
                  concatMap (submergeTuple . (_2 %~ Set.toList)) .
                  Map.toList . getPolyGenerate $
                  mono ^. polyGenerate
            more' <- mapM (flip (uncurry monomorphizePolyType) mono) more
            sequence more' & \case
              Left err -> return $ Left err
              Right [] -> succeed mono
              Right mores -> do
                let topLevels' =
                      mapM (fmap schematized2topLevel . getNode) mores
                    meta = foldMap unNode mores <> unPolyGenerate (unNode mono)
                    unit = liftA2 addTopLevels topLevels' (mono ^. node)
                go (Right $ withMaybeNode unit meta)

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl TopLevel TopLevel a where
  monomorphizeImpl subst a =
    \case
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
      TopClass class' ->
        monomorphizeMaybeWithFailure
          subst
          (Right Nothing)
          (Left . undefined)
          class'
      TopInstance instance' ->
        monomorphizeMaybeWithFailure
          subst
          (Right Nothing)
          (Left . undefined)
          instance'
      TopStruct struct -> monomorphizeTrivial subst a TopStruct struct

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Section Section a where
  monomorphizeImpl subst a =
    \case
      SecDecl decl -> monomorphizeTrivial subst a SecDecl decl
      SecProcedure procedure ->
        monomorphizeTrivial subst a SecProcedure procedure
      SecDatum datum -> monomorphizeTrivial subst a SecDatum datum
      SecSpan {} -> undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Decl Decl a where
  monomorphizeImpl subst a decl =
    case decl of
      ImportDecl imports -> monomorphizeTrivials subst a ImportDecl imports
      ExportDecl exports -> monomorphizeTrivials subst a ExportDecl exports
      ConstDecl {} -> undefined
      TypedefDecl {} -> undefined
      RegDecl {} -> succeed . monomorphizedTrivial . Just $ withAnnot a decl
      PragmaDecl {} -> undefined
      TargetDecl {} -> undefined

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl AST.Class AST.Class a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl Instance Instance a where
  monomorphizeImpl subst a (Instance _ _ methods) = do
    monomorphizeMaybes subst a (const Nothing) methods

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Struct Struct a where
  monomorphizeImpl subst a (Struct _ datums) = do
    monomorphizeMaybes subst a (const Nothing) datums

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Import Import a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Export Export a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Datum Datum a where
  monomorphizeImpl = undefined

succeed :: Applicative f => a -> f (Either err a)
succeed = pure . Right

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl Procedure Procedure a where
  monomorphizeImpl subst a procedure@(Procedure header body) = do
    getTypeHandleIdPolyKind subst a >>= \case
      Mono -> do
        header' <- ensuredJustMonomorphize undefined subst header
        body' <- ensuredJustMonomorphize undefined subst body
        return $ do
          header'' <- header'
          body'' <- body'
          let procedure' =
                withAnnot a <$>
                liftA2 Procedure (getNode header'') (getNode body'')
          let meta = unNode header'' <> unNode body''
          return $ withMaybeNode procedure' meta
      Poly ->
        uses schemes (getTypeHandleId a `Map.lookup`) >>= \case
          Nothing -> undefined -- TODO: logic error
          Just scheme ->
            succeed $
            nullVal &
            polySchemes %~
            Map.insert
              (getTypeHandle a)
              (scheme, FuncScheme (withAnnot a procedure))
      Absurd -> return $ Left undefined

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl ProcedureHeader ProcedureHeader a where
  monomorphizeImpl _ a header =
    succeed . monomorphizedTrivial . Just $ withAnnot a header

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Body Body a where
  monomorphizeImpl subst a (Body items) =
    monomorphizeTrivials subst a Body items

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl BodyItem BodyItem a where
  monomorphizeImpl subst a (BodyDecl decl) =
    monomorphizeTrivial subst a BodyDecl decl >>=
    ensureJustMonomorphized undefined
  monomorphizeImpl subst a (BodyStackDecl decl) =
    monomorphizeTrivial subst a BodyStackDecl decl >>=
    ensureJustMonomorphized undefined
  monomorphizeImpl subst a (BodyStmt stmt) =
    monomorphizeTrivial subst a BodyStmt stmt >>=
    ensureJustMonomorphized undefined

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl StackDecl StackDecl a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Actual Actual a where
  monomorphizeImpl subst a (Actual mKind expr) =
    monomorphizeTrivial subst a (Actual mKind) expr

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Stmt Stmt a where
  monomorphizeImpl subst a stmt =
    case stmt of
      EmptyStmt -> succeed . monomorphizedTrivial . Just $ annotatedStmt
      IfStmt cond tBody eBody -> do
        cond' <- ensuredJustMonomorphize undefined subst cond
        tBody' <- ensuredJustMonomorphize undefined subst tBody
        eBody' <- traverse (ensuredJustMonomorphize undefined subst) eBody
        return $ do
          cond'' <- cond'
          tBody'' <- tBody'
          eBody'' <- sequence eBody'
          let stmt' =
                annotated <$>
                liftA3
                  IfStmt
                  (getNode cond'')
                  (getNode tBody'')
                  (traverse getNode eBody'')
              meta = unNode cond'' <> unNode tBody'' <> foldMap unNode eBody''
          return $ withMaybeNode stmt' meta
      SwitchStmt {} -> undefined
      SpanStmt {} -> undefined
      AssignStmt lValues exprs -> do
        lValues' <- traverse (ensuredJustMonomorphize undefined subst) lValues
        exprs' <- traverse (ensuredJustMonomorphize undefined subst) exprs
        return $ do
          lValues'' <- sequence lValues'
          exprs'' <- sequence exprs'
          let stmt' =
                annotated <$>
                liftA2
                  AssignStmt
                  (traverse getNode lValues'')
                  (traverse getNode exprs'')
              meta = foldMap unNode lValues'' <> foldMap unNode exprs''
          return $ withMaybeNode stmt' meta
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
          let stmt' =
                annotated <$>
                liftA5
                  (`CallStmt` mConv)
                  (traverse getNode rets'')
                  (getNode func'')
                  (traverse getNode actuals'')
                  (traverse getNode mTargs'')
                  (traverse getNode callAnnots'')
              meta =
                foldMap unNode rets'' <>
                unNode func'' <>
                foldMap unNode actuals'' <>
                foldMap unNode mTargs'' <> foldMap unNode callAnnots''
          return $ withMaybeNode stmt' meta
      JumpStmt {} -> undefined
      ReturnStmt mConv Nothing actuals -> do
        actuals' <- traverse (ensuredJustMonomorphize undefined subst) actuals
        return $ do
          actuals'' <- sequence actuals'
          let stmt' =
                annotated . ReturnStmt mConv Nothing <$>
                traverse getNode actuals''
              meta = foldMap unNode actuals''
          return $ withMaybeNode stmt' meta
      ReturnStmt _ (Just _) _ -> undefined
      LabelStmt {} ->
        getTypeHandleIdPolyKind subst a >>= \case
          Mono -> succeed $ withNode annotatedStmt nullVal
          Poly -> return $ Left undefined -- TODO: local label cannot be a polytype
          Absurd -> return $ Left undefined -- TODO: local label cannot have an illegal type
      ContStmt _ _ -> undefined
      GotoStmt expr targets -> do
        expr' <- ensuredJustMonomorphize undefined subst expr
        targets' <- traverse (ensuredJustMonomorphize undefined subst) targets
        return $ do
          expr'' <- expr'
          targets'' <- sequence targets'
          let stmt' =
                annotated <$>
                liftA2 GotoStmt (getNode expr'') (traverse getNode targets'')
              meta = unNode expr'' <> foldMap unNode targets''
          return $ withMaybeNode stmt' meta
      CutToStmt {} -> undefined
    where
      annotated = withAnnot a
      annotatedStmt = withAnnot a stmt

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl KindName KindName a where
  monomorphizeImpl _ a kindName =
    succeed . monomorphizedTrivial . Just $ withAnnot a kindName

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl LValue LValue a where
  monomorphizeImpl subst a lValue =
    getTypeHandleIdPolyKind subst a >>= \case
      Poly -> return $ Left undefined -- TODO: lValue cannot be a polytype
      Absurd -> return $ Left undefined -- TODO: lValue cannot have an illegal type
      Mono ->
        case lValue of
          LVName _ -> succeed $ withNode (withAnnot a lValue) nullVal
          LVInst (Annot (LVName _) b) -> do
            getTypeHandleIdPolyKind subst b >>= \case
              Mono -> succeed $ withNode (withAnnot a lValue) nullVal
              Poly -> do
                let meta =
                      nullVal &
                      polyGenerate %~
                      addGenerate
                        (getTypeHandle b)
                        (Set.singleton $ getTypeHandle a)
                succeed $ withNode (withAnnot a lValue) meta
              Absurd -> return $ Left undefined
          LVInst _ -> return $ Left undefined -- logic error
          LVRef type' expr mAsserts -> do
            type'' <- ensuredJustMonomorphize undefined subst type'
            expr' <- ensuredJustMonomorphize undefined subst expr
            mAsserts' <-
              traverse (ensuredJustMonomorphize undefined subst) mAsserts
            return $ do
              type''' <- type''
              expr'' <- expr'
              mAsserts'' <- sequence mAsserts'
              let lValue' =
                    withAnnot a <$>
                    liftA3
                      LVRef
                      (getNode type''')
                      (getNode expr'')
                      (traverse getNode mAsserts'')
                  meta =
                    unNode type''' <> unNode expr'' <> foldMap unNode mAsserts''
              return $ withMaybeNode lValue' meta

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Targets Targets a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Expr Expr a where
  monomorphizeImpl subst a expr =
    case expr of
      LitExpr lit mType -> do
        lit' <- monomorphize subst lit
        mType' <- traverse (monomorphize subst) mType
        return $ do
          lit'' <- lit'
          mType'' <- sequence mType'
          let expr' =
                withAnnot a <$>
                liftA2 LitExpr (getNode lit'') (traverse getNode mType'')
              meta = unNode lit'' <> foldMap unNode mType''
          return $ withMaybeNode expr' meta
      LVExpr lValue -> monomorphizeTrivial subst a LVExpr lValue
      ParExpr expr' -> monomorphizeTrivial subst a ParExpr expr'
      BinOpExpr op left right -> do
        left' <- monomorphize subst left
        right' <- monomorphize subst right
        return $ do
          left'' <- left'
          right'' <- right'
          let expr' =
                withAnnot a <$>
                liftA2 (BinOpExpr op) (getNode left'') (getNode right'')
              meta = unNode left'' <> unNode right''
          return $ withMaybeNode expr' meta
      ComExpr expr' -> monomorphizeTrivial subst a ComExpr expr'
      NegExpr expr' -> monomorphizeTrivial subst a NegExpr expr'
      InfixExpr {} -> undefined
      PrefixExpr {} -> undefined
      MemberExpr {} -> undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Lit Lit a where
  monomorphizeImpl _ a lit = succeed $ nullVal & node ?~ withAnnot a lit

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl AST.Type AST.Type a where
  monomorphizeImpl subst a type' =
    getTypeHandleIdPolyKind subst a >>= \case
      Mono -> do
        type'' <- reconstructOld (getTypeHandleId a) >>= instantiateType
        return $ (monomorphizedTrivial . Just $ withAnnot a type') <$ type''
      Poly -> return $ Left undefined -- TODO: the type has to be concrete
      Absurd -> return $ Left undefined -- TODO: all types have to make sense

instantiateType ::
     MonadInferencer m
  => Type.Type
  -> m (MonomorphizeError `Either` Monomorphized (Const Void) a)
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
     (HasPos a, HasTypeHandle a, MonadInferencer m, MonadIO m)
  => TypeHandle
  -> TypeHandle
  -> Monomorphized n a
  -> m (Either MonomorphizeError (Monomorphized Schematized a))
monomorphizePolyType scheme inst mono =
  case scheme `Map.lookup` view polySchemes mono of
    Just (_, schematized) ->
      case schematized of
        FuncScheme procedure -> do
          scheme' <- reconstructOld $ handleId scheme
          inst' <- reconstructOld $ handleId inst
          case ComplType (AddrType scheme') `unify` inst' of
            Left _ -> return $ Left undefined
            Right (subst, _) ->
              monomorphize subst procedure <&> fmap (node %~ fmap FuncScheme)
        StructScheme _ -> undefined
    Nothing -> undefined

instance (HasPos a, HasTypeHandle a) =>
         MonomorphizeImpl CallAnnot CallAnnot a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Asserts Asserts a where
  monomorphizeImpl = undefined

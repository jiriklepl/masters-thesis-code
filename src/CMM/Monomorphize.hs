{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}

module CMM.Monomorphize where

import safe Control.Applicative (Applicative(liftA2), liftA3)
import safe Control.Lens.Getter ((^.), uses, view)
import qualified Data.Bimap as Bimap
import safe qualified Data.PartialOrd as PartialOrd
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Set (Set)
import safe qualified Data.Set as Set

import safe CMM.AST
import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.AST.Maps (ASTmap(..), ASTmapGen, Constraint, Space)
import safe CMM.Data.Bounds (Bounds(Bounds))
import safe CMM.Inference.Preprocess.State (HasTypeHandle (getTypeHandle), lookupClass, getTypeHandleId)
import safe CMM.Inference.State
  ( MonadInferencer
  , handlize
  , readConstingBounds
  , readKindingBounds
  , unifs, schemes
  )
import safe CMM.Inference.Subst (Apply(apply))
import safe CMM.Inference.Type (IsTyped(freeTypeVars), Type, TypeVar, Scheme)
import safe CMM.Inference.Type as Type
import safe CMM.Inference.TypeHandle (consting, kinding, typing, TypeHandle (TypeHandle), handleId)
import safe CMM.Monomorphize.PolyKind (PolyKind(..))
import safe CMM.Parser.HasPos (HasPos)
import safe CMM.Utils (backQuote)
import safe Data.Maybe
import safe CMM.Control.Monad
import safe Control.Monad.IO.Class (MonadIO)
import safe CMM.Monomorphize.Schematized
import CMM.Data.Nullable (Nullable (nullVal), Fallbackable ((??)))
import CMM.AST.HasName
import Control.Lens.Setter ((%~))
import Data.Function ((&))
import CMM.Monomorphize.Monomorphized
import Data.Foldable (fold)

data MonomorphizeError

class Monomorphize n n' a | n -> n' where
  monomorphize ::
       (HasPos a, HasTypeHandle a, MonadInferencer m) => n a -> m (MonomorphizeError `Either` n' a)

class MonomorphizeImpl n n' a | n -> n' where
  monomorphizeImpl :: (HasPos a, HasTypeHandle a, MonadInferencer m) => a -> n a -> m (MonomorphizeError `Either` Monomorphized (Annot n') a)

instance {-# OVERLAPPABLE #-} MonomorphizeImpl n n' a => Monomorphize (Annot n) (Monomorphized (Annot n')) a where
  monomorphize (Annot n a) = monomorphizeImpl a n

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

typePolyKind :: MonadInferencer m => TypeVar -> m PolyKind
typePolyKind tVar =
  uses handlize (flip Bimap.lookup) <*> uses unifs (`apply` tVar) >>= \case
    Just handle ->
      mappend (typingPolyKind $ handle ^. typing) <$>
      liftA2
        mappend
        (kindingPolyKind $ handle ^. kinding)
        (kindingPolyKind $ handle ^. consting)
    Nothing ->
      error $
      "(internal logic error) Type variable " <>
      backQuote (show tVar) <> " not registered by the inferencer."

getTypeHandleIdPolyKind :: (MonadInferencer m, HasTypeHandle a) => a -> m PolyKind
getTypeHandleIdPolyKind = typePolyKind . getTypeHandleId

monomorphizeTrivial :: (MonadInferencer m, MonadIO m,
  Monomorphize p (Monomorphized (Annot p')) a, HasPos a,
  HasTypeHandle a) =>
  a
  -> (Annot p' a -> n a)
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeTrivial annot constr =
  monomorphizeMaybe Nothing $ Just . withAnnot annot . constr

monomorphizeEmpty :: (MonadInferencer m, MonadIO m,
  Monomorphize p (Monomorphized (Annot p')) a, HasPos a,
  HasTypeHandle a) =>
  p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeEmpty =
  monomorphizeMaybe Nothing $ const Nothing

monomorphizeTrivials :: (MonadInferencer m,
  MonadIO m, Monomorphize p (Monomorphized p') a, HasPos a,
  HasTypeHandle a) =>
  a
  -> ([p' a] -> n a)
  -> [p a]
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeTrivials = (. (Just .)) . monomorphizeMaybes

ensureJustMonomorphized :: Monad m =>
  a1
  -> Either a1 (Monomorphized n a2)
  -> m (Either a1 (Monomorphized n a2))
ensureJustMonomorphized err mono = do
  return $ do
    mono' <- mono
    case mono' ^. node of
      Nothing -> Left err
      Just _ -> mono

ensuredJustMonomorphize :: (Monomorphize n1 (Monomorphized n2) a, HasPos a, HasTypeHandle a,
  MonadInferencer m, MonadIO m) =>
  MonomorphizeError
  -> n1 a -> m (Either MonomorphizeError (Monomorphized n2 a))
ensuredJustMonomorphize err n =
  monomorphize n >>= ensureJustMonomorphized err

monomorphizeMaybes :: (MonadInferencer m,
 MonadIO m, Monomorphize p (Monomorphized p') a, HasPos a,
 HasTypeHandle a) =>
  a
  -> ([p' a] -> Maybe (n a))
  -> [p a]
  -> m (Either MonomorphizeError (Monomorphized (Annotation n) a))
monomorphizeMaybes annot constr pars =
  sequence <$> traverse monomorphize pars >>@=  \pars' -> do
    let
      generate' = foldGetGenerate pars'
      schemes' = foldGetSchemes pars'
      node' = (withAnnot annot <$>) . constr . catMaybes $ view node <$> pars'
    return $ monomorphized node' generate' schemes'

monomorphizeMaybe :: (MonadInferencer m, MonadIO m,
  Monomorphize p (Monomorphized (Annot p')) a, HasPos a,
  HasTypeHandle a) =>
  Maybe (Annot n a)
  -> (Annot p' a -> Maybe (Annot n a))
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeMaybe a f = monomorphizeMaybeWithFailure (Right a) (Right . f)

monomorphizeMaybeWithFailure :: (MonadInferencer m, MonadIO m,
  Monomorphize p (Monomorphized (Annot p')) a, HasPos a,
  HasTypeHandle a) =>
  Either MonomorphizeError (Maybe (Annot n a))
  -> (Annot p' a -> Either MonomorphizeError (Maybe (Annot n a)))
  -> p a
  -> m (Either MonomorphizeError (Monomorphized (Annot n) a))
monomorphizeMaybeWithFailure a f par = monomorphize par >>@= \par' ->
  case par' ^. node of
    Nothing -> (`withMaybeNode` par') <$> a
    Just n -> (`withMaybeNode` par') <$> f n

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl TopLevel TopLevel a where
  monomorphizeImpl a = \case
    TopProcedure procedure ->
      monomorphizeTrivial a TopProcedure procedure
    TopDecl decl ->
      monomorphizeMaybeWithFailure (Left undefined) (Right . Just . withAnnot a . TopDecl) decl
    TopSection name items -> undefined
      -- sequence <$> traverse monomorphize items >>@= \items' -> do
      --   let
      --     generate' = foldGetGenerate items'
      --     schemes' = foldGetSchemes items'
      --     node' = withAnnot a . TopSection name . catMaybes $ view node <$> items'
      --   return $ monomorphized (Just node') generate' schemes'
    TopClass class' ->
      monomorphizeMaybeWithFailure (Right Nothing) (Left . undefined) class'
    TopInstance instance' ->
      monomorphizeMaybeWithFailure (Right Nothing) (Left . undefined) instance'
    TopStruct struct ->
      monomorphizeTrivial a TopStruct struct

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Section Section a where
  monomorphizeImpl a = \case
    SecDecl decl ->
      monomorphizeTrivial a SecDecl decl
    SecProcedure procedure ->
      monomorphizeTrivial a SecProcedure procedure
    SecDatum datum ->
      monomorphizeTrivial a SecDatum datum
    SecSpan key value items -> undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Decl Decl a where
  monomorphizeImpl a = \case
    ImportDecl imports ->
      monomorphizeTrivials a ImportDecl imports
    ExportDecl exports ->
      monomorphizeTrivials a ExportDecl exports
    ConstDecl m_an na an -> undefined
    TypedefDecl an ans -> undefined
    RegDecl b an -> undefined
    PragmaDecl na an -> undefined
    TargetDecl ans -> undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl AST.Class AST.Class a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Instance Instance a where
  monomorphizeImpl a (Instance _ _ methods) = do
    monomorphizeMaybes a (const Nothing) methods

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Struct Struct a where
  monomorphizeImpl a (Struct _ datums) = do
    monomorphizeMaybes a (const Nothing) datums

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Import Import a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Export Export a where
  monomorphizeImpl = undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Datum Datum a

succeed :: Applicative f => a -> f (Either err a)
succeed = pure . Right

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Procedure Procedure a where
  monomorphizeImpl a procedure@(Procedure header body) = do
    getTypeHandleIdPolyKind a >>= \case
      Mono -> do
        header' <- ensuredJustMonomorphize undefined header
        body' <- ensuredJustMonomorphize undefined body
        return $ do
          header'' <- header'
          body'' <- body'
          let procedure' = withAnnot a <$> liftA2 Procedure (header'' ^. node) (body'' ^. node)
          let meta = unNode header'' <> unNode body''
          return $ withMaybeNode procedure' meta
      Poly -> uses schemes  (getTypeHandleId a `Map.lookup`) >>= \case
         Nothing -> undefined -- TODO: logic error
         Just scheme -> succeed $ nullVal &
          polySchemes %~ Map.insert (getTypeHandle a) (scheme, FuncScheme procedure)
      Absurd -> return $ Left undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl ProcedureHeader ProcedureHeader a where
  monomorphizeImpl a header =
    succeed . monomorphizedTrivial . Just $ withAnnot a header

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Body Body a where
  monomorphizeImpl a (Body items) =
    monomorphizeTrivials a Body items

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl BodyItem BodyItem a where
  monomorphizeImpl a (BodyDecl decl) =
    monomorphizeTrivial a BodyDecl decl >>= ensureJustMonomorphized undefined
  monomorphizeImpl a (BodyStackDecl decl) =
    monomorphizeTrivial a BodyStackDecl decl >>= ensureJustMonomorphized undefined
  monomorphizeImpl a (BodyStmt decl) =
    monomorphizeTrivial a BodyStmt decl >>= ensureJustMonomorphized undefined

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl StackDecl StackDecl a

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Stmt Stmt a where
  monomorphizeImpl a stmt = case stmt of
    EmptyStmt -> succeed . monomorphizedTrivial . Just $ annotatedStmt
    IfStmt cond tBody eBody -> do
      cond' <- ensuredJustMonomorphize undefined cond
      tBody' <- ensuredJustMonomorphize undefined tBody
      eBody' <- traverse (ensuredJustMonomorphize undefined) eBody
      return $ do
        cond'' <- cond'
        tBody'' <- tBody'
        eBody'' <- sequence eBody'
        let
          stmt' = annotated <$> liftA3 IfStmt (getNode cond'') (getNode tBody'') (getNode <$> eBody'')
          meta = unNode cond'' <> unNode tBody'' <> foldMap unNode eBody''
        return $ withMaybeNode stmt' meta
    SwitchStmt an ans -> undefined
    SpanStmt an an' an2 -> undefined
    AssignStmt lValues exprs -> do
      lValues' <- traverse (ensuredJustMonomorphize undefined) lValues
      exprs' <- traverse (ensuredJustMonomorphize undefined) exprs
      return $ do
        lValues'' <- sequence lValues'
        exprs'' <- sequence exprs'
        let
          stmt' = annotated <$> liftA2 AssignStmt (traverse getNode lValues'') (traverse getNode exprs'')
          meta = foldMap unNode lValues'' <> foldMap unNode exprs''
        return $ withMaybeNode stmt' meta
    PrimOpStmt na na' ans ans' -> undefined
    CallStmt ans m_co an ans' m_an ans2 -> undefined
    JumpStmt m_co an ans m_an -> undefined
    ReturnStmt mConv Nothing actuals -> do
      actuals' <- traverse (ensuredJustMonomorphize undefined) actuals
      return $ do
        actuals'' <- sequence actuals'
        let
          stmt' = annotated . ReturnStmt mConv Nothing <$> traverse getNode actuals''
          meta = foldMap unNode actuals''
        return $ withMaybeNode stmt' meta
    ReturnStmt nConv (Just _) ans -> undefined
    LabelStmt {} ->
      getTypeHandleIdPolyKind a >>= \case
         Mono -> succeed $ withNode annotatedStmt nullVal
         Poly -> return $ Left undefined -- TODO: local label cannot be a polytype
         Absurd -> return $ Left undefined -- TODO: local label cannot have an illegal type
    ContStmt na ans -> undefined
    GotoStmt expr targets -> do
      expr' <- ensuredJustMonomorphize undefined expr
      targets' <- traverse (ensuredJustMonomorphize undefined) targets
      return $ do
        expr'' <- expr'
        targets'' <- sequence targets'
        let
          stmt' = annotated <$> liftA2 GotoStmt (getNode expr'') (traverse getNode targets'')
          meta = unNode expr'' <> foldMap unNode targets''
        return $ withMaybeNode stmt' meta
    CutToStmt an ans ans' -> undefined
    where
      annotated = withAnnot a
      annotatedStmt = withAnnot a stmt

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl LValue LValue a where
  monomorphizeImpl a lValue =
    getTypeHandleIdPolyKind a >>= \case
      Poly -> return $ Left undefined -- TODO: lValue cannot be a polytype
      Absurd -> return $ Left undefined -- TODO: lValue cannot have an illegal type
      Mono -> case lValue of
        LVName name -> undefined
        LVRef type' expr mAsserts -> do
          type'' <- ensuredJustMonomorphize undefined type'
          expr' <- ensuredJustMonomorphize undefined expr
          mAsserts' <- traverse (ensuredJustMonomorphize undefined) mAsserts
          return $ do
            type''' <- type''
            expr'' <- expr'
            mAsserts'' <- sequence  mAsserts'
            let
              lValue' = withAnnot a <$> liftA3 LVRef (getNode type''') (getNode expr'') (traverse getNode mAsserts'')
              meta = unNode type''' <> unNode expr'' <> foldMap unNode mAsserts''
            return $ withMaybeNode lValue' meta

instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Unit Unit a
instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Expr Expr a
instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Targets Targets a
instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Actual Actual a
instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl Asserts Asserts a
instance (HasPos a, HasTypeHandle a) => MonomorphizeImpl AST.Type AST.Type a

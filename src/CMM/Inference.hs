{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module CMM.Inference where

import safe Prelude hiding (init)
import safe qualified Data.Kind as Kind
import safe Data.Text (Text)
import Data.Foldable

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
import safe CMM.Parser.HasPos
import safe CMM.Inference.State
import safe CMM.Inference.Type
import safe CMM.Inference.BuiltIn

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
-- TODO: maybe split this into multiple files according to phases

-- TODO: maybe rename to constraints

unifyConstraint :: TypeHandle -> TypeHandle -> Fact
unifyConstraint = undefined -- TODO: continue from here

kindedType :: Text -> TypeHandle -> TypeHandle -> Fact
kindedType = undefined -- TODO: continue from here

kindedConstraint :: Text  -> TypeHandle -> Fact
kindedConstraint = undefined -- TODO: continue from here

linkExprType :: TypeHandle -> TypeHandle -> Fact
linkExprType = undefined -- TODO: continue from here

constExprType :: TypeHandle -> TypeHandle -> Fact
constExprType = undefined -- TODO: continue from here

constExprConstraint :: TypeHandle -> Fact
constExprConstraint = undefined

registerType :: Text -> TypeHandle -> TypeHandle -> Fact
registerType = undefined -- TODO: continue from here

registerConstraint :: Text  -> TypeHandle -> Fact
registerConstraint = undefined -- TODO: continue from here

classConstraint :: ClassHandle -> [TypeHandle] -> Fact
classConstraint = undefined -- TODO: continue from here

class WithTypeHandle a b => Preprocess n n' a b | n -> n' a, n' -> n b where
    preprocess :: (MonadInferPreprocessor m, HasPos a) => n -> m n'

class (WithTypeHandle a b, Functor n) => PreprocessTrivial (n :: Kind.Type -> Kind.Type) a b where
    preprocessTrivial :: HasPos a => n a -> n b
    preprocessTrivial = (withTypeHandle NoType <$>)

instance PreprocessTrivial n a b => PreprocessTrivial (AST.Annot n) a b

instance {-# OVERLAPPABLE #-} (PreprocessTrivial n a b, Functor n) => Preprocess (AST.Annot n a) (AST.Annot n b) a b where
    preprocess = return . preprocessTrivial

withNoTypeHandleAnnot :: WithTypeHandle a b => a -> n b -> AST.Annot n b
withNoTypeHandleAnnot = withTypeHandledAnnot NoType

withTypeHandledAnnot :: WithTypeHandle a b => TypeHandle -> a -> n b -> AST.Annot n b
withTypeHandledAnnot = (AST.withAnnot .) . withTypeHandle

liftAnnot :: WithTypeHandle a b => (n a -> (TypeHandle, n b)) -> AST.Annot n a -> AST.Annot n b
liftAnnot f (AST.Annot n a) =
    let (handle, n') = f n
    in withTypeHandledAnnot handle a n'

liftAnnotM :: (Monad m, WithTypeHandle a b) => (n a -> m (TypeHandle, n b)) -> AST.Annot n a -> m (AST.Annot n b)
liftAnnotM f (AST.Annot n a) = do
    (handle, n') <- f n
    return $ withTypeHandledAnnot handle a n'

pureAnnotM :: (Monad m, WithTypeHandle a b, Functor n) => TypeHandle -> n a -> m (TypeHandle, n b)
pureAnnotM handle = return . (handle,) . (withTypeHandle handle <$>)

instance Preprocess n n' a b => Preprocess [n] [n'] a b where
    preprocess = traverse preprocess

preprocessConstr :: (Preprocess n1 n1' a1 b1, MonadInferPreprocessor m,
    HasPos a1, WithTypeHandle a2 b2, WithTypeHandle a1 b1) =>
    a2 -> (n1' -> n2 b2) -> n1 -> m (AST.Annot n2 b2)
preprocessConstr a constr content = withNoTypeHandleAnnot a . constr <$> preprocess content

preprocessInherit :: (Preprocess (AST.Annot n1 a1) (AST.Annot n1 b1) a1 b1,
 MonadInferPreprocessor m,
 HasPos a1, WithTypeHandle a2 b2) =>
 a2 -> (AST.Annot n1 b1 -> n2 b2) -> AST.Annot n1 a1 -> m (AST.Annot n2 b2)
preprocessInherit a constr content = do
    content' <- preprocess content
    return . withTypeHandledAnnot (getTypeHandle content') a $ constr content'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Unit a) (AST.Annot AST.Unit b) a b where
    preprocess (AST.Annot (AST.Unit topLevels) a) =
        preprocessConstr a AST.Unit topLevels

instance WithTypeHandle a b => Preprocess (AST.Annot AST.TopLevel a) (AST.Annot AST.TopLevel b) a b where
    preprocess (AST.Annot (AST.TopSection name sectionItems) a) =
        preprocessConstr a (AST.TopSection name) sectionItems
    preprocess (AST.Annot (AST.TopDecl decl) a) =
        preprocessConstr a AST.TopDecl decl
    preprocess (AST.Annot (AST.TopProcedure procedure) a) =
        preprocessConstr a AST.TopProcedure procedure

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Section a) (AST.Annot AST.Section b) a b where
    preprocess (AST.Annot (AST.SecDecl decl) a) =
        preprocessConstr a AST.SecDecl decl
    preprocess (AST.Annot (AST.SecProcedure procedure) a) =
        preprocessConstr a AST.SecProcedure procedure
    preprocess (AST.Annot (AST.SecDatum datum) a) = do
        preprocessConstr a AST.SecDatum datum
    preprocess (AST.Annot (AST.SecSpan key value sectionItems) a) = do
        key' <- preprocess key
        value' <- preprocess value
        sectionItems' <- preprocess sectionItems
        storeFact $ unifyConstraint (getTypeHandle key') (getTypeHandle value')
        return . withNoTypeHandleAnnot a $ AST.SecSpan key' value' sectionItems'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Decl a) (AST.Annot AST.Decl b) a b where
    preprocess (AST.Annot (AST.ImportDecl imports) a) =
        preprocessConstr a AST.ImportDecl imports
    preprocess (AST.Annot (AST.ExportDecl exports) a) =
        preprocessConstr a AST.ExportDecl exports
    preprocess (AST.Annot (AST.RegDecl invar registers) a) =
        preprocessConstr a (AST.RegDecl invar) registers
    preprocess (AST.Annot (AST.PragmaDecl name pragma) a) =
        preprocessConstr a (AST.PragmaDecl $ preprocessTrivial name) pragma
    preprocess (AST.Annot (AST.TargetDecl targetDirectives) a) =
        preprocessConstr a AST.TargetDecl targetDirectives
    -- the constant is typed implicitly
    preprocess (AST.Annot (AST.ConstDecl Nothing name expr) a) = do
        expr' <- preprocess expr
        storeVar (AST.getName name) (getTypeHandle expr')
        return . withNoTypeHandleAnnot a $ AST.ConstDecl Nothing (preprocessTrivial name) expr'
    -- the constant is typed explicitly
    preprocess (AST.Annot (AST.ConstDecl (Just type') name expr) a) = do
        expr' <- preprocess expr
        type'' <- preprocess type'
        let handle = getTypeHandle type''
        storeVar (AST.getName name) handle
        storeFact $ unifyConstraint handle (getTypeHandle expr')
        return . withNoTypeHandleAnnot a $ AST.ConstDecl Nothing (preprocessTrivial name) expr'
    preprocess typedef@(AST.Annot (AST.TypedefDecl type' names) _) = do
        type'' <- preprocess type'
        let handle = getTypeHandle type''
        traverse_ (`storeTVar` handle) (AST.getName <$> names)
        return $ withTypeHandle handle <$> typedef

instance WithTypeHandle a b => PreprocessTrivial AST.TargetDirective a b
instance WithTypeHandle a b => PreprocessTrivial AST.Pragma a b
instance WithTypeHandle a b => PreprocessTrivial AST.Asserts a b
instance WithTypeHandle a b => PreprocessTrivial AST.Name a b

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Import a) (AST.Annot AST.Import b) a b where
    preprocess = liftAnnotM $ \case
        import'@AST.Import{} -> do
            handle <- freshTypeHandle
            storeVar (AST.getName import') handle
            pureAnnotM handle import'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Export a) (AST.Annot AST.Export b) a b where
    preprocess = liftAnnotM $ \case
        export@AST.Export{} -> do
            handle <- freshTypeHandle
            storeVar (AST.getName export) handle
            pureAnnotM handle export

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Type a) (AST.Annot AST.Type b) a b where
    preprocess = liftAnnotM $ \case
        tBits@(AST.TBits int) -> pureAnnotM (TBitsType int) tBits
        tName@(AST.TName name) -> do
            handle <- lookupTVar (AST.getName name)
            pureAnnotM handle tName

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Registers a) (AST.Annot AST.Registers b) a b where
    preprocess = liftAnnotM $ \(AST.Registers mKind type' nameStrLits) -> do
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        handle <- case mKind of
            Just kind -> do
                handle <- freshTypeHandle
                storeFact $ kindedType (AST.getName kind) typeType handle
                return handle
            Nothing -> return typeType
        let go (name, Nothing) = do
                storeVar (AST.getName name) handle
                return (withTypeHandle handle <$> name, Nothing)
            go (name, Just (AST.StrLit strLit)) = do
                handle' <- freshTypeHandle
                storeFact $ registerType strLit handle handle'
                storeVar (AST.getName name) handle'
                return (withTypeHandle handle' <$> name, Just (AST.StrLit strLit))
        nameStrLits' <- traverse go nameStrLits
        return (NoType, AST.Registers mKind type'' nameStrLits')

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Procedure a) (AST.Annot AST.Procedure b) a b where

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Lit a) (AST.Annot AST.Lit b) a b where

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Actual a) (AST.Annot AST.Actual b) a b where
    preprocess = liftAnnotM $
        \(AST.Actual mKind expr) -> do
            expr' <- preprocess expr
            let exprType = getTypeHandle expr'
            case mKind of
                Just kind -> do
                    handle <- freshTypeHandle
                    storeFact $ kindedType (AST.getName kind) exprType handle
                    return (handle, AST.Actual mKind expr')
                Nothing -> return (exprType, AST.Actual mKind expr')

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Init a) (AST.Annot AST.Init b) a b where
    preprocess = liftAnnotM $ \case
        (AST.ExprInit exprs) -> do
            exprs' <- traverse preprocess exprs
            handle <- freshTypeHandle
            traverse_ (storeFact . unifyConstraint handle . getTypeHandle) exprs'
            return (handle, AST.ExprInit exprs')
        strInit@AST.StrInit{} -> pureAnnotM StringType strInit
        strInit@AST.Str16Init{} -> pureAnnotM String16Type strInit

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Datum a) (AST.Annot AST.Datum b) a b where
    preprocess = liftAnnotM $ \case
        datum@(AST.DatumLabel name) -> do
            let handle = AddrType NoType
            storeVar (AST.getName name) handle
            pureAnnotM handle datum
        datum@(AST.DatumAlign _) -> do
            pureAnnotM NoType datum
        (AST.Datum type' mSize mInit) -> do
            type'' <- preprocess type'
            let typeType = getTypeHandle type''
            mSize' <- case mSize of
                Just (AST.Annot (AST.Size (Just expr)) a') -> do
                    expr' <- preprocess expr
                    let exprType = getTypeHandle expr'
                    storeFact $ constExprConstraint exprType
                    return . Just . withTypeHandledAnnot exprType a' . AST.Size $ Just expr'
                    -- TODO: maybe enforce sizeType being numeric
                Just (AST.Annot (AST.Size Nothing) a') -> do
                    return . Just . withNoTypeHandleAnnot a' $ AST.Size Nothing
                Nothing -> return Nothing
            mInit' <- case mInit of
                Just init -> do
                    init' <- preprocess init
                    let initType = getTypeHandle init'
                    storeFact $ linkExprType typeType initType
                    return $ Just init'
                Nothing -> return Nothing
            return (AddrType typeType, AST.Datum type'' mSize' mInit')

instance WithTypeHandle a b => Preprocess (AST.Annot AST.LValue a) (AST.Annot AST.LValue b) a b where
    preprocess = liftAnnotM $ \case
        lvName@AST.LVName{} -> do
            handle <- lookupVar (AST.getName lvName)
            pureAnnotM handle lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
        (AST.LVRef type' expr mAsserts) -> do
            type'' <- preprocess type'
            expr' <- preprocess expr
            let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
            storeFact $ classConstraint Address [getTypeHandle expr']
            return (getTypeHandle type'', AST.LVRef type'' expr' mAsserts')

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Expr a) (AST.Annot AST.Expr b) a b where
    preprocess (AST.Annot (AST.ParExpr expr) a) =
        preprocessInherit a AST.ParExpr expr
    preprocess (AST.Annot (AST.LVExpr lvalue) a) =
        preprocessInherit a AST.LVExpr lvalue
    preprocess (AST.Annot (AST.BinOpExpr op left right) a) = do
        handle <- freshTypeHandle
        left' <- preprocess left
        right' <- preprocess right
        let leftType = getTypeHandle left'
        storeFact $ unifyConstraint leftType (getTypeHandle right')
        -- TODO: add constraint dependent on the operator
        return $ withTypeHandledAnnot handle a $ AST.BinOpExpr op left' right'
    preprocess (AST.Annot (AST.NegExpr expr) a) =
        preprocessInherit a AST.NegExpr expr
        -- TODO: add constraint dependent on the operator
    preprocess (AST.Annot (AST.ComExpr expr) a) =
        preprocessInherit a AST.ComExpr expr
        -- TODO: add constraint dependent on the operator
    preprocess (AST.Annot (AST.LitExpr lit mType) a) = do
        lit' <- preprocess lit
        let litType = getTypeHandle lit'
        mType' <- case mType of
            Nothing -> return Nothing
            Just type' -> do
                type'' <- preprocess type'
                let typeType = getTypeHandle type''
                storeFact $ constExprType typeType litType
                return $ Just type''
        return . withTypeHandledAnnot litType a $ AST.LitExpr lit' mType'
    preprocess (AST.Annot (AST.PrefixExpr name actuals) a) = do
        handle <- freshTypeHandle
        actuals' <- traverse preprocess actuals
        let actualTypes = getTypeHandle <$> actuals'
            (mClassHandle, opType) = getNamedOperator $ AST.getName name
        storeFact $ unifyConstraint opType (makeFunction (makeTuple actualTypes) handle)
        case mClassHandle of
            Nothing -> return ()
            Just classHandle -> storeFact $ classConstraint classHandle actualTypes
        return . withTypeHandledAnnot handle a $ AST.PrefixExpr (preprocessTrivial name) actuals'
    preprocess (AST.Annot (AST.InfixExpr name left right) a) = do
        handle <- freshTypeHandle
        left' <- preprocess left
        right' <- preprocess right
        let leftType = getTypeHandle left'
            rightType = getTypeHandle left'
            (mClassHandle, opType) = getNamedOperator $ AST.getName name
        case mClassHandle of
            Nothing -> return ()
            Just classHandle -> storeFact $ classConstraint classHandle [leftType, rightType]
        storeFact $ unifyConstraint opType (makeFunction (makeTuple [leftType, rightType]) handle)
        return . withTypeHandledAnnot handle a $ AST.InfixExpr (preprocessTrivial name) left' right'

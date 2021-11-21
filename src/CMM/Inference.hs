{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CMM.Inference where

import safe Prelude hiding (init)
import safe qualified Data.Kind as Kind
import safe Data.Text (Text)
import Data.Foldable

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
-- import safe CMM.Type
import safe CMM.Parser.HasPos
import safe CMM.Inference.State

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

withNoTypeHandle :: WithTypeHandle a b => a -> n b -> AST.Annot n b
withNoTypeHandle = AST.withAnnot . withTypeHandle NoType

instance Preprocess n n' a b => Preprocess [n] [n'] a b where
    preprocess = traverse preprocess

preprocessConstr :: (Preprocess n1 n1' a1 b1, MonadInferPreprocessor m,
    HasPos a1, WithTypeHandle a2 b2, WithTypeHandle a1 b1) =>
    a2 -> (n1' -> n2 b2) -> n1 -> m (AST.Annot n2 b2)
preprocessConstr a constr content = withNoTypeHandle a . constr <$> preprocess content

preprocessInherit :: (Preprocess (AST.Annot n1 a1) (AST.Annot n1 b1) a1 b1,
 MonadInferPreprocessor m,
 HasPos a1, WithTypeHandle a2 b2) =>
 a2 -> (AST.Annot n1 b1 -> n2 b2) -> AST.Annot n1 a1 -> m (AST.Annot n2 b2)
preprocessInherit a constr content = do
    content' <- preprocess content
    return . AST.withAnnot (withTypeHandle (getTypeHandle content') a) $ constr content'

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
        return . withNoTypeHandle a $ AST.SecSpan key' value' sectionItems'

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
        return . withNoTypeHandle a $ AST.ConstDecl Nothing (preprocessTrivial name) expr'
    -- the constant is typed explicitly
    preprocess (AST.Annot (AST.ConstDecl (Just type') name expr) a) = do
        expr' <- preprocess expr
        type'' <- preprocess type'
        let handle = getTypeHandle type''
        storeVar (AST.getName name) handle
        storeFact $ unifyConstraint handle (getTypeHandle expr')
        return . withNoTypeHandle a $ AST.ConstDecl Nothing (preprocessTrivial name) expr'
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
    preprocess import'@(AST.Annot AST.Import{} _) = do
        handle <- freshTypeHandle
        storeVar (AST.getName import') handle
        return $ withTypeHandle handle <$> import'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Export a) (AST.Annot AST.Export b) a b where
    preprocess export@(AST.Annot AST.Export{} _) = do
        handle <- freshTypeHandle
        storeVar (AST.getName export) handle
        return $ withTypeHandle handle <$> export

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Type a) (AST.Annot AST.Type b) a b where
    preprocess tBits@(AST.Annot (AST.TBits int) _) =
        return $ withTypeHandle (TBitsType int) <$> tBits
    preprocess tName@(AST.Annot (AST.TName name) _) = do
        handle <- lookupTVar (AST.getName name)
        return $ withTypeHandle handle <$> tName

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Registers a) (AST.Annot AST.Registers b) a b where
    preprocess (AST.Annot (AST.Registers mKind type' nameStrLits) a) = do
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
        return . withNoTypeHandle a $ AST.Registers mKind type'' nameStrLits'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Procedure a) (AST.Annot AST.Procedure b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Lit a) (AST.Annot AST.Lit b) a b where

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Init a) (AST.Annot AST.Init b) a b where
    preprocess (AST.Annot (AST.ExprInit exprs) a) = do
        exprs' <- traverse preprocess exprs
        handle <- freshTypeHandle
        traverse_ (storeFact . unifyConstraint handle . getTypeHandle) exprs'
        return . AST.withAnnot (withTypeHandle handle a) $ AST.ExprInit exprs'
    preprocess init@(AST.Annot (AST.StrInit _) _) =
        return $ withTypeHandle StringType <$> init
    preprocess init@(AST.Annot (AST.Str16Init _) _) =
        return $ withTypeHandle String16Type <$> init

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Datum a) (AST.Annot AST.Datum b) a b where
    preprocess datum@(AST.Annot (AST.DatumLabel name) _) = do
        let handle = AddrType NoType
        storeVar (AST.getName name) handle
        return $ withTypeHandle handle <$> datum
    preprocess datum@(AST.Annot (AST.DatumAlign _) _) =
        return $ withTypeHandle NoType <$> datum
    preprocess (AST.Annot (AST.Datum type' mSize mInit) a) = do
        type'' <- preprocess type'
        let typeType = getTypeHandle type''
        mSize' <- case mSize of
            Just (AST.Annot (AST.Size (Just expr)) a') -> do
                expr' <- preprocess expr
                let exprType = getTypeHandle expr'
                storeFact $ constExprConstraint exprType
                return . Just . AST.withAnnot (withTypeHandle exprType a') . AST.Size $ Just expr'
                -- TODO: maybe enforce sizeType being numeric
            Just (AST.Annot (AST.Size Nothing) a') -> do
                return . Just . withNoTypeHandle a' $ AST.Size Nothing
            Nothing -> return Nothing
        mInit' <- case mInit of
            Just init -> do
                init' <- preprocess init
                let initType = getTypeHandle init'
                storeFact $ linkExprType typeType initType
                return $ Just init'
            Nothing -> return Nothing
        return . AST.withAnnot (withTypeHandle (AddrType typeType) a) $ AST.Datum type'' mSize' mInit'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.LValue a) (AST.Annot AST.LValue b) a b where
    preprocess lvName@(AST.Annot AST.LVName{} _) = do
        handle <- lookupVar (AST.getName lvName)
        return $ withTypeHandle handle <$> lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
    preprocess (AST.Annot (AST.LVRef type' expr mAsserts) a) = do
        type'' <- preprocess type'
        expr' <- preprocess expr
        let mAsserts' = (withTypeHandle NoType <$>) <$> mAsserts
        storeFact $ classConstraint Address [getTypeHandle expr']
        return $ AST.withAnnot (withTypeHandle (getTypeHandle type'') a) $ AST.LVRef type'' expr' mAsserts'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Expr a) (AST.Annot AST.Expr b) a b where
    preprocess (AST.Annot (AST.ParExpr expr) a) =
        preprocessInherit a AST.ParExpr expr
    preprocess (AST.Annot (AST.LVExpr lvalue) a) =
        preprocessInherit a AST.LVExpr lvalue
    preprocess (AST.Annot (AST.BinOpExpr op left right) a) = do
        left' <- preprocess left
        right' <- preprocess right
        handle <- freshTypeHandle
        let leftType = getTypeHandle left'
        storeFact $ unifyConstraint leftType (getTypeHandle right')
        -- TODO: add constraint dependent on the operator
        return $ AST.withAnnot (withTypeHandle handle a) $ AST.BinOpExpr op left' right'
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
        return . AST.withAnnot (withTypeHandle litType a) $ AST.LitExpr lit' mType'

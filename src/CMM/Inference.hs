{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module CMM.Inference where

import safe qualified Data.Kind as Kind
import safe Data.Text (Text)
import Data.Foldable

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
import safe CMM.Type
import safe CMM.Parser.HasPos
import safe CMM.Inference.InferPreprocessor

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
-- TODO: maybe split this into multiple files according to phases

-- TODO: maybe rename to constraints

unifyConstraint :: TypeHandle -> TypeHandle -> Fact
unifyConstraint = undefined -- TODO: continue from here

likeConstraint :: TypeHandle -> TypeHandle -> Fact
likeConstraint = undefined -- TODO: continue from here

kindedConstraint :: Text -> TypeHandle -> Fact
kindedConstraint = undefined -- TODO: continue from here

registerConstraint :: Text -> TypeHandle -> Fact
registerConstraint = undefined -- TODO: continue from here

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
    preprocess (AST.Annot (AST.SecDatum datum) a) =
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
        return $ withTypeHandle (TypeTBits int) <$> tBits
    preprocess tName@(AST.Annot (AST.TName name) _) = do
        handle <- lookupTVar (AST.getName name)
        return $ withTypeHandle handle <$> tName

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Registers a) (AST.Annot AST.Registers b) a b where
    preprocess (AST.Annot (AST.Registers mKind type' nameStrLits) a) = do
        type'' <- preprocess type'
        handle <- freshTypeHandle
        storeFact $ likeConstraint (getTypeHandle type'') handle
        case mKind of
            Just kind -> storeFact $ kindedConstraint (AST.getName kind) handle
            Nothing -> return ()
        let go (name, Nothing) = do
                storeVar (AST.getName name) handle
                return (withTypeHandle handle <$> name, Nothing)
            go (name, Just (AST.StrLit strLit)) = do
                handle' <- freshTypeHandle
                storeFact $ likeConstraint handle handle'
                storeFact $ registerConstraint strLit handle'
                storeVar (AST.getName name) handle'
                return (withTypeHandle handle' <$> name, Just (AST.StrLit strLit))
        nameStrLits' <- traverse go nameStrLits
        return . withNoTypeHandle a $ AST.Registers mKind type'' nameStrLits'

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Procedure a) (AST.Annot AST.Procedure b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Datum a) (AST.Annot AST.Datum b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.LValue a) (AST.Annot AST.LValue b) a b where
    preprocess lvName@(AST.Annot AST.LVName{} _) = do
        handle <- lookupVar (AST.getName lvName)
        return $ withTypeHandle handle <$> lvName
    -- TODO: is there a constraint on expr? probably yes -> consult with the man
    preprocess lvRef@(AST.Annot (AST.LVRef type' _ _) _) = do
        type'' <- preprocess type'
        return $ withTypeHandle (getTypeHandle type'') <$> lvRef

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Expr a) (AST.Annot AST.Expr b) a b where
    preprocess (AST.Annot (AST.ParExpr expr) a) =
        preprocessInherit a AST.ParExpr expr
    preprocess (AST.Annot (AST.LVExpr lvalue) a) =
        preprocessInherit a AST.LVExpr lvalue

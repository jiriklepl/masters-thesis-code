{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module CMM.Inference where

import safe qualified Data.Kind as Kind
import safe Data.Map (Map)
import safe qualified Data.Map as Map

import safe qualified CMM.AST as AST
import safe qualified CMM.AST.Utils as AST
import safe CMM.Type
import safe CMM.Parser.HasPos
import safe CMM.Inference.InferPreprocessor

-- the main idea is: (AST, pos) -> ((AST, (pos, handle)), (Map handle Type)); where handle is a pseudonym for the variable
-- TODO: maybe split this into multiple files according to phases

-- TODO: maybe rename to constraints
data Fact
type Facts = [Fact]

unifyConstraint :: TypeHandle -> TypeHandle -> Fact
unifyConstraint = undefined -- TODO: continue from here

class WithTypeHandle a b => Preprocess n n' a b | n -> n' a, n' -> n b where
    preprocess :: (MonadInferPreprocessor m, HasPos a) => n -> m (Facts, n')

class WithTypeHandle a b => PreprocessTrivial (n :: Kind.Type -> Kind.Type) a b

instance {-# OVERLAPPABLE #-} (PreprocessTrivial n a b, Functor n) => Preprocess (AST.Annot n a) (AST.Annot n b) a b where
    preprocess n = return (mempty, withTypeHandle NoType <$> n)

withNoTypeHandle :: WithTypeHandle a b => a -> n b -> AST.Annot n b
withNoTypeHandle = AST.withAnnot . withTypeHandle NoType

instance Preprocess n n' a b => Preprocess [n] [n'] a b where
    preprocess nodes = do
        (facts, nodes') <- unzip <$> traverse preprocess nodes
        return (concat facts, nodes')

preprocessConstr :: (Preprocess n1 a1 a2 b1, MonadInferPreprocessor f,
    HasPos a2, WithTypeHandle a3 b2, WithTypeHandle a2 b1) =>
    a3 -> (a1 -> n2 b2) -> n1 -> f (Facts, AST.Annot n2 b2)
preprocessConstr a constr content = preprocessMap a constr <$> preprocess content

preprocessMap :: (Functor f, WithTypeHandle a1 b) =>
    a1 -> (a2 -> n b) -> f a2 -> f (AST.Annot n b)
preprocessMap a constr = (withNoTypeHandle a . constr <$>)

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
        (facts1, key') <- preprocess key
        (facts2, value') <- preprocess value
        (facts3, sectionItems') <- preprocess sectionItems
        let keyType = getTypeHandle key'
            valueType = getTypeHandle value'
        return ( unifyConstraint keyType valueType : facts1 <> facts2 <> facts3
               , withNoTypeHandle a $ AST.SecSpan key' value' sectionItems'
               )

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Decl a) (AST.Annot AST.Decl b) a b where
    preprocess (AST.Annot (AST.ImportDecl imports) a) =
        preprocessConstr a AST.ImportDecl imports
    preprocess (AST.Annot (AST.ExportDecl exports) a) =
        preprocessConstr a AST.ExportDecl exports
    preprocess (AST.Annot (AST.RegDecl invar registers) a) =
        preprocessConstr a (AST.RegDecl invar) registers
    preprocess (AST.Annot (AST.PragmaDecl name pragma) a) =
        preprocessConstr a (AST.PragmaDecl (withTypeHandle NoType <$> name)) pragma
    preprocess (AST.Annot (AST.TargetDecl targetDirectives) a) =
        preprocessConstr a AST.TargetDecl targetDirectives

instance WithTypeHandle a b => PreprocessTrivial AST.TargetDirective a b
instance WithTypeHandle a b => PreprocessTrivial AST.Pragma a b
instance WithTypeHandle a b => PreprocessTrivial AST.Asserts a b

instance WithTypeHandle a b => Preprocess (AST.Annot AST.Import a) (AST.Annot AST.Import b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Export a) (AST.Annot AST.Export b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Registers a) (AST.Annot AST.Registers b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Procedure a) (AST.Annot AST.Procedure b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Datum a) (AST.Annot AST.Datum b) a b where
instance WithTypeHandle a b => Preprocess (AST.Annot AST.Expr a) (AST.Annot AST.Expr b) a b where

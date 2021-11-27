{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

#define CASE_HEAD0(constructor) constructor -> pure $ constructor
#define CASE_HEAD(constructor) constructor a -> constructor <$>
#define CASE_HEAD2(constructor) constructor a b -> liftA2 constructor
#define CASE_HEAD3(constructor) constructor a b c -> liftA3 constructor
#define CASE_HEAD4(constructor) constructor a b c d -> liftA4 constructor
#define CASE0(constructor) CASE_HEAD0(constructor)
#define CASE(constructor, f) CASE_HEAD(constructor) f a
#define CASE2(constructor, f, g) CASE_HEAD2(constructor) (f a) (g b)
#define CASE3(constructor, f, g, h) CASE_HEAD3(constructor) (f a) (g b) (h c)
#define CASE4(constructor, f, g, h, i) CASE_HEAD4(constructor) (f a) (g b) (h c) (i d)

module CMM.AST.Maps where

import safe Data.Functor.Identity
import safe qualified Data.Kind as Kind

import safe Control.Applicative
import safe CMM.Control.Applicative
import safe CMM.AST
import safe CMM.AST.Annot

type family Constraint hint a b :: Kind.Constraint
type family Space hint :: Kind.Type -> Kind.Type -> (Kind.Type -> Kind.Type) -> Kind.Constraint

type SpaceAnnot hint a b n = Space hint a b (Annot n)
type ASTmapCTX1 hint a b n = (SpaceAnnot hint a b n, ASTmapGen hint a b)
type ASTmapCTX2 hint a b n m = (SpaceAnnot hint a b n, ASTmapCTX1 hint a b m)
type ASTmapCTX3 hint a b n m o = (SpaceAnnot hint a b n, ASTmapCTX2 hint a b m o)
type ASTmapCTX4 hint a b n m o p = (SpaceAnnot hint a b n, ASTmapCTX3 hint a b m o p)
type ASTmapCTX5 hint a b n m o p q = (SpaceAnnot hint a b n, ASTmapCTX4 hint a b m o p q)
type ASTmapCTX6 hint a b n m o p q r = (SpaceAnnot hint a b n, ASTmapCTX5 hint a b m o p q r)
type ASTmapCTX7 hint a b n m o p q r s = (SpaceAnnot hint a b n, ASTmapCTX6 hint a b m o p q r s)
type ASTmapCTX8 hint a b n m o p q r s t = (SpaceAnnot hint a b n, ASTmapCTX7 hint a b m o p q r s t)
type ASTmapCTX9 hint a b n m o p q r s t u = (SpaceAnnot hint a b n, ASTmapCTX8 hint a b m o p q r s t u)

class ASTmapGen hint a b

class ASTmap hint n a b where
  astMapM :: (Applicative f, Constraint hint a b) =>
    hint -> (forall n' . Space hint a b n' => n' a -> f (n' b))
    -> n a -> f (n b)

astMap :: (ASTmap hint n a b, Constraint hint a b) =>
  hint -> (forall n' . Space hint a b n' => n' a -> n' b)
  -> n a -> n b
astMap hint f = runIdentity . astMapM hint (pure . f)

trivial :: (Applicative f, Functor n) => n a -> f (n b)
trivial = pure . (error "Is not a trivial functor" <$>)

instance ASTmapCTX1 hint a b TopLevel => ASTmap hint Unit a b where
  astMapM _ f (Unit a) = Unit <$> traverse f a

instance ASTmapCTX3 hint a b Decl Procedure Section => ASTmap hint TopLevel a b where
  astMapM _ f = \case
    CASE(TopSection strLit, traverse f)
    CASE(TopDecl, f)
    CASE(TopProcedure, f)

instance ASTmapCTX5 hint a b Datum Decl Expr Procedure Section => ASTmap hint Section a b where
  astMapM _ f = \case
    CASE(SecDecl, f)
    CASE(SecProcedure, f)
    CASE(SecDatum, f)
    CASE3(SecSpan, f, f, traverse f)

instance (ASTmapCTX8 hint a b Export Expr Import Name Pragma Registers TargetDirective Type, Space hint a b Name)  => ASTmap hint Decl a b where
  astMapM _ f = \case
    CASE(ImportDecl, traverse f)
    CASE(ExportDecl, traverse f)
    CASE3(ConstDecl, traverse f, f, f)
    CASE2(TypedefDecl, f, traverse f)
    RegDecl a b -> (RegDecl a) <$> f b
    CASE2(PragmaDecl, f, f)
    CASE(TargetDecl, traverse f)

instance ASTmap hint TargetDirective a b where
  astMapM _ _ = trivial

instance ASTmap hint Import a b where
  astMapM _ _ = trivial

instance ASTmap hint Export a b where
  astMapM _ _ = trivial

instance (ASTmapCTX3 hint a b Init Size Type, Space hint a b Name)  => ASTmap hint Datum a b where
  astMapM _ f = \case
    CASE(DatumLabel, f)
    CASE0(DatumAlign a)
    CASE3(Datum, f, traverse f, traverse f)

instance ASTmapCTX1 hint a b Expr  => ASTmap hint Init a b where
  astMapM _ f = \case
    CASE(ExprInit, traverse f)
    CASE0(StrInit a)
    CASE0(Str16Init a)

instance ASTmapCTX2 hint a b Name Type  => ASTmap hint Registers a b where
  astMapM _ f = \case
    Registers a b c -> liftA2 (Registers a) (f b) (traverse (\(x, y) -> (, y) <$> f x) c)

instance ASTmapCTX1 hint a b Expr  => ASTmap hint Size a b where
  astMapM _ f = \case
    CASE(Size, traverse f)

instance (ASTmapCTX1 hint a b BodyItem) => ASTmap hint Body a b where
  astMapM _ f = \case
    CASE(Body, traverse f)

instance (ASTmapCTX3 hint a b Decl StackDecl Stmt) => ASTmap hint BodyItem a b where
  astMapM _ f = \case
    CASE(BodyDecl, f)
    CASE(BodyStackDecl, f)
    CASE(BodyStmt, f)

instance (ASTmapCTX2 hint a b Body Formal, Space hint a b Name) => ASTmap hint Procedure a b where
  astMapM _ f = \case
    Procedure a b c d -> liftA3 (Procedure a) (f b) (traverse f c) (f d)

instance (ASTmapCTX1 hint a b Type, Space hint a b Name) => ASTmap hint Formal a b where
  astMapM _ f = \case
    Formal a b c d -> liftA2 (Formal a b) (f c) (f d)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Actual a b where
  astMapM _ f = \case
    Actual a b -> Actual a <$> f b

instance ASTmapCTX1 hint a b Datum => ASTmap hint StackDecl a b where
  astMapM _ f = \case
    StackDecl a -> StackDecl <$> traverse f a

instance (ASTmapCTX9 hint a b Actual Arm Body CallAnnot Expr Flow KindName LValue Targets, Space hint a b Name) => ASTmap hint Stmt a b where
  astMapM _ f = \case
    CASE0(EmptyStmt)
    CASE3(IfStmt, f, f, traverse f)
    CASE2(SwitchStmt, f, traverse f)
    CASE3(SpanStmt, f, f, f)
    CASE2(AssignStmt, traverse f, traverse f)
    CASE4(PrimOpStmt, f, f, traverse f, traverse f)
    CallStmt a b c d e g -> liftA6 CallStmt (traverse f a) (pure b) (f c) (traverse f d) (traverse f e) (traverse f g)
    CASE4(JumpStmt, pure, f, traverse f, traverse f)
    ReturnStmt a (Just (b, c)) d -> liftA2 (ReturnStmt a) (Just <$> liftA2 (,) (f b) (f c)) (traverse f d)
    ReturnStmt a Nothing b -> ReturnStmt a Nothing <$> traverse f b
    CASE(LabelStmt, f)
    CASE2(ContStmt, f, traverse f)
    CASE2(GotoStmt, f, traverse f)
    CASE3(CutToStmt, f, traverse f, traverse f)

instance (ASTmapGen hint a b, Space hint a b Name) => ASTmap hint KindName a b where
  astMapM _ f = \case
    KindName a b -> KindName a <$> f b

instance ASTmapCTX2 hint a b Body Range => ASTmap hint Arm a b where
  astMapM _ f = \case
    CASE2(Arm, traverse f, f)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Range a b where
  astMapM _ f = \case
    CASE2(Range, f, traverse f)

instance (ASTmapCTX3 hint a b Asserts Expr Type, Space hint a b Name) => ASTmap hint LValue a b where
  astMapM _ f = \case
    CASE(LVName, f)
    CASE3(LVRef, f, f, traverse f)

instance ASTmapCTX1 hint a b Name => ASTmap hint Flow a b where
  astMapM _ f = \case
    CASE(AlsoCutsTo, traverse f)
    CASE(AlsoUnwindsTo, traverse f)
    CASE(AlsoReturnsTo, traverse f)
    CASE0(AlsoAborts)
    CASE0(NeverReturns)

instance ASTmapCTX1 hint a b Name => ASTmap hint Alias a b where
  astMapM _ f = \case
    CASE(Reads, traverse f)
    CASE(Writes, traverse f)

instance ASTmapCTX2 hint a b Flow Alias => ASTmap hint CallAnnot a b where
  astMapM _ f = \case
    CASE(FlowAnnot, f)
    CASE(AliasAnnot, f)

instance ASTmapCTX1 hint a b Name => ASTmap hint Targets a b where
  astMapM _ f = \case
    CASE(Targets, traverse f)

instance (ASTmapCTX5 hint a b Actual Expr Lit LValue Type, Space hint a b Name) => ASTmap hint Expr a b where
  astMapM _ f = \case
    CASE2(LitExpr, f, traverse f)
    CASE(LVExpr, f)
    CASE(ParExpr, f)
    BinOpExpr a b c -> liftA2 (BinOpExpr a) (f b) (f c)
    CASE(ComExpr, f)
    CASE(NegExpr, f)
    CASE3(InfixExpr, f, f, f)
    CASE2(PrefixExpr, f, traverse f)

instance ASTmap hint Lit a b where
  astMapM _ _ = trivial

instance ASTmap hint Type a b where
  astMapM _ _ = trivial

instance ASTmapCTX1 hint a b Name => ASTmap hint Asserts a b where
  astMapM _ f = \case
    AlignAssert a b -> AlignAssert a <$> traverse f b
    InAssert a b -> liftA2 InAssert (traverse f a) (pure b)

instance ASTmap hint Name a b where
  astMapM _ _ = trivial

instance ASTmap hint Pragma a b where
  astMapM _ _ = trivial

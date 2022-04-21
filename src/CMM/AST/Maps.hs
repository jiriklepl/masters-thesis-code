{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.AST.Maps where

import safe Control.Applicative (Applicative(liftA2, pure), liftA3)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor, (<$>))
import safe Data.Functor.Identity (runIdentity)
import safe qualified Data.Kind as Kind
import safe Data.Maybe (Maybe(Just, Nothing))
import safe Data.Traversable (Traversable(traverse))
import safe GHC.Err (error)

import safe CMM.AST
  ( Actual(Actual)
  , Alias(Reads, Writes)
  , Arm(Arm)
  , Asserts(AlignAssert, InAssert)
  , Body(Body)
  , BodyItem(BodyDecl, BodyStackDecl, BodyStmt)
  , CallAnnot(AliasAnnot, FlowAnnot)
  , Class(Class)
  , Datum(Datum, DatumAlign, DatumLabel)
  , Decl(ConstDecl, ExportDecl, ImportDecl, PragmaDecl, RegDecl,
     TargetDecl, TypedefDecl)
  , Export
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, MemberExpr,
     NegExpr, ParExpr, PrefixExpr)
  , Flow(AlsoAborts, AlsoCutsTo, AlsoReturnsTo, AlsoUnwindsTo,
     NeverReturns)
  , Formal(Formal)
  , Import
  , Init(ExprInit, Str16Init, StrInit)
  , Instance(Instance)
  , KindName(KindName)
  , LValue(LVName, LVRef)
  , Lit
  , Name
  , ParaName(ParaName)
  , ParaType(ParaType)
  , Pragma
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Range(Range)
  , Registers(Registers)
  , Section(SecDatum, SecDecl, SecProcedure, SecSpan)
  , Size(Size)
  , StackDecl(StackDecl)
  , Stmt(AssignStmt, CallStmt, ContStmt, CutToStmt, EmptyStmt,
     GotoStmt, IfStmt, JumpStmt, LabelStmt, PrimOpStmt, ReturnStmt,
     SpanStmt, SwitchStmt)
  , Struct(Struct)
  , TargetDirective
  , Targets(Targets)
  , TopLevel(TopClass, TopDecl, TopInstance, TopProcedure, TopSection,
         TopStruct)
  , Type(TAuto, TBits, TName, TPar)
  , Unit(Unit)
  )
import safe CMM.AST.Annot (Annot)
import safe CMM.Control.Applicative (liftA4, liftA6)

type family Constraint hint a b :: Kind.Constraint

type family Space hint :: Kind.Type -> Kind.Type -> (Kind.Type -> Kind.Type) -> Kind.Constraint

type SpaceAnnot hint a b n = Space hint a b (Annot n)

type ASTmapCTX1 hint a b n = (SpaceAnnot hint a b n, ASTmapGen hint a b)

type ASTmapCTX2 hint a b n m = (SpaceAnnot hint a b n, ASTmapCTX1 hint a b m)

type ASTmapCTX3 hint a b n m o
   = (SpaceAnnot hint a b n, ASTmapCTX2 hint a b m o)

type ASTmapCTX4 hint a b n m o p
   = (SpaceAnnot hint a b n, ASTmapCTX3 hint a b m o p)

type ASTmapCTX5 hint a b n m o p q
   = (SpaceAnnot hint a b n, ASTmapCTX4 hint a b m o p q)

type ASTmapCTX6 hint a b n m o p q r
   = (SpaceAnnot hint a b n, ASTmapCTX5 hint a b m o p q r)

type ASTmapCTX7 hint a b n m o p q r s
   = (SpaceAnnot hint a b n, ASTmapCTX6 hint a b m o p q r s)

type ASTmapCTX8 hint a b n m o p q r s t
   = (SpaceAnnot hint a b n, ASTmapCTX7 hint a b m o p q r s t)

type ASTmapCTX9 hint a b n m o p q r s t u
   = (SpaceAnnot hint a b n, ASTmapCTX8 hint a b m o p q r s t u)

class ASTmapGen hint a b

class ASTmap hint n a b where
  astMapM ::
       (Applicative f, Constraint hint a b)
    => hint
    -> (forall n'. Space hint a b n' =>
                     n' a -> f (n' b))
    -> n a
    -> f (n b)

astMap ::
     (ASTmap hint n a b, Constraint hint a b)
  => hint
  -> (forall n'. Space hint a b n' =>
                   n' a -> n' b)
  -> n a
  -> n b
astMap hint f = runIdentity . astMapM hint (pure . f)

trivial :: (Applicative f, Functor n) => n a -> f (n b)
trivial = pure . (error "Is not a trivial functor" <$>)

instance ASTmapCTX1 hint a b TopLevel => ASTmap hint Unit a b where
  astMapM _ f (Unit a) = Unit <$> traverse f a

instance ASTmapCTX6 hint a b Class Decl Instance Procedure Section Struct =>
         ASTmap hint TopLevel a b where
  astMapM _ f =
    \case
      TopSection strLit a -> TopSection strLit <$> traverse f a
      TopDecl a -> TopDecl <$> f a
      TopProcedure a -> TopProcedure <$> f a
      TopClass a -> TopClass <$> f a
      TopInstance a -> TopInstance <$> f a
      TopStruct a -> TopStruct <$> f a

instance ASTmapCTX5 hint a b Datum Decl Expr Procedure Section =>
         ASTmap hint Section a b where
  astMapM _ f =
    \case
      SecDecl a -> SecDecl <$> f a
      SecProcedure a -> SecProcedure <$> f a
      SecDatum a -> SecDatum <$> f a
      SecSpan a b c -> liftA3 SecSpan (f a) (f b) (traverse f c)

instance ( ASTmapCTX8 hint a b Export Expr Import Name Pragma Registers TargetDirective Type
         , Space hint a b Name
         ) =>
         ASTmap hint Decl a b where
  astMapM _ f =
    \case
      ImportDecl a -> ImportDecl <$> traverse f a
      ExportDecl a -> ExportDecl <$> traverse f a
      ConstDecl a b c -> liftA3 ConstDecl (traverse f a) (f b) (f c)
      TypedefDecl a b -> liftA2 TypedefDecl (f a) (traverse f b)
      RegDecl a b -> RegDecl a <$> f b
      PragmaDecl a b -> liftA2 PragmaDecl (f a) (f b)
      TargetDecl a -> TargetDecl <$> traverse f a

instance ASTmapCTX3 hint a b (ParaName Name) (ParaName Type) ProcedureDecl =>
         ASTmap hint Class a b where
  astMapM _ f (Class a b c) = liftA3 Class (traverse f a) (f b) (traverse f c)

instance ASTmapCTX2 hint a b (ParaName Type) Procedure =>
         ASTmap hint Instance a b where
  astMapM _ f (Instance a b c) =
    liftA3 Instance (traverse f a) (f b) (traverse f c)

instance ASTmapCTX2 hint a b (ParaName Name) Datum =>
         ASTmap hint Struct a b where
  astMapM _ f (Struct a b) = liftA2 Struct (f a) (traverse f b)

instance (ASTmapCTX1 hint a b param, Space hint a b Name) =>
         ASTmap hint (ParaName param) a b where
  astMapM _ f (ParaName a b) = liftA2 ParaName (f a) (traverse f b)

instance ASTmap hint TargetDirective a b where
  astMapM _ _ = trivial

instance ASTmap hint Import a b where
  astMapM _ _ = trivial

instance ASTmap hint Export a b where
  astMapM _ _ = trivial

instance (ASTmapCTX3 hint a b Init Size Type, Space hint a b Name) =>
         ASTmap hint Datum a b where
  astMapM _ f =
    \case
      DatumLabel a -> DatumLabel <$> f a
      DatumAlign a -> pure $ DatumAlign a
      Datum a b c -> liftA3 Datum (f a) (traverse f b) (traverse f c)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Init a b where
  astMapM _ f =
    \case
      ExprInit a -> ExprInit <$> traverse f a
      StrInit a -> pure $ StrInit a
      Str16Init a -> pure $ Str16Init a

instance ASTmapCTX2 hint a b Name Type => ASTmap hint Registers a b where
  astMapM _ f =
    \case
      Registers a b c ->
        liftA2 (Registers a) (f b) (traverse (\(x, y) -> (, y) <$> f x) c)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Size a b where
  astMapM _ f =
    \case
      Size a -> Size <$> traverse f a

instance (ASTmapCTX1 hint a b BodyItem) => ASTmap hint Body a b where
  astMapM _ f =
    \case
      Body a -> Body <$> traverse f a

instance (ASTmapCTX3 hint a b Decl StackDecl Stmt) =>
         ASTmap hint BodyItem a b where
  astMapM _ f =
    \case
      BodyDecl a -> BodyDecl <$> f a
      BodyStackDecl a -> BodyStackDecl <$> f a
      BodyStmt a -> BodyStmt <$> f a

instance ASTmapCTX2 hint a b Body ProcedureHeader =>
         ASTmap hint Procedure a b where
  astMapM _ f =
    \case
      Procedure a b -> liftA2 Procedure (f a) (f b)

instance ASTmapCTX1 hint a b ProcedureHeader =>
         ASTmap hint ProcedureDecl a b where
  astMapM _ f =
    \case
      ProcedureDecl a -> ProcedureDecl <$> f a

instance (ASTmapCTX2 hint a b Formal Type, Space hint a b Name) =>
         ASTmap hint ProcedureHeader a b where
  astMapM _ f =
    \case
      ProcedureHeader a b c d ->
        liftA3
          (ProcedureHeader a)
          (f b)
          (traverse f c)
          (traverse (traverse f) d)

instance (ASTmapCTX1 hint a b Type, Space hint a b Name) =>
         ASTmap hint Formal a b where
  astMapM _ f =
    \case
      Formal a b c d -> liftA2 (Formal a b) (f c) (f d)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Actual a b where
  astMapM _ f =
    \case
      Actual a b -> Actual a <$> f b

instance ASTmapCTX1 hint a b Datum => ASTmap hint StackDecl a b where
  astMapM _ f =
    \case
      StackDecl a -> StackDecl <$> traverse f a

instance ( ASTmapCTX9 hint a b Actual Arm Body CallAnnot Expr Flow KindName LValue Targets
         , Space hint a b Name
         ) =>
         ASTmap hint Stmt a b where
  astMapM _ f =
    \case
      EmptyStmt -> pure EmptyStmt
      IfStmt a b c -> liftA3 IfStmt (f a) (f b) (traverse f c)
      SwitchStmt a b -> liftA2 SwitchStmt (f a) (traverse f b)
      SpanStmt a b c -> liftA3 SpanStmt (f a) (f b) (f c)
      AssignStmt a b -> liftA2 AssignStmt (traverse f a) (traverse f b)
      PrimOpStmt a b c d ->
        liftA4 PrimOpStmt (f a) (f b) (traverse f c) (traverse f d)
      CallStmt a b c d e g ->
        liftA6
          CallStmt
          (traverse f a)
          (pure b)
          (f c)
          (traverse f d)
          (traverse f e)
          (traverse f g)
      JumpStmt a b c d ->
        liftA4 JumpStmt (pure a) (f b) (traverse f c) (traverse f d)
      ReturnStmt a (Just (b, c)) d ->
        liftA2 (ReturnStmt a) (Just <$> liftA2 (,) (f b) (f c)) (traverse f d)
      ReturnStmt a Nothing b -> ReturnStmt a Nothing <$> traverse f b
      LabelStmt a -> LabelStmt <$> f a
      ContStmt a b -> liftA2 ContStmt (f a) (traverse f b)
      GotoStmt a b -> liftA2 GotoStmt (f a) (traverse f b)
      CutToStmt a b c -> liftA3 CutToStmt (f a) (traverse f b) (traverse f c)

instance (ASTmapGen hint a b, Space hint a b Name) =>
         ASTmap hint KindName a b where
  astMapM _ f =
    \case
      KindName a b -> KindName a <$> f b

instance ASTmapCTX2 hint a b Body Range => ASTmap hint Arm a b where
  astMapM _ f =
    \case
      Arm a b -> liftA2 Arm (traverse f a) (f b)

instance ASTmapCTX1 hint a b Expr => ASTmap hint Range a b where
  astMapM _ f =
    \case
      Range a b -> liftA2 Range (f a) (traverse f b)

instance (ASTmapCTX3 hint a b Asserts Expr Type, Space hint a b Name) =>
         ASTmap hint LValue a b where
  astMapM _ f =
    \case
      LVName a -> LVName <$> f a
      LVRef a b c -> liftA3 LVRef (f a) (f b) (traverse f c)

instance ASTmapCTX1 hint a b Name => ASTmap hint Flow a b where
  astMapM _ f =
    \case
      AlsoCutsTo a -> AlsoCutsTo <$> traverse f a
      AlsoUnwindsTo a -> AlsoUnwindsTo <$> traverse f a
      AlsoReturnsTo a -> AlsoReturnsTo <$> traverse f a
      AlsoAborts -> pure AlsoAborts
      NeverReturns -> pure NeverReturns

instance ASTmapCTX1 hint a b Name => ASTmap hint Alias a b where
  astMapM _ f =
    \case
      Reads a -> Reads <$> traverse f a
      Writes a -> Writes <$> traverse f a

instance ASTmapCTX2 hint a b Flow Alias => ASTmap hint CallAnnot a b where
  astMapM _ f =
    \case
      FlowAnnot a -> FlowAnnot <$> f a
      AliasAnnot a -> AliasAnnot <$> f a

instance ASTmapCTX1 hint a b Name => ASTmap hint Targets a b where
  astMapM _ f =
    \case
      Targets a -> Targets <$> traverse f a

instance ( ASTmapCTX6 hint a b Actual Expr Lit LValue Name Type
         , Space hint a b Name
         ) =>
         ASTmap hint Expr a b where
  astMapM _ f =
    \case
      LitExpr a b -> liftA2 LitExpr (f a) (traverse f b)
      LVExpr a -> LVExpr <$> f a
      ParExpr a -> ParExpr <$> f a
      BinOpExpr a b c -> liftA2 (BinOpExpr a) (f b) (f c)
      MemberExpr a b -> liftA2 MemberExpr (f a) (f b)
      ComExpr a -> ComExpr <$> f a
      NegExpr a -> NegExpr <$> f a
      InfixExpr a b c -> liftA3 InfixExpr (f a) (f b) (f c)
      PrefixExpr a b -> liftA2 PrefixExpr (f a) (traverse f b)

instance ASTmap hint Lit a b where
  astMapM _ _ = trivial

instance (ASTmapCTX1 hint a b ParaType, Space hint a b Name) =>
         ASTmap hint Type a b where
  astMapM _ f =
    \case
      TBits a -> pure $ TBits a
      TName a -> TName <$> f a
      TAuto a -> TAuto <$> traverse f a
      TPar a -> TPar <$> f a

instance ASTmapCTX1 hint a b Type => ASTmap hint ParaType a b where
  astMapM _ f (ParaType a b) = liftA2 ParaType (f a) (traverse f b)

instance ASTmapCTX1 hint a b Name => ASTmap hint Asserts a b where
  astMapM _ f =
    \case
      AlignAssert a b -> AlignAssert a <$> traverse f b
      InAssert a b -> liftA2 InAssert (traverse f a) (pure b)

instance ASTmap hint Name a b where
  astMapM _ _ = trivial

instance ASTmap hint Pragma a b where
  astMapM _ _ = trivial

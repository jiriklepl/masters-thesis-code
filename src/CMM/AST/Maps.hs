{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module CMM.AST.Maps where

import safe Control.Applicative (Applicative(liftA2), liftA3)
import safe Data.Functor.Identity (runIdentity)
import safe qualified Data.Kind as Kind

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot)
import safe CMM.Control.Applicative (liftA4, liftA6)

type family Constraint hint a b :: Kind.Constraint

type family Space hint :: Kind.Type -> Kind.Type -> (Kind.Type -> Kind.Type) -> Kind.Constraint

type SpaceAnnot hint a b n = Space hint a b (Annot n)

type ASTmapCTX1 hint a b n = SpaceAnnot hint a b n

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

instance ASTmapCTX1 hint a b AST.TopLevel => ASTmap hint AST.Unit a b where
  astMapM _ f (AST.Unit a) = AST.Unit <$> traverse f a

instance ASTmapCTX6 hint a b AST.Class AST.Decl AST.Instance AST.Procedure AST.Section AST.Struct =>
         ASTmap hint AST.TopLevel a b where
  astMapM _ f =
    \case
      AST.TopSection strLit a -> AST.TopSection strLit <$> traverse f a
      AST.TopDecl a -> AST.TopDecl <$> f a
      AST.TopProcedure a -> AST.TopProcedure <$> f a
      AST.TopClass a -> AST.TopClass <$> f a
      AST.TopInstance a -> AST.TopInstance <$> f a
      AST.TopStruct a -> AST.TopStruct <$> f a

instance ASTmapCTX5 hint a b AST.Datum AST.Decl AST.Expr AST.Procedure AST.Section =>
         ASTmap hint AST.Section a b where
  astMapM _ f =
    \case
      AST.SecDecl a -> AST.SecDecl <$> f a
      AST.SecProcedure a -> AST.SecProcedure <$> f a
      AST.SecDatum a -> AST.SecDatum <$> f a
      AST.SecSpan a b c -> liftA3 AST.SecSpan (f a) (f b) (traverse f c)

instance ( ASTmapCTX8 hint a b AST.Export AST.Expr AST.Import AST.Name AST.Pragma AST.Registers AST.TargetDirective AST.Type
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.Decl a b where
  astMapM _ f =
    \case
      AST.ImportDecl a -> AST.ImportDecl <$> traverse f a
      AST.ExportDecl a -> AST.ExportDecl <$> traverse f a
      AST.ConstDecl a b c -> liftA3 AST.ConstDecl (traverse f a) (f b) (f c)
      AST.TypedefDecl a b -> liftA2 AST.TypedefDecl (f a) (traverse f b)
      AST.RegDecl a b -> AST.RegDecl a <$> f b
      AST.PragmaDecl a b -> liftA2 AST.PragmaDecl (f a) (f b)
      AST.TargetDecl a -> AST.TargetDecl <$> traverse f a

instance ASTmapCTX4 hint a b AST.FunDeps (AST.ParaName AST.Name) (AST.ParaName AST.Type) AST.ProcedureDecl =>
         ASTmap hint AST.Class a b where
  astMapM _ f (AST.Class a b c d) =
    liftA4 AST.Class (traverse f a) (f b) (traverse f c) (traverse f d)

instance ASTmapCTX2 hint a b (AST.ParaName AST.Type) AST.Procedure =>
         ASTmap hint AST.Instance a b where
  astMapM _ f (AST.Instance a b c) =
    liftA3 AST.Instance (traverse f a) (f b) (traverse f c)

instance ASTmapCTX2 hint a b (AST.ParaName AST.Name) AST.Datum =>
         ASTmap hint AST.Struct a b where
  astMapM _ f (AST.Struct a b) = liftA2 AST.Struct (f a) (traverse f b)

instance (ASTmapCTX1 hint a b param, Space hint a b AST.Name) =>
         ASTmap hint (AST.ParaName param) a b where
  astMapM _ f (AST.ParaName a b) = liftA2 AST.ParaName (f a) (traverse f b)

instance ASTmapCTX1 hint a b AST.FunDep => ASTmap hint AST.FunDeps a b where
  astMapM _ f (AST.FunDeps a) = AST.FunDeps <$> traverse f a

instance ASTmapCTX1 hint a b AST.Name => ASTmap hint AST.FunDep a b where
  astMapM _ f (AST.FunDep a b) = liftA2 AST.FunDep (traverse f a) (traverse f b)

instance ASTmap hint AST.TargetDirective a b where
  astMapM _ _ = trivial

instance ASTmap hint AST.Import a b where
  astMapM _ _ = trivial

instance ASTmap hint AST.Export a b where
  astMapM _ _ = trivial

instance ( ASTmapCTX3 hint a b AST.Init AST.Size AST.Type
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.Datum a b where
  astMapM _ f =
    \case
      AST.DatumLabel a -> AST.DatumLabel <$> f a
      AST.DatumAlign a -> pure $ AST.DatumAlign a
      AST.Datum a b c d ->
        liftA3 (AST.Datum a) (f b) (traverse f c) (traverse f d)

instance ASTmapCTX1 hint a b AST.Expr => ASTmap hint AST.Init a b where
  astMapM _ f =
    \case
      AST.ExprInit a -> AST.ExprInit <$> traverse f a
      AST.StrInit a -> pure $ AST.StrInit a
      AST.Str16Init a -> pure $ AST.Str16Init a

instance ASTmapCTX2 hint a b AST.Name AST.Type =>
         ASTmap hint AST.Registers a b where
  astMapM _ f =
    \case
      AST.Registers a b c ->
        liftA2 (AST.Registers a) (f b) (traverse (\(x, y) -> (, y) <$> f x) c)

instance ASTmapCTX1 hint a b AST.Expr => ASTmap hint AST.Size a b where
  astMapM _ f =
    \case
      AST.Size a -> AST.Size <$> traverse f a

instance ASTmapCTX1 hint a b AST.BodyItem => ASTmap hint AST.Body a b where
  astMapM _ f =
    \case
      AST.Body a -> AST.Body <$> traverse f a

instance ASTmapCTX3 hint a b AST.Decl AST.StackDecl AST.Stmt =>
         ASTmap hint AST.BodyItem a b where
  astMapM _ f =
    \case
      AST.BodyDecl a -> AST.BodyDecl <$> f a
      AST.BodyStackDecl a -> AST.BodyStackDecl <$> f a
      AST.BodyStmt a -> AST.BodyStmt <$> f a

instance ASTmapCTX2 hint a b AST.Body AST.ProcedureHeader =>
         ASTmap hint AST.Procedure a b where
  astMapM _ f =
    \case
      AST.Procedure a b -> liftA2 AST.Procedure (f a) (f b)

instance ASTmapCTX1 hint a b AST.ProcedureHeader =>
         ASTmap hint AST.ProcedureDecl a b where
  astMapM _ f =
    \case
      AST.ProcedureDecl a -> AST.ProcedureDecl <$> f a

instance ( ASTmapCTX3 hint a b AST.Formal AST.Type AST.SemiFormal
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.ProcedureHeader a b where
  astMapM _ f =
    \case
      AST.ProcedureHeader a b c d ->
        liftA3
          (AST.ProcedureHeader a)
          (f b)
          (traverse f c)
          (traverse (traverse f) d)

instance (ASTmapCTX1 hint a b AST.Type, Space hint a b AST.Name) =>
         ASTmap hint AST.Formal a b where
  astMapM _ f =
    \case
      AST.Formal a b c d -> liftA2 (AST.Formal a b) (f c) (f d)

instance (ASTmapCTX1 hint a b AST.Type) => ASTmap hint AST.SemiFormal a b where
  astMapM _ f =
    \case
      AST.SemiFormal a b -> AST.SemiFormal a <$> f b

instance ASTmapCTX1 hint a b AST.Expr => ASTmap hint AST.Actual a b where
  astMapM _ f =
    \case
      AST.Actual a b -> AST.Actual a <$> f b

instance ASTmapCTX1 hint a b AST.Datum => ASTmap hint AST.StackDecl a b where
  astMapM _ f =
    \case
      AST.StackDecl a -> AST.StackDecl <$> traverse f a

instance ( ASTmapCTX9 hint a b AST.Actual AST.Arm AST.Body AST.CallAnnot AST.Expr AST.Flow AST.KindName AST.LValue AST.Targets
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.Stmt a b where
  astMapM _ f =
    \case
      AST.EmptyStmt -> pure AST.EmptyStmt
      AST.IfStmt a b c -> liftA3 AST.IfStmt (f a) (f b) (traverse f c)
      AST.SwitchStmt a b -> liftA2 AST.SwitchStmt (f a) (traverse f b)
      AST.SpanStmt a b c -> liftA3 AST.SpanStmt (f a) (f b) (f c)
      AST.AssignStmt a b -> liftA2 AST.AssignStmt (traverse f a) (traverse f b)
      AST.PrimOpStmt a b c d ->
        liftA4 AST.PrimOpStmt (f a) (f b) (traverse f c) (traverse f d)
      AST.CallStmt a b c d e g ->
        liftA6
          AST.CallStmt
          (traverse f a)
          (pure b)
          (f c)
          (traverse f d)
          (traverse f e)
          (traverse f g)
      AST.JumpStmt a b c d ->
        liftA4 AST.JumpStmt (pure a) (f b) (traverse f c) (traverse f d)
      AST.ReturnStmt a (Just (b, c)) d ->
        liftA2
          (AST.ReturnStmt a)
          (Just <$> liftA2 (,) (f b) (f c))
          (traverse f d)
      AST.ReturnStmt a Nothing b -> AST.ReturnStmt a Nothing <$> traverse f b
      AST.LabelStmt a -> AST.LabelStmt <$> f a
      AST.ContStmt a b -> liftA2 AST.ContStmt (f a) (traverse f b)
      AST.GotoStmt a b -> liftA2 AST.GotoStmt (f a) (traverse f b)
      AST.CutToStmt a b c ->
        liftA3 AST.CutToStmt (f a) (traverse f b) (traverse f c)
      AST.DroppedStmt a -> AST.DroppedStmt <$> f a

instance Space hint a b AST.Name => ASTmap hint AST.KindName a b where
  astMapM _ f =
    \case
      AST.KindName a b -> AST.KindName a <$> f b

instance ASTmapCTX2 hint a b AST.Body AST.Range => ASTmap hint AST.Arm a b where
  astMapM _ f =
    \case
      AST.Arm a b -> liftA2 AST.Arm (traverse f a) (f b)

instance ASTmapCTX1 hint a b AST.Expr => ASTmap hint AST.Range a b where
  astMapM _ f =
    \case
      AST.Range a b -> liftA2 AST.Range (f a) (traverse f b)

instance ( ASTmapCTX3 hint a b AST.Asserts AST.Expr AST.Type
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.LValue a b where
  astMapM _ f =
    \case
      AST.LVName a -> AST.LVName <$> f a
      AST.LVRef a b c -> liftA3 AST.LVRef (traverse f a) (f b) (traverse f c)

instance ASTmapCTX1 hint a b AST.Name => ASTmap hint AST.Flow a b where
  astMapM _ f =
    \case
      AST.AlsoCutsTo a -> AST.AlsoCutsTo <$> traverse f a
      AST.AlsoUnwindsTo a -> AST.AlsoUnwindsTo <$> traverse f a
      AST.AlsoReturnsTo a -> AST.AlsoReturnsTo <$> traverse f a
      AST.AlsoAborts -> pure AST.AlsoAborts
      AST.NeverReturns -> pure AST.NeverReturns

instance ASTmapCTX1 hint a b AST.Name => ASTmap hint AST.Alias a b where
  astMapM _ f =
    \case
      AST.Reads a -> AST.Reads <$> traverse f a
      AST.Writes a -> AST.Writes <$> traverse f a

instance ASTmapCTX2 hint a b AST.Flow AST.Alias =>
         ASTmap hint AST.CallAnnot a b where
  astMapM _ f =
    \case
      AST.FlowAnnot a -> AST.FlowAnnot <$> f a
      AST.AliasAnnot a -> AST.AliasAnnot <$> f a

instance ASTmapCTX1 hint a b AST.Name => ASTmap hint AST.Targets a b where
  astMapM _ f =
    \case
      AST.Targets a -> AST.Targets <$> traverse f a

instance ( ASTmapCTX6 hint a b AST.Actual AST.Expr AST.Lit AST.LValue AST.Name AST.Type
         , Space hint a b AST.Name
         ) =>
         ASTmap hint AST.Expr a b where
  astMapM _ f =
    \case
      AST.LitExpr a b -> liftA2 AST.LitExpr (f a) (traverse f b)
      AST.LVExpr a -> AST.LVExpr <$> f a
      AST.ParExpr a -> AST.ParExpr <$> f a
      AST.BinOpExpr a b c -> liftA2 (AST.BinOpExpr a) (f b) (f c)
      AST.MemberExpr a b -> liftA2 AST.MemberExpr (f a) (f b)
      AST.ComExpr a -> AST.ComExpr <$> f a
      AST.NegExpr a -> AST.NegExpr <$> f a
      AST.InfixExpr a b c -> liftA3 AST.InfixExpr (f a) (f b) (f c)
      AST.PrefixExpr a b -> liftA2 AST.PrefixExpr (f a) (traverse f b)

instance ASTmap hint AST.Lit a b where
  astMapM _ _ = trivial

instance (ASTmapCTX2 hint a b AST.ParaType AST.Type, Space hint a b AST.Name) =>
         ASTmap hint AST.Type a b where
  astMapM _ f =
    \case
      AST.TBits a -> pure $ AST.TBits a
      AST.TName a -> AST.TName <$> f a
      AST.TPar a -> AST.TPar <$> f a
      AST.TAuto a -> AST.TAuto <$> traverse f a
      AST.TPtr a -> AST.TPtr <$> f a
      AST.TVoid -> pure AST.TVoid
      AST.TBool -> pure AST.TBool
      AST.TLabel -> pure AST.TLabel

instance ASTmapCTX1 hint a b AST.Type => ASTmap hint AST.ParaType a b where
  astMapM _ f (AST.ParaType a b) = liftA2 AST.ParaType (f a) (traverse f b)

instance ASTmapCTX1 hint a b AST.Name => ASTmap hint AST.Asserts a b where
  astMapM _ f =
    \case
      AST.AlignAssert a b -> AST.AlignAssert a <$> traverse f b
      AST.InAssert a b -> liftA2 AST.InAssert (traverse f a) (pure b)

instance ASTmap hint AST.Name a b where
  astMapM _ _ = trivial

instance ASTmap hint AST.Pragma a b where
  astMapM _ _ = trivial

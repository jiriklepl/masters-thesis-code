{-# LANGUAGE Safe #-}

module CMM.AST.Wrap where

import safe Data.Data (Data)

import safe Prettyprinter (Pretty(pretty))

import safe qualified CMM.AST as AST

-- | Wraps an AST node to allow fpr multiple possible nodes to occupy the given place (used, for example, in errors, for convenience)
data ASTWrapper a
  = WrapUnit (AST.Unit a)
  | WrappedTopLevel (AST.TopLevel a)
  | WrappedSection (AST.Section a)
  | WrappedDecl (AST.Decl a)
  | WrappedClass (AST.Class a)
  | WrappedInstance (AST.Instance a)
  | WrappedStruct (AST.Struct a)
  | WrappedTargetDirective (AST.TargetDirective a)
  | WrappedImport (AST.Import a)
  | WrappedExport (AST.Export a)
  | WrappedDatum (AST.Datum a)
  | WrappedInit (AST.Init a)
  | WrappedRegisters (AST.Registers a)
  | WrappedSize (AST.Size a)
  | WrappedBody (AST.Body a)
  | WrappedBodyItem (AST.BodyItem a)
  | WrappedProcedure (AST.Procedure a)
  | WrappedProcedureHeader (AST.ProcedureHeader a)
  | WrappedFormal (AST.Formal a)
  | WrappedSemiFormal (AST.SemiFormal a)
  | WrappedActual (AST.Actual a)
  | WrappedStmt (AST.Stmt a)
  | WrappedKindName (AST.KindName a)
  | WrappedArm (AST.Arm a)
  | WrappedRange (AST.Range a)
  | WrappedLValue (AST.LValue a)
  | WrappedFlow (AST.Flow a)
  | WrappedAlias (AST.Alias a)
  | WrappedCallAnnot (AST.CallAnnot a)
  | WrappedTargets (AST.Targets a)
  | WrappedExpr (AST.Expr a)
  | WrappedLit (AST.Lit a)
  | WrappedType (AST.Type a)
  | WrappedParaType (AST.ParaType a)
  | WrappedAsserts (AST.Asserts a)
  | WrappedPragma (AST.Pragma a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (ASTWrapper ())

instance (Pretty (ASTWrapper a)) where
  pretty =
    \case
      WrapUnit unit -> pretty unit
      WrappedTopLevel topLevel -> pretty topLevel
      WrappedSection section -> pretty section
      WrappedDecl decl -> pretty decl
      WrappedClass class' -> pretty class'
      WrappedInstance instance' -> pretty instance'
      WrappedStruct struct -> pretty struct
      WrappedTargetDirective targetDirective -> pretty targetDirective
      WrappedImport import' -> pretty import'
      WrappedExport export -> pretty export
      WrappedDatum datum -> pretty datum
      WrappedInit init' -> pretty init'
      WrappedRegisters registers -> pretty registers
      WrappedSize size -> pretty size
      WrappedBody body -> pretty body
      WrappedBodyItem bodyItem -> pretty bodyItem
      WrappedProcedure procedure -> pretty procedure
      WrappedProcedureHeader procedureHeader -> pretty procedureHeader
      WrappedFormal formal -> pretty formal
      WrappedSemiFormal semiFormal -> pretty semiFormal
      WrappedActual actual -> pretty actual
      WrappedStmt stmt -> pretty stmt
      WrappedKindName kindName -> pretty kindName
      WrappedArm arm -> pretty arm
      WrappedRange range -> pretty range
      WrappedLValue lValue -> pretty lValue
      WrappedFlow flow -> pretty flow
      WrappedAlias alias -> pretty alias
      WrappedCallAnnot callAnnot -> pretty callAnnot
      WrappedTargets targets -> pretty targets
      WrappedExpr expr -> pretty expr
      WrappedLit lit -> pretty lit
      WrappedType type' -> pretty type'
      WrappedParaType paraType -> pretty paraType
      WrappedAsserts asserts -> pretty asserts
      WrappedPragma pragma -> pretty pragma

-- | Wraps the given node into a `ASTWrapper`
class MakeWrapped n where
  makeWrapped :: n a -> ASTWrapper a

instance MakeWrapped ASTWrapper where
  makeWrapped = id

instance MakeWrapped AST.Unit where
  makeWrapped = WrapUnit

instance MakeWrapped AST.TopLevel where
  makeWrapped = WrappedTopLevel

instance MakeWrapped AST.Section where
  makeWrapped = WrappedSection

instance MakeWrapped AST.Decl where
  makeWrapped = WrappedDecl

instance MakeWrapped AST.Class where
  makeWrapped = WrappedClass

instance MakeWrapped AST.Instance where
  makeWrapped = WrappedInstance

instance MakeWrapped AST.Struct where
  makeWrapped = WrappedStruct

instance MakeWrapped AST.TargetDirective where
  makeWrapped = WrappedTargetDirective

instance MakeWrapped AST.Import where
  makeWrapped = WrappedImport

instance MakeWrapped AST.Export where
  makeWrapped = WrappedExport

instance MakeWrapped AST.Datum where
  makeWrapped = WrappedDatum

instance MakeWrapped AST.Init where
  makeWrapped = WrappedInit

instance MakeWrapped AST.Registers where
  makeWrapped = WrappedRegisters

instance MakeWrapped AST.Size where
  makeWrapped = WrappedSize

instance MakeWrapped AST.Body where
  makeWrapped = WrappedBody

instance MakeWrapped AST.BodyItem where
  makeWrapped = WrappedBodyItem

instance MakeWrapped AST.Procedure where
  makeWrapped = WrappedProcedure

instance MakeWrapped AST.ProcedureHeader where
  makeWrapped = WrappedProcedureHeader

instance MakeWrapped AST.Formal where
  makeWrapped = WrappedFormal

instance MakeWrapped AST.SemiFormal where
  makeWrapped = WrappedSemiFormal

instance MakeWrapped AST.Actual where
  makeWrapped = WrappedActual

instance MakeWrapped AST.Stmt where
  makeWrapped = WrappedStmt

instance MakeWrapped AST.KindName where
  makeWrapped = WrappedKindName

instance MakeWrapped AST.Arm where
  makeWrapped = WrappedArm

instance MakeWrapped AST.Range where
  makeWrapped = WrappedRange

instance MakeWrapped AST.LValue where
  makeWrapped = WrappedLValue

instance MakeWrapped AST.Flow where
  makeWrapped = WrappedFlow

instance MakeWrapped AST.Alias where
  makeWrapped = WrappedAlias

instance MakeWrapped AST.CallAnnot where
  makeWrapped = WrappedCallAnnot

instance MakeWrapped AST.Targets where
  makeWrapped = WrappedTargets

instance MakeWrapped AST.Expr where
  makeWrapped = WrappedExpr

instance MakeWrapped AST.Lit where
  makeWrapped = WrappedLit

instance MakeWrapped AST.Type where
  makeWrapped = WrappedType

instance MakeWrapped AST.ParaType where
  makeWrapped = WrappedParaType

instance MakeWrapped AST.Asserts where
  makeWrapped = WrappedAsserts

instance MakeWrapped AST.Pragma where
  makeWrapped = WrappedPragma

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.CMM where

import Prelude
import Data.Data
import Data.Text (Text)

data Annotation annot node =
  Annot annot node
  deriving stock (Show, Data)

type Annot annot node = Annotation annot (node annot)

takeAnnot :: Annot annot node -> annot
takeAnnot (Annot annot _) = annot

newtype Unit a =
  Unit [Annot a TopLevel]
  deriving stock (Show, Data)

data TopLevel a
  = TopSection Text [Annot a Section]
  | TopDecl (Annot a Decl)
  | TopProcedure (Annot a Procedure)
  deriving stock (Show, Data)

data Section a
  = SecDecl (Annot a Decl)
  | SecProcedure (Annot a Procedure)
  | SecDatum (Annot a Datum)
  | SecSpan (Annot a Expr) (Annot a Expr) [Annot a Section]
  deriving stock (Show, Data)

data Decl a
  = ImportDecl [Annot a Import] -- at least one
  | ExportDecl [Annot a Export] -- at least one
  | ConstDecl (Maybe (Annot a Type)) Name (Annot a Expr)
  | TypedefDecl (Annot a Type) [Name] -- at least one
  | RegDecl Bool (Annot a Registers)
  | PragmaDecl Name (Annot a Pragma)
  | TargetDecl [Annot a TargetDirective]
  deriving stock (Show, Data)

data TargetDirective a
  = MemSize Int
  | ByteOrder Endian
  | PointerSize Int
  | WordSize Int
  deriving stock (Show, Data)

data Endian
  = Little
  | Big
  deriving stock (Show, Data)

data Import a =
  Import (Maybe Text) Name
  deriving stock (Show, Data)

data Export a =
  Export Name (Maybe Text)
  deriving stock (Show, Data)

data Datum a
  = DatumLabel Name
  | DatumAlign Int
  | Datum (Annot a Type) (Maybe (Annot a Size)) (Maybe (Annot a Init))
  deriving stock (Show, Data)

data Init a
  = ExprInit [Annot a Expr]
  | StrInit Text
  | Str16Init Text
  deriving stock (Show, Data)

data Registers a =
  Registers (Maybe Kind) (Annot a Type) [(Name, Maybe Text)] -- at least one
  deriving stock (Show, Data)

newtype Size a =
  Size (Maybe (Annot a Expr))
  deriving stock (Show, Data)

newtype Body a =
  Body [Annot a BodyItem]
  deriving stock (Show, Data)

data BodyItem a
  = BodyDecl (Annot a Decl)
  | BodyStackDecl (Annot a StackDecl)
  | BodyStmt (Annot a Stmt)
  deriving stock (Show, Data)

data Procedure a =
  Procedure (Maybe Conv) Name [Annot a Formal] (Annot a Body)
  deriving stock (Show, Data)

data Formal a =
  Formal (Maybe Kind) Bool (Annot a Type) Name
  deriving stock (Show, Data)

data Actual a =
  Actual (Maybe Kind) (Annot a Expr)
  deriving stock (Show, Data)

newtype Kind =
  Kind Text
  deriving stock (Show, Data)

data KindName a =
  KindName (Maybe Kind) Name
  deriving stock (Show, Data)

newtype StackDecl a =
  StackDecl [Annot a Datum] -- at least one
  deriving stock (Show, Data)

data Stmt a
  = EmptyStmt
  | IfStmt (Annot a Expr) (Annot a Body) (Maybe (Annot a Body))
  | SwitchStmt (Annot a Expr) [Annot a Arm]
  | SpanStmt (Annot a Expr) (Annot a Expr) (Annot a Body)
  | AssignStmt [(Annot a LValue, Annot a Expr)]
  | PrimOpStmt Name Name [Annot a Actual] [Annot a Flow]
  | CallStmt
      [Annot a KindName]
      (Maybe Conv)
      (Annot a Expr)
      [Annot a Actual]
      (Maybe (Annot a Targets))
      [Either (Annot a Flow) (Annot a Alias)]
  | JumpStmt
      (Maybe Conv)
      (Annot a Expr)
      [Annot a Actual]
      (Maybe (Annot a Targets))
  | ReturnStmt
      (Maybe Conv)
      (Maybe (Annot a Expr, Annot a Expr))
      [Annot a Actual]
  | LabelStmt Name
  | ContStmt Name [Annot a KindName]
  | GotoStmt (Annot a Expr) (Maybe (Annot a Targets))
  | CutToStmt (Annot a Expr) [Annot a Actual] [Annot a Flow]
  deriving stock (Show, Data)

data Arm a =
  Arm [Annot a Range] (Annot a Body)
  deriving stock (Show, Data)

data Range a =
  Range (Annot a Expr) (Maybe (Annot a Expr))
  deriving stock (Show, Data)

data LValue a
  = LVName Name
  | LVRef (Annot a Type) (Annot a Expr) (Maybe (Annot a Asserts))
  deriving stock (Show, Data)

data Flow a
  = AlsoCutsTo [Name] -- at least one
  | AlsoUnwindsTo [Name] -- at least one
  | AlsoReturnsTo [Name] -- at least one
  | AlsoAborts
  | NeverReturns
  deriving stock (Show, Data)

-- TODO: should be a part of inference?
data Alias a
  = Reads [Name]
  | Writes [Name]
  deriving stock (Show, Data)

newtype Targets a =
  Targets [Name]
  deriving stock (Show, Data)

data Expr a
  = LitExpr (Annot a Lit) (Maybe (Annot a Type))
  | LVExpr (Annot a LValue)
  | ParExpr (Annot a Expr)
  | BinOpExpr Op (Annot a Expr) (Annot a Expr)
  | ComExpr (Annot a Expr)
  | NegExpr (Annot a Expr)
  | InfixExpr (Annot a Expr) Name (Annot a Expr)
  | PrefixExpr Name [Annot a Actual]
  deriving stock (Show, Data)

data Lit a
  = LitInt Int -- TODO?: think more
  | LitFloat Float
  | LitChar Char
  deriving stock (Show, Data)

data Type a
  = TBits Int
  | TName Name
  deriving stock (Show, Data)

newtype Conv =
  Foreign Text
  deriving stock (Show, Data)

data Asserts a
  = AlignAssert Int [Name]
  | InAssert [Name] (Maybe Int)
  deriving stock (Show, Data)

data Op
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | ModOp
  | AndOp
  | OrOp
  | XorOp
  | ShLOp
  | ShROp
  | EqOp
  | NeqOp
  | GtOp
  | LtOp
  | GeOp
  | LeOp
  deriving stock (Show, Data)

newtype Name =
  Name Text -- TODO: consult this with the manual (and supervisor) (and also add hash etc?)
  deriving stock (Show, Data)

data Pragma a
  deriving stock Data -- TODO: the manual does not specify at all

instance Show (Pragma a) where
  show = undefined -- TODO

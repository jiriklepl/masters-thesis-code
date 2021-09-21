{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.AST where

import Data.Data
import Data.Text (Text)
import Prelude

data Annotation node annot = Annot (node annot) annot deriving (Show, Functor, Data, AST)
deriving instance (Eq (node a), Eq a) => Eq (Annotation node a)

type Annot node annot = Annotation node annot

takeAnnot :: Annot n annot -> annot
takeAnnot (Annot _ annot) = annot

class Functor n => AST n where
  updateAnnots :: (a -> b) -> n a -> n b
  updateAnnots = fmap

  stripAnnots :: n a -> n ()
  stripAnnots = updateAnnots $ const ()

newtype Unit a =
  Unit [Annot TopLevel a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Unit ())

data TopLevel a
  = TopSection Text [Annot Section a]
  | TopDecl (Annot Decl a)
  | TopProcedure (Annot Procedure a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (TopLevel ())

data Section a
  = SecDecl (Annot Decl a)
  | SecProcedure (Annot Procedure a)
  | SecDatum (Annot Datum a)
  | SecSpan (Annot Expr a) (Annot Expr a) [Annot Section a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Section ())

data Decl a
  = ImportDecl [Annot Import a] -- at least one
  | ExportDecl [Annot Export a] -- at least one
  | ConstDecl (Maybe (Annot Type a)) Name (Annot Expr a)
  | TypedefDecl (Annot Type a) [Name] -- at least one
  | RegDecl Bool (Annot Registers a)
  | PragmaDecl Name (Annot Pragma a)
  | TargetDecl [Annot TargetDirective a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Decl ())

data TargetDirective a
  = MemSize Int
  | ByteOrder Endian
  | PointerSize Int
  | WordSize Int
  deriving (Show, Functor, Data, Eq, AST)

data Import a =
  Import (Maybe Text) Name
  deriving (Show, Functor, Data, Eq, AST)

data Export a =
  Export Name (Maybe Text)
  deriving (Show, Functor, Data, Eq, AST)

data Endian
  = Little
  | Big
  deriving (Show, Data, Eq)

data Datum a
  = DatumLabel Name
  | DatumAlign Int
  | Datum (Annot Type a) (Maybe (Annot Size a)) (Maybe (Annot Init a))
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Datum ())

data Init a
  = ExprInit [Annot Expr a]
  | StrInit Text
  | Str16Init Text
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Init ())

data Registers a =
  Registers (Maybe Kind) (Annot Type a) [(Name, Maybe Text)] -- at least one
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Registers ())

newtype Size a =
  Size (Maybe (Annot Expr a))
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Size ())

newtype Body a =
  Body [Annot BodyItem a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Body ())

data BodyItem a
  = BodyDecl (Annot Decl a)
  | BodyStackDecl (Annot StackDecl a)
  | BodyStmt (Annot Stmt a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (BodyItem ())

data Procedure a =
  Procedure (Maybe Conv) Name [Annot Formal a] (Annot Body a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Procedure ())

data Formal a =
  Formal (Maybe Kind) Bool (Annot Type a) Name
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Formal ())

data Actual a =
  Actual (Maybe Kind) (Annot Expr a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Actual ())

newtype Kind =
  Kind Text
  deriving (Show, Data, Eq)

newtype StackDecl a =
  StackDecl [Annot Datum a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (StackDecl ())

data Stmt a
  = EmptyStmt
  | IfStmt (Annot Expr a) (Annot Body a) (Maybe (Annot Body a))
  | SwitchStmt (Annot Expr a) [Annot Arm a]
  | SpanStmt (Annot Expr a) (Annot Expr a) (Annot Body a)
  | AssignStmt [Annot LValue a] [Annot Expr a]
  | PrimOpStmt Name Name [Annot Actual a] [Annot Flow a]
  | CallStmt
      [Annot KindName a]
      (Maybe Conv)
      (Annot Expr a)
      [Annot Actual a]
      (Maybe (Annot Targets a))
      [Annot CallAnnot a]
  | JumpStmt
      (Maybe Conv)
      (Annot Expr a)
      [Annot Actual a]
      (Maybe (Annot Targets a))
  | ReturnStmt
      (Maybe Conv)
      (Maybe (Annot Expr a, Annot Expr a))
      [Annot Actual a]
  | LabelStmt Name
  | ContStmt Name [Annot KindName a]
  | GotoStmt (Annot Expr a) (Maybe (Annot Targets a))
  | CutToStmt (Annot Expr a) [Annot Actual a] [Annot Flow a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Stmt ())

data KindName a =
  KindName (Maybe Kind) Name
  deriving (Show, Functor, Data, Eq, AST)

data Arm a =
  Arm [Annot Range a] (Annot Body a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Arm ())

data Range a =
  Range (Annot Expr a) (Maybe (Annot Expr a))
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Range ())

data LValue a
  = LVName Name
  | LVRef (Annot Type a) (Annot Expr a) (Maybe (Annot Asserts a))
  deriving (Show, Functor, Data, AST)

deriving instance Eq (LValue ())

data Flow a
  = AlsoCutsTo [Name] -- at least one
  | AlsoUnwindsTo [Name] -- at least one
  | AlsoReturnsTo [Name] -- at least one
  | AlsoAborts
  | NeverReturns
  deriving (Show, Functor, Data, Eq, AST)

-- TODO: should be a part of inference?
data Alias a
  = Reads [Name]
  | Writes [Name]
  deriving (Show, Functor, Data, Eq, AST)

data CallAnnot a
  = FlowAnnot (Annot Flow a)
  | AliasAnnot (Annot Alias a)
  deriving (Show, Functor, Data, AST)

deriving instance Eq (CallAnnot ())

newtype Targets a =
  Targets [Name]
  deriving (Show, Functor, Data, Eq, AST)

data Expr a
  = LitExpr (Annot Lit a) (Maybe (Annot Type a))
  | LVExpr (Annot LValue a)
  | ParExpr (Annot Expr a)
  | BinOpExpr Op (Annot Expr a) (Annot Expr a)
  | ComExpr (Annot Expr a)
  | NegExpr (Annot Expr a)
  | InfixExpr Name (Annot Expr a) (Annot Expr a)
  | PrefixExpr Name [Annot Actual a]
  deriving (Show, Functor, Data, AST)

deriving instance Eq (Expr ())

data Lit a
  = LitInt Int -- TODO?: think more
  | LitFloat Float
  | LitChar Char
  deriving (Show, Functor, Data, Eq, AST)

data Type a
  = TBits Int
  | TName Name
  deriving (Show, Functor, Data, Eq, AST)

newtype Conv =
  Foreign Text
  deriving (Show, Data, Eq)

data Asserts a
  = AlignAssert Int [Name]
  | InAssert [Name] (Maybe Int) -- at least one Name
  deriving (Show, Functor, Data, Eq, AST)

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
  deriving (Show, Data, Eq)

newtype Name =
  Name Text
  deriving (Show, Data, Eq)

data Pragma a
  deriving (Functor, Data, AST) -- TODO: the manual does not specify at all

instance Show (Pragma a) where
  show = undefined

instance Eq (Pragma a) where
  (==) = undefined

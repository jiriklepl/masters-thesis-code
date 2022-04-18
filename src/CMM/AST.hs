{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module CMM.AST where

import safe Prelude (Float)

import safe Text.Show ( Show(show) )
import safe Data.Functor ( Functor )
import safe Data.Foldable ( Foldable )
import safe Data.Traversable ( Traversable )
import safe Data.Eq ( Eq((==)) )
import safe Data.Maybe ( Maybe )
import safe Data.Bool ( Bool )
import safe Data.Int ( Int )
import safe Data.Char ( Char )
import safe GHC.Err (error)
import safe Data.Data (Data)
import safe qualified Data.Kind as Kind
import safe Data.Text (Text)

import safe CMM.AST.Annot ( Annot )

class ASTNode (n :: Kind.Type -> Kind.Type)

class AST (n :: Kind.Type -> Kind.Type)

deriving instance ASTNode n => AST (Annot n)

newtype Unit a =
  Unit [Annot TopLevel a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Unit ())

data TopLevel a
  = TopSection StrLit [Annot Section a]
  | TopDecl (Annot Decl a)
  | TopProcedure (Annot Procedure a)
  | TopClass (Annot Class a)
  | TopInstance (Annot Instance a)
  | TopStruct (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (TopLevel ())

data Section a
  = SecDecl (Annot Decl a)
  | SecProcedure (Annot Procedure a)
  | SecDatum (Annot Datum a)
  | SecSpan (Annot Expr a) (Annot Expr a) [Annot Section a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Section ())

data Decl a
  = ImportDecl [Annot Import a] -- at least one
  | ExportDecl [Annot Export a] -- at least one
  | ConstDecl (Maybe (Annot Type a)) (Name a) (Annot Expr a)
  | TypedefDecl (Annot Type a) [Annot Name a] -- at least one
  | RegDecl Bool (Annot Registers a)
  | PragmaDecl (Name a) (Annot Pragma a)
  | TargetDecl [Annot TargetDirective a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Decl ())

data Class a =
  Class
    [Annot (ParaName Type) a]
    (Annot (ParaName Name) a)
    [Annot ProcedureDecl a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Class ())

data Instance a =
  Instance
    [Annot (ParaName Type) a]
    (Annot (ParaName Type) a)
    [Annot Procedure a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Instance ())

data Struct a =
  Struct (Annot (ParaName Name) a) [Annot Datum a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Struct ())

data ParaName param a =
  ParaName (Name a) [Annot param a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (param ()) => Eq (ParaName param ())

data TargetDirective a
  = MemSize Int
  | ByteOrder Endian
  | PointerSize Int
  | WordSize Int
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Import a =
  Import (Maybe StrLit) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Export a =
  Export (Name a) (Maybe StrLit)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Endian
  = Little
  | Big
  deriving (Show, Data, Eq)

data Datum a
  = DatumLabel (Name a)
  | DatumAlign Int
  | Datum (Annot Type a) (Maybe (Annot Size a)) (Maybe (Annot Init a))
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Datum ())

data Init a
  = ExprInit [Annot Expr a]
  | StrInit StrLit
  | Str16Init StrLit
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Init ())

data Registers a =
  Registers (Maybe Kind) (Annot Type a) [(Annot Name a, Maybe StrLit)] -- at least one
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Registers ())

newtype Size a =
  Size (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Size ())

newtype Body a =
  Body [Annot BodyItem a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Body ())

data BodyItem a
  = BodyDecl (Annot Decl a)
  | BodyStackDecl (Annot StackDecl a)
  | BodyStmt (Annot Stmt a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (BodyItem ())

data Procedure a =
  Procedure (Annot ProcedureHeader a) (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Procedure ())

newtype ProcedureDecl a =
  ProcedureDecl (Annot ProcedureHeader a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (ProcedureDecl ())

data ProcedureHeader a =
  ProcedureHeader (Maybe Conv) (Name a) [Annot Formal a] (Maybe [Annot Type a])
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (ProcedureHeader ())

data Formal a =
  Formal (Maybe Kind) Bool (Annot Type a) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Formal ())

data Actual a =
  Actual (Maybe Kind) (Annot Expr a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Actual ())

newtype Kind =
  Kind StrLit
  deriving (Show, Data, Eq)

newtype StackDecl a =
  StackDecl [Annot Datum a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (StackDecl ())

data Stmt a
  = EmptyStmt
  | IfStmt (Annot Expr a) (Annot Body a) (Maybe (Annot Body a))
  | SwitchStmt (Annot Expr a) [Annot Arm a]
  | SpanStmt (Annot Expr a) (Annot Expr a) (Annot Body a)
  | AssignStmt [Annot LValue a] [Annot Expr a]
  | PrimOpStmt (Name a) (Name a) [Annot Actual a] [Annot Flow a]
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
  | LabelStmt (Name a)
  | ContStmt (Name a) [Annot KindName a]
  | GotoStmt (Annot Expr a) (Maybe (Annot Targets a))
  | CutToStmt (Annot Expr a) [Annot Actual a] [Annot Flow a]
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Stmt ())

data KindName a =
  KindName (Maybe Kind) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Arm a =
  Arm [Annot Range a] (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Arm ())

data Range a =
  Range (Annot Expr a) (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Range ())

data LValue a
  = LVName (Name a)
  | LVRef (Annot Type a) (Annot Expr a) (Maybe (Annot Asserts a))
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (LValue ())

data Flow a
  = AlsoCutsTo [Annot Name a] -- at least one
  | AlsoUnwindsTo [Annot Name a] -- at least one
  | AlsoReturnsTo [Annot Name a] -- at least one
  | AlsoAborts
  | NeverReturns
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Alias a
  = Reads [Annot Name a]
  | Writes [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data CallAnnot a
  = FlowAnnot (Annot Flow a)
  | AliasAnnot (Annot Alias a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (CallAnnot ())

newtype Targets a =
  Targets [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Expr a
  = LitExpr (Annot Lit a) (Maybe (Annot Type a))
  | LVExpr (Annot LValue a)
  | ParExpr (Annot Expr a)
  | BinOpExpr Op (Annot Expr a) (Annot Expr a)
  | ComExpr (Annot Expr a)
  | NegExpr (Annot Expr a)
  | InfixExpr (Name a) (Annot Expr a) (Annot Expr a)
  | PrefixExpr (Name a) [Annot Actual a]
  | MemberExpr (Annot Expr a) (Annot Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, ASTNode, AST)

deriving instance Eq (Expr ())

data Lit a
  = LitInt Int
  | LitFloat Float
  | LitChar Char
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data Type a
  = TBits Int
  | TName (Name a)
  | TAuto (Maybe (Name a))
  | TPar (Annot ParaType a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

data ParaType a =
  ParaType (Annot Type a) [Annot Type a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

newtype Conv =
  Foreign StrLit
  deriving (Show, Data, Eq)

data Asserts a
  = AlignAssert Int [Annot Name a]
  | InAssert [Annot Name a] (Maybe Int) -- at least one (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

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

newtype Name a =
  Name Text
  deriving (Show, Functor, Foldable, Traversable, Data, Eq, ASTNode, AST)

newtype StrLit =
  StrLit Text
  deriving (Show, Data, Eq)

data Pragma a
  deriving (Functor, Foldable, Traversable, Data, ASTNode, AST) -- FIXME: the manual does not specify at all

instance Show (Pragma a) where
  show = error "pragmas are not implemented"

instance Eq (Pragma a) where
  (==) = error "pragmas are not implemented"

module Language where

import qualified Data.Text as T
import Data.Text (Text)

-- TODO: categorize names? (different namespaces for variables, operators, etc)

data Unit a = Unit [TopLevel a] a
            deriving (Show)

data TopLevel a = TopSection Text [Section a] a
                | TopDecl (Decl a)
                | TopProcedure (Procedure a)
                deriving (Show)

data Section a = SecDecl (Decl a)
               | SecProcedure (Procedure a)
               | SecDatum (Datum a)
               | SecSpan (Expr a) (Expr a) [Section a] a
               deriving (Show)

data Decl a = ImportDecl [Import a] a -- at least one
            | ExportDecl [Export a] a -- at least one
            | ConstDecl (Maybe (Type a)) Name (Expr a) a
            | TypedefDecl (Type a) [Name] a -- at least one
            | RegDecl (Maybe Invariant) (Registers a) a
            | PragmaDecl Name (Pragma a) a
            | TargetDecl [TargetDirective a] a
            deriving (Show)

data TargetDirective a = MemSize Int a
                       | ByteOrder Endian a
                       | PointerSize Int a
                       | WordSize Int a
                       deriving (Show)

data Endian = Little | Big -- TODO: or just Bool?
            deriving (Show)

data Import a = Import (Maybe Text) Name a
              deriving (Show)

data Export a = Export Name (Maybe Text) a
              deriving (Show)

data Invariant = Invariant -- TODO: or it can be just Bool?
               deriving (Show)

data Datum a = DatumLabel Name a
             | DatumAlign Int a
             | Datum (Type a) (Maybe (Size a)) (Maybe (Init a)) a
             deriving (Show)

data Init a = ExprInit [Expr a] a
            | StrInit Text a
            | Str16Init String16 a
            deriving (Show)

data Registers a = Registers (Maybe Kind) (Type a) [(Name, Maybe Text)] a -- at least one
                 deriving (Show)

data Size a = Size (Maybe (Expr a)) a
            deriving (Show)

data Body a = Body [BodyItem a] a
            deriving (Show)

data BodyItem a = BodyDecl (Decl a)
                | BodyStackDecl (StackDecl a)
                | BodyStmt (Stmt a)
                deriving (Show)

data Procedure a = Procedure (Maybe Conv) Name [Formal a] (Body a) a
                 deriving (Show)

data Formal a = Formal (Maybe Kind) (Maybe Invariant) (Type a) Name a
              deriving (Show)

data Actual a = Actual (Maybe Kind) (Expr a) a
              deriving (Show)

newtype Kind = Kind Text
             deriving (Show)

data KindName a = KindName (Maybe Kind) Name a
                deriving (Show)

data StackDecl a = StackDecl [Datum a] a -- at least one
                 deriving (Show)

data Stmt a = EmptyStmt a
            | IfStmt (Expr a) (Body a) (Maybe (Body a)) a
            | SwitchStmt (Expr a) [Arm a] a
            | SpanStmt (Expr a) (Expr a) (Body a) a
            | AssignStmt [LValue a] [Expr a] a -- TODO?: maybe list of tuples
            | PrimOpStmt Name Name [Actual a] [Flow a] a
            | CallStmt [KindName a] (Maybe Conv) (Expr a) [Actual a] (Maybe (Targets a)) [Either (Flow a) (Alias a)] a
            | JumpStmt (Maybe Conv) (Expr a)  [Actual a] (Maybe (Targets a)) a
            | ReturnStmt (Maybe Conv) (Maybe (Expr a, Expr a)) [Actual a] a
            | LabelStmt Name a
            | ContStmt Name [KindName a] a
            | GotoStmt (Expr a) (Maybe (Targets a)) a
            | CutToStmt (Expr a) [Actual a] [Flow a] a
            deriving (Show)

data Arm a = Arm [Range a] (Body a) a
           deriving (Show)

data Range a = Range (Expr a) (Maybe (Expr a)) a
             deriving (Show)

data LValue a = LVName Name a -- TODO?: name
              | LVRef (Type a) (Expr a) (Maybe (Asserts a)) a -- TODO?: name
              deriving (Show)

data Flow a = AlsoCutsTo [Name] a -- at least one
            | AlsoUnwindsTo [Name] a -- at least one
            | AlsoReturnsTo [Name] a -- at least one
            | AlsoAborts a
            | NeverReturns a
            deriving (Show)
-- TODO: should be a part of inference?

data Alias a = Reads [Name] a | Writes [Name] a
             deriving (Show)

data Targets a = Targets [Name] a
               deriving (Show)

data Expr a = LitExpr (Lit a) (Maybe (Type a)) a
            | NameExpr Name a
            | RefExpr (Type a) (Expr a) (Maybe (Asserts a)) a
            | ParExpr (Expr a) a
            | BinOpExpr (Expr a) Op (Expr a) a
            | ComExpr (Expr a) a
            | NegExpr (Expr a) a
            | InfixExpr (Expr a) Name (Expr a) a
            | PrefixExpr Name [Actual a] a
            deriving (Show)

data Lit a = LitInt Int a -- TODO?: think more
           | LitFloat Float a
           | LitChar Char a
           deriving (Show)

data Type a = TBits Int a -- TODO?: name
            | TName Name a -- TODO?: name
            deriving (Show)

newtype String16 = String16 Text -- TODO: implement String16
                 deriving (Show)

newtype Conv = Foreign Text
             deriving (Show)

data Asserts a = AlignAssert Int [Name] a
               | InAssert [Name] (Maybe Int) a
               deriving (Show)

data Op = AddOp | SubOp | MulOp | DivOp | ModOp | AndOp | OrOp | XorOp | ShLOp | ShROp | EqOp | NeqOp | GtOp | LtOp | GeOp | LeOp
        deriving (Show)

newtype Name = Name Text -- TODO: consult this with the manual (and mirek) (and also add hash etc?)
             deriving (Show)

data Pragma a -- TODO: the manual does not specify at all

instance Show (Pragma a) where
    show = undefined -- TODO

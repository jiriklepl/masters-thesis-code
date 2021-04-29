module Language where

import qualified Data.Text as T
import Data.Text (Text)

-- TODO: categorize names? (different namespaces for variables, operators, etc)

data Unit a = Unit [TopLevel a] a

data TopLevel a = TopSection Text [Section a] a
                | TopDecl (Decl a)
                | TopProcedure (Procedure a)

data Section a = SecDecl (Decl a)
               | SecProcedure (Procedure a)
               | SecDatum (Datum a)
               | SecSpan (Expr a) (Expr a) [Section a] a

data Decl a = ImportDecl [Import a] a -- at least one
            | ExportDecl [Export a] a -- at least one
            | ConstDecl (Maybe (Type a)) Name (Expr a) a
            | TypedefDecl (Type a) [Name] a -- at least one
            | RegDecl (Maybe Invariant) (Registers a) a
            | PragmaDecl Name (Pragma a) a
            | TargetDecl [TargetDirective a] a

data TargetDirective a = MemSize Int a
                       | ByteOrder Endian a
                       | PointerSize Int a
                       | WordSize Int a

data Endian = Little | Big -- TODO: or just Bool?

data Import a = Import (Maybe Text) Name a

data Export a = Export Name (Maybe Text) a

data Invariant = Invariant -- TODO: or it can be just Bool?

data Datum a = DatumLabel Name a
             | DatumAlign Int a
             | Datum (Type a) (Maybe (Size a)) (Maybe (Init a)) a

data Init a = ExprInit [Expr a] a
            | StrInit Text a
            | Str16Init String16 a

data Registers a = Registers (Maybe Kind) (Type a) [(Name, Maybe Text)] a -- at least one

data Size a = Size (Maybe (Expr a)) a

data Body a = Body [BodyItem a] a

data BodyItem a = BodyDecl (Decl a) a
                | BodyStackDecl (StackDecl a) a
                | BodyStmt (Stmt a) a

data Procedure a = Procedure (Maybe Conv) Name [Formal a] (Body a) a

data Formal a = Formal (Maybe Kind) (Maybe Invariant) (Type a) Name a

data Actual a = Actual (Maybe Kind) (Expr a) a

newtype Kind = Kind Text -- can be IEEE 754, integer, floating point, qubite, oracle byte, etc.
-- TODO: should be a part of inference?

data KindName a = KindName (Maybe Kind) Name a

data StackDecl a = StackDecl [Datum a] a -- at least one

data Stmt a = EmptyStmt a
            | IfStmt (Expr a) (Body a) (Maybe (Body a)) a
            | SwitchStmt (Expr a) [Arm a] a
            | SpanStmt (Expr a) (Expr a) (Body a) a
            | AssignStmt [LValue a] [Expr a] a -- TODO?: maybe list of tuples
            | PrimOpStmt [LValue a] Name [Flow a] a
            | CallStmt [KindName a] (Maybe Conv) (Expr a) [Actual a] (Maybe (Targets a)) [Either (Flow a) (Alias a)] a
            | JumpStmt (Maybe Conv) (Expr a)  [Actual a] (Maybe (Targets a)) a
            | ReturnStmt (Maybe Conv) (Maybe (Expr a, Expr a)) [Actual a] a
            | LabelStmt Name a
            | ContStmt Name [KindName a] a
            | GotoStmt (Expr a) (Maybe (Targets a)) a
            | CutToStmt (Expr a) [Actual a] [Flow a] a

data Arm a = Arm [Range a] (Body a) a

data Range a = Range (Expr a) (Maybe (Expr a)) a

data LValue a = LVName Name a -- TODO?: name
              | LVRef (Type a) (Expr a) (Maybe (Asserts a)) a -- TODO?: name

data Flow a = AlsoCutsTo [Name] a -- at least one
            | AlsoUnwindsTo [Name] a -- at least one
            | AlsoReturnsTo [Name] a -- at least one
            | AlsoAbsorbs a
            | NeverReturns a
-- TODO: should be a part of inference?

data Alias a = Reads [Name] a | Writes [Name] a

data Targets a = Targets [Name] a

data Expr a = LitExpr (Lit a) (Maybe (Type a)) a
            | NameExpr Name a
            | RefExpr (Type a) (Expr a) (Maybe (Asserts a)) a
            | ParExpr (Expr a) a
            | BinOpExpr (Expr a) Op (Expr a) a
            | ComExpr (Expr a) a
            | NegExpr (Expr a) a
            | InfixExpr (Expr a) Name (Expr a) a
            | PrefixExpr Name [Actual a] a

data Lit a = LitInt Int a -- TODO?: think more
           | LitFloat Float a
           | LitChar Char a

data Type a = TBits Int a -- TODO?: name
            | TName (Name) a -- TODO?: name

newtype String16 = String16 Text -- TODO: implement String16

newtype Conv = Foreign Text

data Asserts a = AlignAssert Int [Name] a

data Op = AddOp | SubOp | MulOp | DivOp | ModOp | AndOp | OrOp | NorOp | ShLOp | ShROp | EqOp | NeqOp | GtOp | LtOp | GeOp | LeOp

newtype Name = Name Text -- TODO: consult this with the manual (and mirek) (and also add hash etc?)

data Pragma a -- TODO: the manual doesn't specify at all

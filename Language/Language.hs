module Language where

import qualified Data.Text as T
import Data.Text (Text)

class AST a where
  takePos :: a b -> b

data Unit a = Unit [TopLevel a] a
            deriving (Show)

instance AST Unit where
  takePos (Unit _ pos) = pos

data TopLevel a = TopSection Text [Section a] a
                | TopDecl (Decl a)
                | TopProcedure (Procedure a)
                deriving (Show)

instance AST TopLevel where
  takePos (TopSection _ _ pos) = pos
  takePos (TopDecl decl) = takePos decl
  takePos (TopProcedure procedure) = takePos procedure

data Section a = SecDecl (Decl a)
               | SecProcedure (Procedure a)
               | SecDatum (Datum a)
               | SecSpan (Expr a) (Expr a) [Section a]
               deriving (Show)

instance AST Section where
  takePos (SecDecl decl) = takePos decl
  takePos (SecProcedure procedure) = takePos procedure
  takePos (SecDatum datum) = takePos datum
  takePos (SecSpan left _ _) = takePos left

data Decl a = ImportDecl [Import a] a -- at least one
            | ExportDecl [Export a] a -- at least one
            | ConstDecl (Maybe (Type a)) Name (Expr a)
            | TypedefDecl (Type a) [Name] -- at least one
            | RegDecl (Maybe Invariant) (Registers a)
            | PragmaDecl Name (Pragma a)
            | TargetDecl [TargetDirective a] a
            deriving (Show)

instance AST Decl where
  takePos (ImportDecl _ pos) = pos
  takePos (ExportDecl _ pos) = pos
  takePos (ConstDecl _ _ expr) = takePos expr
  takePos (TypedefDecl t _) = takePos t
  takePos (RegDecl _ registers) = takePos registers
  takePos (PragmaDecl _ pragma) = takePos pragma -- TODO: simplify this
  takePos (TargetDecl _ pos) = pos

data TargetDirective a = MemSize Int a
                       | ByteOrder Endian a
                       | PointerSize Int a
                       | WordSize Int a
                       deriving (Show)

instance AST TargetDirective where
  takePos (MemSize _ pos) = pos
  takePos (ByteOrder _ pos) = pos
  takePos (PointerSize _ pos) = pos
  takePos (WordSize _ pos) = pos

data Endian = Little | Big
            deriving (Show)

data Import a = Import (Maybe Text) Name a
              deriving (Show)

instance AST Import where
  takePos (Import _ _ pos) = pos

data Export a = Export Name (Maybe Text) a
              deriving (Show)

instance AST Export where
  takePos (Export _ _ pos) = pos

data Invariant = Invariant -- TODO: or it can be just Bool?
               deriving (Show)

data Datum a = DatumLabel Name a
             | DatumAlign Int a
             | Datum (Type a) (Maybe (Size a)) (Maybe (Init a))
             deriving (Show)

instance AST Datum where
  takePos (DatumLabel _ pos) = pos
  takePos (DatumAlign _ pos) = pos
  takePos (Datum type_ _ _) = takePos type_

data Init a = ExprInit [Expr a] a
            | StrInit Text a
            | Str16Init String16 a
            deriving (Show)

instance AST Init where
  takePos (ExprInit _ pos) = pos
  takePos (StrInit _ pos) = pos
  takePos (Str16Init _ pos) = pos

data Registers a = Registers (Maybe Kind) (Type a) [(Name, Maybe Text)] -- at least one
                 deriving (Show)

instance AST Registers where
  takePos (Registers _ type_ _) = takePos type_
data Size a = Size (Maybe (Expr a)) a
            deriving (Show)


instance AST Size where
  takePos (Size _ pos) = pos

data Body a = Body [BodyItem a] a
            deriving (Show)


instance AST Body where
  takePos (Body _ pos) = pos
data BodyItem a = BodyDecl (Decl a)
                | BodyStackDecl (StackDecl a)
                | BodyStmt (Stmt a)
                deriving (Show)

instance AST BodyItem where
  takePos (BodyDecl decl) = takePos decl
  takePos (BodyStackDecl stackDecl) = takePos stackDecl
  takePos (BodyStmt stmt) = takePos stmt

data Procedure a = Procedure (Maybe Conv) Name [Formal a] (Body a)
                 deriving (Show)

instance AST Procedure where
  takePos (Procedure _ _ _ body) = takePos body

data Formal a = Formal (Maybe Kind) (Maybe Invariant) (Type a) Name
              deriving (Show)

instance AST Formal where
  takePos (Formal _ _ type_ _) = takePos type_

data Actual a = Actual (Maybe Kind) (Expr a)
              deriving (Show)

instance AST Actual where
  takePos (Actual _ expr) = takePos expr

newtype Kind = Kind Text
             deriving (Show)

data KindName a = KindName (Maybe Kind) Name a
                deriving (Show)

instance AST KindName where
  takePos (KindName _ _ pos) = pos

data StackDecl a = StackDecl [Datum a] a -- at least one
                 deriving (Show)

instance AST StackDecl where
  takePos (StackDecl _ pos) = pos

data Stmt a = EmptyStmt a
            | IfStmt (Expr a) (Body a) (Maybe (Body a))
            | SwitchStmt (Expr a) [Arm a]
            | SpanStmt (Expr a) (Expr a) (Body a)
            | AssignStmt [LValue a] [Expr a] a -- TODO?: maybe list of tuples
            | PrimOpStmt Name Name [Actual a] [Flow a] a
            | CallStmt [KindName a] (Maybe Conv) (Expr a) [Actual a] (Maybe (Targets a)) [Either (Flow a) (Alias a)]
            | JumpStmt (Maybe Conv) (Expr a)  [Actual a] (Maybe (Targets a))
            | ReturnStmt (Maybe Conv) (Maybe (Expr a, Expr a)) [Actual a] a
            | LabelStmt Name a
            | ContStmt Name [KindName a] a
            | GotoStmt (Expr a) (Maybe (Targets a))
            | CutToStmt (Expr a) [Actual a] [Flow a]
            deriving (Show)

instance AST Stmt where
  takePos (EmptyStmt pos) = pos
  takePos (IfStmt expr _ _) = takePos expr
  takePos (SwitchStmt expr _) = takePos expr
  takePos (SpanStmt left _ _) = takePos left
  takePos (AssignStmt _ _ pos) = pos
  takePos (PrimOpStmt _ _ _ _ pos) = pos
  takePos (CallStmt _ _ expr _ _ _) = takePos expr
  takePos (JumpStmt _ expr _ _) = takePos expr
  takePos (ReturnStmt _ _ _ pos) = pos
  takePos (LabelStmt _ pos) = pos
  takePos (ContStmt _ _ pos) = pos
  takePos (GotoStmt expr _) = takePos expr
  takePos (CutToStmt expr _ _) = takePos expr

data Arm a = Arm [Range a] (Body a)
           deriving (Show)

instance AST Arm where
  takePos (Arm _ body) = takePos body

data Range a = Range (Expr a) (Maybe (Expr a))
             deriving (Show)

instance AST Range where
  takePos (Range expr _) = takePos expr

data LValue a = LVName Name a -- TODO?: name
              | LVRef (Type a) (Expr a) (Maybe (Asserts a)) -- TODO?: name
              deriving (Show)

instance AST LValue where
  takePos (LVName _ pos) = pos
  takePos (LVRef _ expr _) = takePos expr

data Flow a = AlsoCutsTo [Name] a -- at least one
            | AlsoUnwindsTo [Name] a -- at least one
            | AlsoReturnsTo [Name] a -- at least one
            | AlsoAborts a
            | NeverReturns a
            deriving (Show)
-- TODO: should be a part of inference?

instance AST Flow where
  takePos (AlsoCutsTo _ pos) = pos
  takePos (AlsoUnwindsTo _ pos) = pos
  takePos (AlsoReturnsTo _ pos) = pos
  takePos (AlsoAborts pos) = pos
  takePos (NeverReturns pos) = pos

data Alias a = Reads [Name] a
             | Writes [Name] a
             deriving (Show)

instance AST Alias where
  takePos (Reads _ pos) = pos
  takePos (Writes _ pos) = pos

data Targets a = Targets [Name] a
               deriving (Show)

instance AST Targets where
  takePos (Targets _ pos) = pos

data Expr a = LitExpr (Lit a) (Maybe (Type a))
            | NameExpr Name a
            | RefExpr (Type a) (Expr a) (Maybe (Asserts a))
            | ParExpr (Expr a)
            | BinOpExpr (Expr a) Op (Expr a)
            | ComExpr (Expr a)
            | NegExpr (Expr a)
            | InfixExpr (Expr a) Name (Expr a)
            | PrefixExpr Name [Actual a] a
            deriving (Show)

instance AST Expr where
  takePos (LitExpr lit _) = takePos lit
  takePos (NameExpr _ pos) = pos
  takePos (RefExpr _ expr _) = takePos expr
  takePos (BinOpExpr left _ _) = takePos left
  takePos (ComExpr expr) = takePos expr
  takePos (NegExpr expr) = takePos expr
  takePos (InfixExpr left _ _) = takePos left
  takePos (PrefixExpr _ _ pos) = pos

data Lit a = LitInt Int a -- TODO?: think more
           | LitFloat Float a
           | LitChar Char a
           deriving (Show)

instance AST Lit where
  takePos (LitInt _ pos) = pos
  takePos (LitFloat _ pos) = pos
  takePos (LitChar _ pos) = pos

data Type a = TBits Int a -- TODO?: name
            | TName Name a -- TODO?: name
            deriving (Show)

instance AST Type where
  takePos (TBits _ pos) = pos
  takePos (TName _ pos) = pos

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

instance AST Pragma where
    takePos = undefined -- TODO

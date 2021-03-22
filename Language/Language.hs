module Language where

-- TODO: categorize names? (different namespaces for variables, operators, etc)

data Unit a = Unit [TopLevel a] a

data TopLevel a = TopSection String [Section a] a
                | TopDecl (Decl a) a
                | TopProcedure (Procedure a) a

data Section a = SecDecl (Decl a) a
               | SecProcedure (Procedure a) a
               | SecDatum (Datum a) a
               | SecSpan (Expr a) (Expr a) [Section a] a

data Decl a = ImportDecl [Import a] a -- at least one
            | ExportDecl [Export a] a -- at least one
            | ConstDecl (Maybe (Type a)) (Name a) (Expr a) a
            | TypedefDecl (Type a) [Name a] a -- at least one
            | RegDecl (Maybe Invariant) (Registers a) a
            | PragmaDecl (Name a) (Pragma a) a
            | TargetDecl [TargetDirective a] a

data TargetDirective a = MemSize Int a
                       | BiteOrder Endian a
                       | PointerSize Int a
                       | WordSize Int a

data Endian = Little | Big -- TODO: or just Bool?

data Import a = Import (Maybe String) (Name a) a

data Export a = Export (Name a) (Maybe String) a

data Invariant = Invariant -- TODO: or it can be just Bool?

data Datum a = DatumLabel (Name a) a
             | DatumAlign Int a
             | Datum (Type a) (Maybe (Size a)) (Maybe (Init a)) a

data Init a = ExprInit [Expr a] a
            | StrInit String a
            | Str16Init String16 a

data Registers a = Registers (Maybe Kind) (Type a) [(Name a, Maybe String)] a -- at least one

data Size a = Size (Maybe (Expr a)) a

data Body a = Body [BodyItem a] a

data BodyItem a = BodyDecl (Decl a) a
                | BodyStackDecl (StackDecl a) a
                | BodyStmt (Stmt a) a

data Procedure a = Procedure (Maybe Conv) (Name a) [Formal a] (Body a) a

data Formal a = Formal (Maybe Kind) (Maybe Invariant) (Type a) (Name a) a

data Actual a = Actual (Maybe Kind) (Expr a) a

newtype Kind = Kind String -- can be IEEE 754, integer, floating point, qubite, oracle byte, etc.

data KindName a = KindName (Maybe Kind) (Name a) a

data StackDecl a = StackDecl [Datum a] a -- at least one

data Stmt a = EmptyStmt a
            | IfStmt (Expr a) (Body a) (Maybe (Body a)) a
            | SwitchStmt (Expr a) [Arm a] a
            | SpanStmt (Expr a) (Expr a) (Body a) a
            | AssignStmt [LValue a] [Expr a] a -- TODO: maybe list of tuples?
            | PrimOpStmt [LValue a] (Name a) [Flow a] a
            | CallStmt [KindName a] (Maybe Conv) (Expr a) [Actual a] (Maybe (Targets a)) [Either (Flow a) (Alias a)] a
            | JumpStmt (Maybe Conv) (Expr a)  [Actual a] (Maybe (Targets a)) a
            | ReturnStmt (Maybe Conv) (Maybe (Expr a, Expr a)) [Actual a] a
            | LabelStmt (Name a) a
            | ContStmt (Name a) [KindName a] a
            | GotoStmt (Expr a) (Maybe (Targets a)) a
            | CutToStmt (Expr a) [Actual a] [Flow a] a

data Arm a = Arm [Range a] (Body a) a

data Range a = Range (Expr a) (Maybe (Expr a)) a

data LValue a = LVName (Name a) a -- TODO: name
              | LVRef (Type a) (Expr a) (Maybe (Asserts a)) a -- TODO: name

data Flow a = AlsoCutsTo [Name a] a -- at least one
            | AlsoUnwindsTo [Name a] a -- at least one
            | AlsoReturnsTo [Name a] a -- at least one
            | AlsoAbsorbs a
            | NeverReturns a

data Alias a = Reads [Name a] a | Writes [Name a] a

data Targets a = Targets [Name a] a

data Expr a = LitExpr (Lit a) (Maybe (Type a)) a
            | NameExpr (Name a) a
            | RefExpr (Type a) (Expr a) (Maybe (Asserts a)) a
            | ParExpr (Expr a) a
            | BinOpExpr (Expr a) Op (Expr a) a
            | ComExpr (Expr a) a
            | NegExpr (Expr a) a
            | InfixExpr (Expr a) (Name a) (Expr a) a
            | PrefixExpr (Name a) [Actual a] a -- TODO: name

data Lit a = LitInt Int a -- TODO: think more
           | LitFloat Float a
           | LitChar Char a

data Type a = TBits Int a -- TODO: name
            | TName (Name a) a -- TODO: name

newtype String16 = String16 String -- TODO: implement String16

newtype Conv = Foreign String

data Asserts a = AlignAssert Int [Name a] a

data Op = AddOp | SubOp | MulOp | DivOp | ModOp | AndOp | OrOp | NorOp | ShLOp | ShROp | EqOp | NeqOp | GtOp | LtOp | GeOp | LeOp

data Name a = Name String a -- TODO: consult this with the manual (and mirek) (and also add hash etc?)

data Pragma a -- TODO: the manual doesn't specify at all

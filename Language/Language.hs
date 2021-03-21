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
            | RegDecl IsInvariant (Registers a) a
            | PragmaDecl (Name a) (Pragma a) a
-- | target {memsize int | byteorder (little | big) | pointersize int | wordsize int};

data Import a = Import (Maybe String) (Name a) a

data Export a = Export (Name a) (Maybe String) a

newtype IsInvariant = Invariant Bool -- TODO: or it can be just Bool?

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

data Formal a = Formal (Maybe Kind) IsInvariant (Type a) (Name a) a

data Actual a = Actual (Maybe Kind) (Expr a) a

newtype Kind = Kind String -- can be IEEE 754, integer, floating point, qubite, oracle byte, etc.

data StackDecl a = StackDecl [Datum a] a -- at least one

data Stmt a = EmptyStmt a
            | IfStmt (Expr a) (Body a) (Maybe (Body a)) a
            | SwitchStmt (Expr a) [Arm a] a
            | SpanStmt (Expr a) (Expr a) (Body a) a
            | AssignStmt [LValue a] [Expr a] a -- TODO: maybe list of tuples?
            | CallStmt [LValue a] (Name a) [Flow a] a
            -- TODO: [kindednames =] [conv] expr ( [actuals] ) [targets] {flow | alias};
            | JumpStmt (Maybe Conv) (Expr a)  [Actual a] (Maybe (Targets a)) a
            -- TODO: | ReturnStmt
            | LabelStmt (Name a) a
            | ContStmt (Name a) [(Kind, Name a)] a
            | GotoStmt (Expr a) (Maybe (Targets a)) a
            | CutToStmt (Expr a) [Actual a] [Flow a] a

data Arm a = Arm [Range a] (Body a) a

data Range a = Range (Expr a) (Maybe (Expr a)) a

data LValue a = LVName (Name a) a -- TODO: name
              | LVRef (Type a) (Expr a) (Maybe (Asserts a)) a -- TODO: name

data Flow a -- TODO: rewrite this

data Alias a = Reads [Name a] a | Writes [Name a] a

data Targets a = Targets [Name a] a

data Expr a = LitExpr (Lit a) (Maybe (Type a)) a
            | LVExpr (LValue a) a
            | ParExpr (Expr a) a -- TODO: do we even need this?
            | BinExpr (Expr a) (Op a) (Expr a) a -- TODO: does op even need a parameter?
            | PrefixExpr (Name a) [Actual a] a -- TODO: name

data Lit a = LitInt Int a -- TODO: think more
           | LitFloat Float a
           | LitChar Char a

data Type a = TBits Int a -- TODO: name
            | TName (Name a) a -- TODO: name

newtype String16 = String16 String -- TODO: implement String16

newtype Conv = Foreign String

data Asserts a = AlignAssert Int [Name a] a
               -- TODO: and the reverse of this?

data Op a = OpName (Name a) | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpAnd | OpOr | OpNeg | OpShL | OpShR | OpEq | OpNeq | OpGT | OpLT | OpGE | OpLE

data Name a = Name String a -- TODO: consult this with the manual (and mirek) (and also add hash etc?)

data Pragma a -- TODO: the manual doesn't specify at all

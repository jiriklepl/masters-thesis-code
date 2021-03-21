module Language where

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
            -- | [invariant] registers;
            | PragmaDecl (Name a) (Pragma a) a
-- | target {memsize int | byteorder (little | big) | pointersize int | wordsize int};

data Import a = Import (Maybe String) (Name a) a

data Export a = Export (Name a) (Maybe String) a

data Datum a = DatumLabel (Name a) a
             | DatumAlign Int a
             | Datum (Type a) (Maybe Size) (Maybe Init) a

data Init a = ExprInit [Expr a] a
            | StrInit String a
            | Str16Init String16 a

data Registers a = Registers (Maybe Kind) (Type a) [(Name a, Maybe String)] a -- at least one

data Size a = Size (Maybe (Expr a)) a

data Body a = Body [BodyItem a] a

data BodyItem a = BodyDecl (Decl a) a
                | BodyStackDecl (StackDecl a) a
                | BodyStmt (Stmt a) a

data Procedure a = Procedure (Maybe (Conv a)) (Name a) [Formal a] (Body a) a

data Formal a = Formal (Maybe Kind) (Maybe (Invariant a)) (Type a) (Name a) a

data Actual a = Actual (Maybe Kind) (Expr a) a

newtype Kind = Kind String

newtype StackDecl a = StackDecl [Datum a] a

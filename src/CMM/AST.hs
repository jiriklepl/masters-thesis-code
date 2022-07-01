{-# LANGUAGE Safe #-}

module CMM.AST where

import safe Data.Data (Data)
import safe Data.Text (Text)

import safe Prettyprinter
  ( Doc
  , Pretty(pretty)
  , (<+>)

  , angles
  , braces
  , brackets
  , colon
  , dquotes
  , equals
  , hsep
  , parens
  , semi
  , slash
  , space
  , squotes
  , vsep
  )

import safe CMM.AST.Annot (Annot, Annotation(Annot))
import safe CMM.Pretty
  ( arrow
  , bquotes
  , bracesBlock
  , commaPretty
  , commaSep
  , darrow
  , dcolon
  , ddot
  , ifTrue
  , inBraces
  , maybeSpacedL
  , maybeSpacedR
  )
import safe CMM.Utils (backQuote, HasCallStack)
import safe qualified CMM.Lexer.Token as T

newtype Unit a =
  Unit [Annot TopLevel a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Unit ())

instance Pretty (Unit a) where
  pretty =
    \case
      Unit topLevels -> vsep $ pretty <$> topLevels

data TopLevel a
  = TopSection StrLit [Annot Section a]
  | TopDecl (Annot Decl a)
  | TopProcedure (Annot Procedure a)
  | TopClass (Annot Class a)
  | TopInstance (Annot Instance a)
  | TopStruct (Annot Struct a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (TopLevel ())

instance Pretty (TopLevel a) where
  pretty =
    \case
      TopSection name items -> T.sectionName <+> pretty name <+> bracesBlock items
      TopDecl decl -> pretty decl
      TopProcedure procedure -> pretty procedure
      TopClass class' -> pretty class'
      TopInstance instance' -> pretty instance'
      TopStruct struct -> pretty struct

data Section a
  = SecDecl (Annot Decl a)
  | SecProcedure (Annot Procedure a)
  | SecDatum (Annot Datum a)
  | SecSpan (Annot Expr a) (Annot Expr a) [Annot Section a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Section ())

instance Pretty (Section a) where
  pretty =
    \case
      SecDecl decl -> pretty decl
      SecProcedure procedure -> pretty procedure
      SecDatum datum -> pretty datum
      SecSpan left right sections ->
        pretty left <+> pretty right <+> bracesBlock sections

data Decl a
  = ImportDecl [Annot Import a] -- at least one
  | ExportDecl [Annot Export a] -- at least one
  | ConstDecl (Maybe (Annot Type a)) (Name a) (Annot Expr a)
  | TypedefDecl (Annot Type a) [Annot Name a] -- at least one
  | RegDecl Bool (Annot Registers a)
  | PragmaDecl (Name a) (Annot Pragma a)
  | TargetDecl [Annot TargetDirective a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Decl ())

instance Pretty (Decl a) where
  pretty =
    \case
      ImportDecl imports -> T.importName <+> commaPretty imports <> semi
      ExportDecl exports -> T.exportName <+> commaPretty exports <> semi
      ConstDecl mType name expr ->
        T.constName <+>
        maybeSpacedR mType <> pretty name <+> equals <+> pretty expr <> semi
      TypedefDecl type' names ->
        T.typedefName <+> pretty type' <+> commaPretty names <> semi
      RegDecl invar registers ->
        ifTrue invar (T.invariantName <> space) <> pretty registers <> semi
      PragmaDecl name pragma ->
        T.pragmaName <+> pretty name <+> braces (pretty pragma)
      TargetDecl targetDirectives ->
        T.targetName <+> hsep (pretty <$> targetDirectives) <> semi

data Class a =
  Class
    [Annot (ParaName Type) a]
    (Annot (ParaName Name) a)
    [Annot ProcedureDecl a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Class ())

instance Pretty (Class a) where
  pretty class' =
    T.className <+>
    case class' of
      Class [] paraName methods -> pretty paraName <+> bracesBlock methods
      Class paraNames paraName methods ->
        commaPretty paraNames <+>
        darrow <+> pretty paraName <+> bracesBlock methods

data Instance a =
  Instance
    [Annot (ParaName Type) a]
    (Annot (ParaName Type) a)
    [Annot Procedure a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Instance ())

instance Pretty (Instance a) where
  pretty instance' =
    T.instanceName <+>
    case instance' of
      Instance [] paraName methods -> pretty paraName <+> bracesBlock methods
      Instance paraNames paraName methods ->
        commaPretty paraNames <+>
        darrow <+> pretty paraName <+> bracesBlock methods

data Struct a =
  Struct (Annot (ParaName Name) a) [Annot Datum a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Struct ())

instance Pretty (Struct a) where
  pretty =
    \case
      Struct paraName datums ->
        T.structName <+> pretty paraName <+> inBraces (prettyDatums datums)

data ParaName param a =
  ParaName (Name a) [Annot param a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (param ()) => Eq (ParaName param ())

instance Pretty (param a) => Pretty (ParaName param a) where
  pretty =
    \case
      ParaName name [] -> pretty name
      ParaName name types -> pretty name <+> hsep (pretty <$> types)

data TargetDirective a
  = MemSize Int
  | ByteOrder Endian
  | PointerSize Int
  | WordSize Int
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (TargetDirective a) where
  pretty =
    \case
      MemSize int -> T.memsizeName <+> pretty int
      ByteOrder endian ->
        T.byteorderName <+>
        (\case
           Little -> T.littleName
           Big -> T.bigName)
          endian
      PointerSize int -> T.pointersizeName <+> pretty int
      WordSize int -> T.wordsizeName <+> pretty int

data Import a =
  Import (Maybe StrLit) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Import a) where
  pretty =
    \case
      Import (Just string) name -> pretty string <+> T.asName <+> pretty name
      Import Nothing name -> pretty name

data Export a =
  Export (Name a) (Maybe StrLit)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Export a) where
  pretty =
    \case
      Export name (Just string) -> pretty name <+> T.asName <+> pretty string
      Export name Nothing -> pretty name

data Endian
  = Little
  | Big
  deriving (Show, Data, Eq)

instance Pretty Endian where
  pretty =
    \case
      Little -> T.littleName
      Big -> T.bigName

data Datum a
  = DatumLabel (Name a)
  | DatumAlign Int
  | Datum Bool (Annot Type a) (Maybe (Annot Size a)) (Maybe (Annot Init a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Datum ())

instance Pretty (Datum a) where
  pretty =
    \case
      DatumLabel name -> pretty name <> colon
      DatumAlign int -> T.alignName <+> pretty int <> semi
      Datum new type' mSize mInit ->
        ifTrue new (T.newName <> space) <>
        pretty type' <>
        maybe mempty pretty mSize <> maybe mempty pretty mInit <> semi

prettyDatums :: [Annotation Datum annot] -> Doc ann
prettyDatums [] = mempty
prettyDatums (datum:others) =
  case datum of
    DatumLabel {} `Annot` _ -> pretty datum <+> prettyDatums others
    _ -> pretty datum <> prettyDatums others

data Init a
  = ExprInit [Annot Expr a]
  | StrInit StrLit
  | Str16Init StrLit
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Init ())

instance Pretty (Init a) where
  pretty =
    \case
      ExprInit exprs -> braces $ commaPretty exprs
      StrInit string -> pretty string
      Str16Init string -> T.unicodeName <> parens (dquotes $ pretty string)

data Registers a =
  Registers (Maybe Kind) (Annot Type a) [(Annot Name a, Maybe StrLit)] -- at least one
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Registers ())

instance Pretty (Registers a) where
  pretty =
    \case
      Registers mKind type' nameStringPairs ->
        maybeSpacedR mKind <> pretty type' <+>
        commaSep
          [ pretty name <>
          maybe mempty ((space <>) . (equals <+>) . pretty) mString
          | (name, mString) <- nameStringPairs
          ]

newtype Size a =
  Size (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Size ())

instance Pretty (Size a) where
  pretty =
    \case
      Size mExpr -> brackets $ maybe mempty pretty mExpr

newtype Body a =
  Body [Annot BodyItem a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Body ())

instance Pretty (Body a) where
  pretty =
    \case
      Body bodyItems -> bracesBlock bodyItems

data BodyItem a
  = BodyDecl (Annot Decl a)
  | BodyStackDecl (Annot StackDecl a)
  | BodyStmt (Annot Stmt a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (BodyItem ())

instance Pretty (BodyItem a) where
  pretty =
    \case
      BodyDecl decl -> pretty decl
      BodyStackDecl stackDecl -> pretty stackDecl
      BodyStmt stmt -> pretty stmt

data Procedure a =
  Procedure (Annot ProcedureHeader a) (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Procedure ())

instance Pretty (Procedure a) where
  pretty =
    \case
      Procedure header body -> pretty header <+> pretty body

newtype ProcedureDecl a =
  ProcedureDecl (Annot ProcedureHeader a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (ProcedureDecl ())

instance Pretty (ProcedureDecl a) where
  pretty =
    \case
      ProcedureDecl header -> pretty header <> semi

data ProcedureHeader a =
  ProcedureHeader
    (Maybe Conv)
    (Name a)
    [Annot Formal a]
    (Maybe [Annot SemiFormal a])
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (ProcedureHeader ())

instance Pretty (ProcedureHeader a) where
  pretty =
    \case
      ProcedureHeader mConv name formals mTypes ->
        maybeSpacedR mConv <>
        pretty name <> parens (commaPretty formals) <> ending
        where ending =
                case mTypes of
                  Nothing -> mempty
                  Just [] -> mempty <+> arrow
                  Just types -> mempty <+> arrow <+> commaPretty types

data Formal a =
  Formal (Maybe Kind) Bool (Annot Type a) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Formal ())

instance Pretty (Formal a) where
  pretty =
    \case
      Formal mKind invar type' name ->
        maybeSpacedR mKind <>
        ifTrue invar (T.invariantName <> space) <> pretty type' <+>
        pretty name

data SemiFormal a =
  SemiFormal (Maybe Kind) (Annot Type a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (SemiFormal ())

instance Pretty (SemiFormal a) where
  pretty =
    \case
      SemiFormal mKind type' -> maybeSpacedR mKind <> pretty type'

data Actual a =
  Actual (Maybe Kind) (Annot Expr a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Actual ())

instance Pretty (Actual a) where
  pretty =
    \case
      Actual mKind expr -> maybeSpacedR mKind <> pretty expr

newtype Kind =
  Kind StrLit
  deriving (Show, Data, Eq)

instance Pretty Kind where
  pretty =
    \case
      Kind string -> pretty string

newtype StackDecl a =
  StackDecl [Annot Datum a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (StackDecl ())

instance Pretty (StackDecl a) where
  pretty =
    \case
      StackDecl datums -> T.stackdataName <+> inBraces (prettyDatums datums)

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
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Stmt ())

instance Pretty (Stmt a) where
  pretty stmt =
    case stmt of
      EmptyStmt -> semi
      IfStmt cond ifBody mElseBody ->
        T.ifName <+>
        pretty cond <+>
        pretty ifBody <>
        maybe mempty ((space <>) . (T.elseName <+>) . pretty) mElseBody
      SwitchStmt expr arms -> T.switchName <+> pretty expr <+> bracesBlock arms
      SpanStmt left right body ->
        T.spanName <+> pretty left <+> pretty right <+> pretty body
      AssignStmt lvalues exprs ->
        commaPretty lvalues <+> equals <+> commaPretty exprs <> semi
      PrimOpStmt left opName actuals flows ->
        pretty left <+>
        equals <+>
        "%%" <>
        pretty opName <>
        parens (commaPretty actuals) <> hsep (pretty <$> flows) <> semi
      CallStmt [] mConv _ _ _ _ -> maybeSpacedR mConv <> prettyCallStmtRest stmt
      CallStmt kindedNames mConv _ _ _ _ ->
        commaPretty kindedNames <+>
        equals <> maybeSpacedL mConv <+> prettyCallStmtRest stmt
      JumpStmt mConv expr actuals mTargets ->
        T.jumpName <+>
        maybeSpacedR mConv <>
        pretty expr <>
        parens (commaPretty actuals) <> maybeSpacedL mTargets <> semi
      ReturnStmt mConv mChoices actuals ->
        maybeSpacedR mConv <> T.returnName <+>
        maybe
          mempty
          (\(left, right) -> angles $ pretty left <> slash <> pretty right)
          mChoices <>
        parens (commaPretty actuals) <> semi
      LabelStmt name -> pretty name <> colon
      ContStmt name kindedNames ->
        T.continuationName <+>
        pretty name <> parens (commaPretty kindedNames) <> colon
      GotoStmt expr mTargets ->
        T.gotoName <+> pretty expr <> maybeSpacedL mTargets <> semi
      CutToStmt expr actuals flows ->
        T.cutName <+>
        T.toName <+>
        pretty expr <>
        parens (commaPretty actuals) <> hsep (pretty <$> flows) <> semi

prettyCallStmtRest :: HasCallStack => Stmt a -> Doc ann
prettyCallStmtRest =
  \case
    CallStmt _ _ expr actuals mTargets flowOrAliases ->
      pretty expr <>
      parens (commaPretty actuals) <>
      maybeSpacedL mTargets <>
      ifTrue (not $ null flowOrAliases) space <>
      hsep (pretty <$> flowOrAliases) <> semi
    _ -> error "`prettyCallStmtRest` is implemented only for call statements"

data KindName a =
  KindName (Maybe Kind) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (KindName a) where
  pretty =
    \case
      KindName mKind name -> maybeSpacedR mKind <> pretty name

data Arm a =
  Arm [Annot Range a] (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Arm ())

instance Pretty (Arm a) where
  pretty =
    \case
      Arm ranges body -> T.caseName <+> commaPretty ranges <> colon <+> pretty body

data Range a =
  Range (Annot Expr a) (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Range ())

instance Pretty (Range a) where
  pretty =
    \case
      Range left Nothing -> pretty left
      Range left (Just right) -> pretty left <+> ddot <+> pretty right

data LValue a
  = LVName (Name a)
  | LVRef (Maybe (Annot Type a)) (Annot Expr a) (Maybe (Annot Asserts a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (LValue ())

instance Pretty (LValue a) where
  pretty =
    \case
      LVName name -> pretty name
      LVRef type' expr mAsserts ->
        pretty type' <>
        brackets
          (pretty expr <>
           maybe mempty (\asserts -> space <> pretty asserts) mAsserts)

data Flow a
  = AlsoCutsTo [Annot Name a] -- at least one
  | AlsoUnwindsTo [Annot Name a] -- at least one
  | AlsoReturnsTo [Annot Name a] -- at least one
  | AlsoAborts
  | NeverReturns
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Flow a) where
  pretty =
    \case
      AlsoCutsTo names -> T.alsoName <+> T.cutsName <+> T.toName <+> commaPretty names
      AlsoUnwindsTo names -> T.alsoName <+> T.unwindsName <+> T.toName <+> commaPretty names
      AlsoReturnsTo names -> T.alsoName <+> T.returnsName <+> T.toName <+> commaPretty names
      AlsoAborts -> T.alsoName <+> T.abortsName
      NeverReturns -> T.neverName <+> T.returnsName

data Alias a
  = Reads [Annot Name a]
  | Writes [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Alias a) where
  pretty =
    \case
      Reads names -> T.readsName <+> commaPretty names
      Writes names -> T.writesName <+> commaPretty names

data CallAnnot a
  = FlowAnnot (Annot Flow a)
  | AliasAnnot (Annot Alias a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (CallAnnot ())

instance Pretty (CallAnnot a) where
  pretty =
    \case
      AliasAnnot alias -> pretty alias
      FlowAnnot flow -> pretty flow

newtype Targets a =
  Targets [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Targets a) where
  pretty =
    \case
      Targets names -> T.targetsName <+> commaPretty names

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
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Expr ())

instance Pretty (Expr a) where
  pretty =
    \case
      LitExpr lit mType ->
        pretty lit <> maybe mempty ((space <>) . (dcolon <+>) . pretty) mType
      LVExpr lvalue -> pretty lvalue
      ParExpr expr -> parens $ pretty expr
      BinOpExpr op left right -> pretty left <+> pretty op <+> pretty right
      ComExpr expr -> "~" <> pretty expr
      NegExpr expr -> "-" <> pretty expr
      InfixExpr name left right ->
        pretty left <+> bquotes (pretty name) <+> pretty right
      PrefixExpr name actuals ->
        "%" <> pretty name <> parens (commaPretty actuals)
      MemberExpr expr field -> pretty expr <> arrow <> pretty field

data Lit a
  = LitInt Int
  | LitFloat Float
  | LitChar Char
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Lit a) where
  pretty =
    \case
      LitInt int -> pretty int
      LitFloat float -> pretty float
      LitChar char -> squotes $ pretty char

data Type a
  = TBits Int
  | TName (Name a)
  | TAuto (Maybe (Name a))
  | TPtr (Annot Type a)
  | TPar (Annot ParaType a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Type a) where
  pretty =
    \case
      TBits int -> T.bitsName <> pretty int
      TName name -> pretty name
      TAuto mName -> T.autoName <> maybe mempty (parens . pretty) mName
      TPtr t -> T.ptrName <> parens (pretty t)
      TPar paraType -> parens $ pretty paraType

data ParaType a =
  ParaType (Annot Type a) [Annot Type a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (ParaType a) where
  pretty =
    \case
      ParaType type' types -> hsep $ pretty <$> (type' : types)

newtype Conv =
  Foreign StrLit
  deriving (Show, Data, Eq)

instance Pretty Conv where
  pretty =
    \case
      Foreign string -> T.foreignName <+> pretty string

data Asserts a
  = AlignAssert Int [Annot Name a]
  | InAssert [Annot Name a] (Maybe Int) -- at least one (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Asserts a) where
  pretty =
    \case
      AlignAssert int [] -> T.alignedName <+> pretty int
      AlignAssert int names ->
        T.alignedName <+> pretty int <+> T.inName <+> commaPretty names
      InAssert names mInt -> T.inName <+> commaPretty names <> maybeSpacedL mInt

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

instance Pretty Op where
  pretty =
    \case
      AddOp -> "+"
      SubOp -> "-"
      MulOp -> "*"
      DivOp -> "/"
      ModOp -> "%"
      AndOp -> "&"
      OrOp -> "|"
      XorOp -> "^"
      ShLOp -> "<<"
      ShROp -> ">>"
      EqOp -> "=="
      NeqOp -> "!="
      GtOp -> ">"
      LtOp -> "<"
      GeOp -> ">="
      LeOp -> "<="

newtype Name a =
  Name Text
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Name a) where
  pretty =
    \case
      Name name -> pretty name

newtype StrLit =
  StrLit Text
  deriving (Show, Data, Eq)

instance Pretty StrLit where
  pretty =
    \case
      StrLit string -> pretty $ show string

data Pragma a
  deriving (Functor, Foldable, Traversable, Data) -- FIXME: the manual does not specify at all

instance Eq (Pragma a) where
  (==) = error "pragmas are not implemented"

instance Show (Pragma a) where
  show = error "pragmas are not implemented"

instance Pretty (Pragma a) where
  pretty _ = error $ backQuote T.pragmaName <> "s are not specified" -- FIXME: pragmas are not specified

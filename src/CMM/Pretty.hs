{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Pretty
  (
  ) where

import safe Data.Bool (Bool, not)
import safe Data.Foldable (Foldable(null))
import safe Data.Function (($), (.))
import safe Data.Functor (Functor(fmap), (<$>))
import safe Data.Maybe (Maybe(Just, Nothing), maybe)
import safe Data.Monoid (Monoid(mempty), (<>))
import safe GHC.Err (error)
import safe Text.Show (Show(show))

import safe Prettyprinter
  ( Doc
  , Pretty(pretty)
  , (<+>)
  , angles
  , braces
  , brackets
  , colon
  , comma
  , dquotes
  , enclose
  , equals
  , hsep
  , indent
  , parens
  , punctuate
  , semi
  , slash
  , space
  , squotes
  , vsep
  )

import safe CMM.AST
  ( Actual(Actual)
  , Alias(Reads, Writes)
  , Arm(Arm)
  , Asserts(AlignAssert, InAssert)
  , Body(Body)
  , BodyItem(BodyDecl, BodyStackDecl, BodyStmt)
  , CallAnnot(AliasAnnot, FlowAnnot)
  , Class(Class)
  , Conv(Foreign)
  , Datum(Datum, DatumAlign, DatumLabel)
  , Decl(ConstDecl, ExportDecl, ImportDecl, PragmaDecl, RegDecl,
     TargetDecl, TypedefDecl)
  , Endian(Big, Little)
  , Export(Export)
  , Expr(BinOpExpr, ComExpr, InfixExpr, LVExpr, LitExpr, MemberExpr,
     NegExpr, ParExpr, PrefixExpr)
  , Flow(AlsoAborts, AlsoCutsTo, AlsoReturnsTo, AlsoUnwindsTo,
     NeverReturns)
  , Formal(Formal)
  , Import(Import)
  , Init(ExprInit, Str16Init, StrInit)
  , Instance(Instance)
  , Kind(Kind)
  , KindName(KindName)
  , LValue(LVName, LVRef)
  , Lit(LitChar, LitFloat, LitInt)
  , Name(Name)
  , Op(AddOp, AndOp, DivOp, EqOp, GeOp, GtOp, LeOp, LtOp, ModOp, MulOp,
   NeqOp, OrOp, ShLOp, ShROp, SubOp, XorOp)
  , ParaName(ParaName)
  , ParaType(ParaType)
  , Pragma
  , Procedure(Procedure)
  , ProcedureDecl(ProcedureDecl)
  , ProcedureHeader(ProcedureHeader)
  , Range(Range)
  , Registers(Registers)
  , Section(SecDatum, SecDecl, SecProcedure, SecSpan)
  , Size(Size)
  , StackDecl(StackDecl)
  , Stmt(AssignStmt, CallStmt, ContStmt, CutToStmt, EmptyStmt,
     GotoStmt, IfStmt, JumpStmt, LabelStmt, PrimOpStmt, ReturnStmt,
     SpanStmt, SwitchStmt)
  , StrLit(StrLit)
  , Struct(Struct)
  , TargetDirective(ByteOrder, MemSize, PointerSize, WordSize)
  , Targets(Targets)
  , TopLevel(TopClass, TopDecl, TopInstance, TopProcedure, TopSection,
         TopStruct)
  , Type(TAuto, TBits, TName, TPar)
  , Unit(Unit)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot))

commaSep :: [Doc ann] -> Doc ann
commaSep xs = hsep $ punctuate comma xs

commaPretty :: Pretty a => [a] -> Doc ann
commaPretty = commaSep . fmap pretty

bracesBlock :: Pretty a => [a] -> Doc ann
bracesBlock xs = braces . enclose "\n" "\n" . indent 2 . vsep $ pretty <$> xs

maybeSpacedL :: Pretty a => Maybe a -> Doc ann
maybeSpacedL = maybe mempty ((space <>) . pretty)

maybeSpacedR :: Pretty a => Maybe a -> Doc ann
maybeSpacedR = maybe mempty ((<> space) . pretty)

darrow :: Doc ann
darrow = "=>"

ifTrue :: Monoid a => Bool -> a -> a
ifTrue bool x =
  if bool
    then x
    else mempty

instance (Pretty (n a)) => Pretty (Annot n a) where
  pretty = \case
    Annot n _ -> pretty n

instance Pretty (Unit a) where
  pretty = \case
    Unit topLevels -> vsep $ pretty <$> topLevels

instance Pretty (TopLevel a) where
  pretty = \case
    TopSection name items ->
      "section" <+> pretty name <+> bracesBlock items
    TopDecl decl -> pretty decl
    TopProcedure procedure -> pretty procedure
    TopClass class' -> pretty class'
    TopInstance instance' -> pretty instance'
    TopStruct struct -> pretty struct

instance Pretty (Section a) where
  pretty = \case
    SecDecl decl -> pretty decl
    SecProcedure procedure -> pretty procedure
    SecDatum datum -> pretty datum
    SecSpan left right sections ->
      pretty left <+> pretty right <+> bracesBlock sections

instance Pretty (Decl a) where
  pretty = \case
    ImportDecl imports -> "import" <+> commaPretty imports <> semi
    ExportDecl exports -> "export" <+> commaPretty exports <> semi
    ConstDecl mType name expr ->
      "const" <+>
      maybeSpacedR mType <> pretty name <+> equals <+> pretty expr <> semi
    TypedefDecl type_ names ->
      "typedef" <+> pretty type_ <+> commaPretty names <> semi
    RegDecl invar registers ->
      ifTrue invar ("invariant" <> space) <> pretty registers <> semi
    PragmaDecl name pragma ->
      "pragma" <+> pretty name <+> braces (pretty pragma)
    TargetDecl targetDirectives ->
      "target" <+> hsep (pretty <$> targetDirectives) <> semi

instance Pretty (Class a) where
  pretty = \case
    Class [] paraName methods ->
      "class" <+> pretty paraName <+> bracesBlock methods
    Class paraNames paraName methods ->
      "class" <+>
      commaPretty paraNames <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Instance a) where
  pretty = \case
    Instance [] paraName methods ->
      "instance" <+> pretty paraName <+> bracesBlock methods
    Instance paraNames paraName methods ->
      "instance" <+>
      commaPretty paraNames <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Struct a) where
  pretty (Struct paraName datums) =
    "struct" <+> pretty paraName <+> bracesBlock datums

instance Pretty (param a) => Pretty (ParaName param a) where
  pretty = \case
    ParaName name [] -> pretty name
    ParaName name types -> pretty name <+> hsep (pretty <$> types)

instance Pretty (TargetDirective a) where
  pretty = \case
    MemSize int -> "memsize" <+> pretty int
    ByteOrder endian ->
      "byteorder" <+>
      (\case
        Little -> "little"
        Big -> "big")
        endian
    PointerSize int -> "pointersize" <+> pretty int
    WordSize int -> "wordsize" <+> pretty int

instance Pretty (Import a) where
  pretty = \case
    Import (Just string) name -> pretty string <+> "as" <+> pretty name
    Import Nothing name -> pretty name

instance Pretty (Export a) where
  pretty = \case
    Export name (Just string) -> pretty name <+> "as" <+> pretty string
    Export name Nothing -> pretty name

instance Pretty Endian where
  pretty = \case
    Little -> "little"
    Big -> "big"

instance Pretty (Datum a) where
  pretty = \case
    DatumLabel name -> pretty name <> colon
    DatumAlign int -> "align" <+> pretty int <> semi
    Datum type_ mSize mInit ->
      pretty type_ <>
      maybe mempty pretty mSize <> maybe mempty pretty mInit <> semi

instance Pretty (Init a) where
  pretty = \case
    ExprInit exprs -> braces $ commaPretty exprs
    StrInit string -> pretty string
    Str16Init string -> "unicode" <> parens (dquotes $ pretty string)

instance Pretty (Registers a) where
  pretty (Registers mKind type_ nameStringPairs) =
    maybeSpacedR mKind <> pretty type_ <+>
    commaSep
      [ pretty name <> maybe mempty ((space <>) . (equals <+>) . pretty) mString
      | (name, mString) <- nameStringPairs
      ]

instance Pretty (Size a) where
  pretty (Size mExpr) = brackets $ maybe mempty pretty mExpr

instance Pretty (Body a) where
  pretty (Body bodyItems) = bracesBlock bodyItems

instance Pretty (BodyItem a) where
  pretty = \case
    BodyDecl decl -> pretty decl
    BodyStackDecl stackDecl -> pretty stackDecl
    BodyStmt stmt -> pretty stmt

instance Pretty (Procedure a) where
  pretty (Procedure header body) = pretty header <+> pretty body

instance Pretty (ProcedureDecl a) where
  pretty (ProcedureDecl header) = pretty header <> semi

instance Pretty (ProcedureHeader a) where
  pretty (ProcedureHeader mConv name formals mTypes) =
    maybeSpacedR mConv <> pretty name <> parens (commaPretty formals) <> ending
    where
      ending =
        case mTypes of
          Nothing -> mempty
          Just [] -> mempty <+> "->"
          Just types -> mempty <+> "->" <+> commaPretty types

instance Pretty (Formal a) where
  pretty (Formal mKind invar type_ name) =
    maybeSpacedR mKind <> ifTrue invar ("invariant" <> space) <> pretty type_ <+>
    pretty name

instance Pretty (Actual a) where
  pretty (Actual mKind expr) = maybeSpacedR mKind <> pretty expr

instance Pretty Kind where
  pretty (Kind string) = pretty string

instance Pretty (StackDecl a) where
  pretty (StackDecl datums) = "stackdata" <+> bracesBlock datums

instance Pretty (Stmt a) where
  pretty stmt = case stmt of
    EmptyStmt -> semi
    IfStmt cond ifBody mElseBody ->
      "if" <+>
      pretty cond <+>
      pretty ifBody <> maybe mempty ((space <>) . ("else" <+>) . pretty) mElseBody
    SwitchStmt expr arms -> "switch" <+> pretty expr <+> bracesBlock arms
    SpanStmt left right body ->
      "span" <+> pretty left <+> pretty right <+> pretty body
    AssignStmt lvalues exprs ->
      commaPretty lvalues <+> equals <+> commaPretty exprs <> semi
    PrimOpStmt left opName actuals flows ->
      pretty left <+>
      equals <+>
      "%%" <>
      pretty opName <>
      parens (commaPretty actuals) <> hsep (pretty <$> flows) <> semi
    CallStmt [] mConv _ _ _ _ ->
      maybeSpacedR mConv <> prettyCallStmtRest stmt
    CallStmt kindedNames mConv _ _ _ _ ->
      commaPretty kindedNames <+>
      equals <> maybeSpacedL mConv <+> prettyCallStmtRest stmt
    JumpStmt mConv expr actuals mTargets ->
      "jump" <+>
      maybeSpacedR mConv <>
      pretty expr <> parens (commaPretty actuals) <> maybeSpacedL mTargets <> semi
    ReturnStmt mConv mChoices actuals ->
      maybeSpacedR mConv <> "return" <+>
      maybe
        mempty
        (\(left, right) -> angles $ pretty left <> slash <> pretty right)
        mChoices <>
      parens (commaPretty actuals) <> semi
    LabelStmt name -> pretty name <> colon
    ContStmt name kindedNames ->
      "continuation" <+> pretty name <> parens (commaPretty kindedNames) <> colon
    GotoStmt expr mTargets ->
      "goto" <+> pretty expr <> maybeSpacedL mTargets <> semi
    CutToStmt expr actuals flows ->
      "cut" <+>
      "to" <+>
      pretty expr <>
      parens (commaPretty actuals) <> hsep (pretty <$> flows) <> semi

instance Pretty (KindName a) where
  pretty (KindName mKind name) = maybeSpacedR mKind <> pretty name

instance Pretty (Arm a) where
  pretty (Arm ranges body) =
    "case" <+> commaPretty ranges <> colon <+> pretty body

instance Pretty (Range a) where
  pretty = \case
    Range left Nothing -> pretty left
    Range left (Just right) -> pretty left <+> ".." <+> pretty right

instance Pretty (LValue a) where
  pretty = \case
    LVName name -> pretty name
    LVRef type_ expr mAsserts ->
      pretty type_ <>
      brackets
        (pretty expr <>
        maybe mempty (\asserts -> space <> pretty asserts) mAsserts)

instance Pretty (Flow a) where
  pretty = \case
    AlsoCutsTo names -> "also" <+> "cuts" <+> "to" <+> commaPretty names
    AlsoUnwindsTo names ->
      "also" <+> "unwinds" <+> "to" <+> commaPretty names
    AlsoReturnsTo names ->
      "also" <+> "returns" <+> "to" <+> commaPretty names
    AlsoAborts -> "also" <+> "aborts"
    NeverReturns -> "never" <+> "returns"

instance Pretty (Alias a) where
  pretty = \case
    Reads names -> "reads" <+> commaPretty names
    Writes names -> "writes" <+> commaPretty names

instance Pretty (CallAnnot a) where
  pretty = \case
    AliasAnnot alias -> pretty alias
    FlowAnnot flow -> pretty flow

instance Pretty (Targets a) where
  pretty = \case
    Targets names -> "targets" <+> commaPretty names

instance Pretty (Expr a) where
  pretty = \case
    LitExpr lit mType ->
      pretty lit <> maybe mempty ((space <>) . ("::" <+>) . pretty) mType
    LVExpr lvalue -> pretty lvalue
    ParExpr expr -> parens $ pretty expr
    BinOpExpr op left right ->
      pretty left <+> prettyOp op <+> pretty right
      where
        prettyOp AddOp = "+"
        prettyOp SubOp = "-"
        prettyOp MulOp = "*"
        prettyOp DivOp = "/"
        prettyOp ModOp = "%"
        prettyOp AndOp = "&"
        prettyOp OrOp = "|"
        prettyOp XorOp = "^"
        prettyOp ShLOp = "<<"
        prettyOp ShROp = ">>"
        prettyOp EqOp = "=="
        prettyOp NeqOp = "!="
        prettyOp GtOp = ">"
        prettyOp LtOp = "<"
        prettyOp GeOp = ">="
        prettyOp LeOp = "<="
    ComExpr expr -> "~" <> pretty expr
    NegExpr expr -> "-" <> pretty expr
    InfixExpr name left right ->
      pretty left <+> "`" <> pretty name <> "`" <+> pretty right
    PrefixExpr name actuals ->
      "%" <> pretty name <> parens (commaPretty actuals)
    MemberExpr expr field -> pretty expr <> "->" <> pretty field

instance Pretty (Lit a) where
  pretty = \case
    LitInt int -> pretty int
    LitFloat float -> pretty float
    LitChar char -> squotes $ pretty char

instance Pretty (Type a) where
  pretty = \case
    TBits int -> "bits" <> pretty int
    TName name -> pretty name
    TAuto mName -> "auto" <> maybe mempty (parens . pretty) mName
    TPar paraType -> parens $ pretty paraType

instance Pretty (ParaType a) where
  pretty = \case
    ParaType type' types -> hsep $ pretty <$> (type' : types)

instance Pretty Conv where
  pretty = \case
    Foreign string -> "foreign" <+> pretty string

instance Pretty (Asserts a) where
  pretty = \case
    AlignAssert int [] -> "aligned" <+> pretty int
    AlignAssert int names ->
      "aligned" <+> pretty int <+> "in" <+> commaPretty names
    InAssert names mInt -> "in" <+> commaPretty names <> maybeSpacedL mInt

instance Pretty (Pragma a) where
  pretty _ = error "`Pragma`s are not specified" -- FIXME: pragmas are not specified

instance Pretty (Name a) where
  pretty = \case
    Name name -> pretty name

instance Pretty StrLit where
  pretty = \case
    StrLit string -> pretty $ show string

prettyCallStmtRest :: Stmt a -> Doc ann
prettyCallStmtRest = \case
  CallStmt _ _ expr actuals mTargets flowOrAliases ->
    pretty expr <>
    parens (commaPretty actuals) <>
    maybeSpacedL mTargets <>
    ifTrue (not $ null flowOrAliases) space <>
    hsep (pretty <$> flowOrAliases) <> semi
  _ ->
    error "`prettyCallStmtRest` is implemented only for call statements"

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
  pretty (Annot n _) = pretty n

instance Pretty (Unit a) where
  pretty (Unit topLevels) = vsep $ pretty <$> topLevels

instance Pretty (TopLevel a) where
  pretty (TopSection name items) =
    "section" <+> pretty name <+> bracesBlock items
  pretty (TopDecl decl) = pretty decl
  pretty (TopProcedure procedure) = pretty procedure
  pretty (TopClass class') = pretty class'
  pretty (TopInstance instance') = pretty instance'
  pretty (TopStruct struct) = pretty struct

instance Pretty (Section a) where
  pretty (SecDecl decl) = pretty decl
  pretty (SecProcedure procedure) = pretty procedure
  pretty (SecDatum datum) = pretty datum
  pretty (SecSpan left right sections) =
    pretty left <+> pretty right <+> bracesBlock sections

instance Pretty (Decl a) where
  pretty (ImportDecl imports) = "import" <+> commaPretty imports <> semi
  pretty (ExportDecl exports) = "export" <+> commaPretty exports <> semi
  pretty (ConstDecl mType name expr) =
    "const" <+>
    maybeSpacedR mType <> pretty name <+> equals <+> pretty expr <> semi
  pretty (TypedefDecl type_ names) =
    "typedef" <+> pretty type_ <+> commaPretty names <> semi
  pretty (RegDecl invar registers) =
    ifTrue invar ("invariant" <> space) <> pretty registers <> semi
  pretty (PragmaDecl name pragma) =
    "pragma" <+> pretty name <+> braces (pretty pragma)
  pretty (TargetDecl targetDirectives) =
    "target" <+> hsep (pretty <$> targetDirectives) <> semi

instance Pretty (Class a) where
  pretty (Class [] paraName methods) =
    "class" <+> pretty paraName <+> bracesBlock methods
  pretty (Class paraNames paraName methods) =
    "class" <+>
    commaPretty paraNames <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Instance a) where
  pretty (Instance [] paraName methods) =
    "instance" <+> pretty paraName <+> bracesBlock methods
  pretty (Instance paraNames paraName methods) =
    "instance" <+>
    commaPretty paraNames <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Struct a) where
  pretty (Struct paraName datums) =
    "struct" <+> pretty paraName <+> bracesBlock datums

instance Pretty (param a) => Pretty (ParaName param a) where
  pretty (ParaName name []) = pretty name
  pretty (ParaName name types) = pretty name <+> hsep (pretty <$> types)

instance Pretty (TargetDirective a) where
  pretty (MemSize int) = "memsize" <+> pretty int
  pretty (ByteOrder endian) =
    "byteorder" <+>
    (\case
       Little -> "little"
       Big -> "big")
      endian
  pretty (PointerSize int) = "pointersize" <+> pretty int
  pretty (WordSize int) = "wordsize" <+> pretty int

instance Pretty (Import a) where
  pretty (Import (Just string) name) = pretty string <+> "as" <+> pretty name
  pretty (Import Nothing name) = pretty name

instance Pretty (Export a) where
  pretty (Export name (Just string)) = pretty name <+> "as" <+> pretty string
  pretty (Export name Nothing) = pretty name

instance Pretty Endian where
  pretty Little = "little"
  pretty Big = "big"

instance Pretty (Datum a) where
  pretty (DatumLabel name) = pretty name <> colon
  pretty (DatumAlign int) = "align" <+> pretty int <> semi
  pretty (Datum type_ mSize mInit) =
    pretty type_ <>
    maybe mempty pretty mSize <> maybe mempty pretty mInit <> semi

instance Pretty (Init a) where
  pretty (ExprInit exprs) = braces $ commaPretty exprs
  pretty (StrInit string) = pretty string
  pretty (Str16Init string) = "unicode" <> parens (dquotes $ pretty string)

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
  pretty (BodyDecl decl) = pretty decl
  pretty (BodyStackDecl stackDecl) = pretty stackDecl
  pretty (BodyStmt stmt) = pretty stmt

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
  pretty EmptyStmt = semi
  pretty (IfStmt cond ifBody mElseBody) =
    "if" <+>
    pretty cond <+>
    pretty ifBody <> maybe mempty ((space <>) . ("else" <+>) . pretty) mElseBody
  pretty (SwitchStmt expr arms) = "switch" <+> pretty expr <+> bracesBlock arms
  pretty (SpanStmt left right body) =
    "span" <+> pretty left <+> pretty right <+> pretty body
  pretty (AssignStmt lvalues exprs) =
    commaPretty lvalues <+> equals <+> commaPretty exprs <> semi
  pretty (PrimOpStmt left opName actuals flows) =
    pretty left <+>
    equals <+>
    "%%" <>
    pretty opName <>
    parens (commaPretty actuals) <> hsep (pretty <$> flows) <> semi
  pretty callStmt@(CallStmt [] mConv _ _ _ _) =
    maybeSpacedR mConv <> prettyCallStmtRest callStmt
  pretty callStmt@(CallStmt kindedNames mConv _ _ _ _) =
    commaPretty kindedNames <+>
    equals <> maybeSpacedL mConv <+> prettyCallStmtRest callStmt
  pretty (JumpStmt mConv expr actuals mTargets) =
    "jump" <+>
    maybeSpacedR mConv <>
    pretty expr <> parens (commaPretty actuals) <> maybeSpacedL mTargets <> semi
  pretty (ReturnStmt mConv mChoices actuals) =
    maybeSpacedR mConv <> "return" <+>
    maybe
      mempty
      (\(left, right) -> angles $ pretty left <> slash <> pretty right)
      mChoices <>
    parens (commaPretty actuals) <> semi
  pretty (LabelStmt name) = pretty name <> colon
  pretty (ContStmt name kindedNames) =
    "continuation" <+> pretty name <> parens (commaPretty kindedNames) <> colon
  pretty (GotoStmt expr mTargets) =
    "goto" <+> pretty expr <> maybeSpacedL mTargets <> semi
  pretty (CutToStmt expr actuals flows) =
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
  pretty (Range left Nothing) = pretty left
  pretty (Range left (Just right)) = pretty left <+> ".." <+> pretty right

instance Pretty (LValue a) where
  pretty (LVName name) = pretty name
  pretty (LVRef type_ expr mAsserts) =
    pretty type_ <>
    brackets
      (pretty expr <>
       maybe mempty (\asserts -> space <> pretty asserts) mAsserts)

instance Pretty (Flow a) where
  pretty (AlsoCutsTo names) = "also" <+> "cuts" <+> "to" <+> commaPretty names
  pretty (AlsoUnwindsTo names) =
    "also" <+> "unwinds" <+> "to" <+> commaPretty names
  pretty (AlsoReturnsTo names) =
    "also" <+> "returns" <+> "to" <+> commaPretty names
  pretty AlsoAborts = "also" <+> "aborts"
  pretty NeverReturns = "never" <+> "returns"

instance Pretty (Alias a) where
  pretty (Reads names) = "reads" <+> commaPretty names
  pretty (Writes names) = "writes" <+> commaPretty names

instance Pretty (CallAnnot a) where
  pretty (AliasAnnot alias) = pretty alias
  pretty (FlowAnnot flow) = pretty flow

instance Pretty (Targets a) where
  pretty (Targets names) = "targets" <+> commaPretty names

instance Pretty (Expr a) where
  pretty (LitExpr lit mType) =
    pretty lit <> maybe mempty ((space <>) . ("::" <+>) . pretty) mType
  pretty (LVExpr lvalue) = pretty lvalue
  pretty (ParExpr expr) = parens $ pretty expr
  pretty (BinOpExpr op left right) =
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
  pretty (ComExpr expr) = "~" <> pretty expr
  pretty (NegExpr expr) = "-" <> pretty expr
  pretty (InfixExpr name left right) =
    pretty left <+> "`" <> pretty name <> "`" <+> pretty right
  pretty (PrefixExpr name actuals) =
    "%" <> pretty name <> parens (commaPretty actuals)
  pretty (MemberExpr expr field) = pretty expr <> "->" <> pretty field

instance Pretty (Lit a) where
  pretty (LitInt int) = pretty int
  pretty (LitFloat float) = pretty float
  pretty (LitChar char) = squotes $ pretty char

instance Pretty (Type a) where
  pretty (TBits int) = "bits" <> pretty int
  pretty (TName name) = pretty name
  pretty (TAuto mName) = "auto" <> maybe mempty (parens . pretty) mName
  pretty (TPar paraType) = parens $ pretty paraType

instance Pretty (ParaType a) where
  pretty (ParaType type' types) = hsep $ pretty <$> (type' : types)

instance Pretty Conv where
  pretty (Foreign string) = "foreign" <+> pretty string

instance Pretty (Asserts a) where
  pretty (AlignAssert int []) = "aligned" <+> pretty int
  pretty (AlignAssert int names) =
    "aligned" <+> pretty int <+> "in" <+> commaPretty names
  pretty (InAssert names mInt) = "in" <+> commaPretty names <> maybeSpacedL mInt

instance Pretty (Pragma a) where
  pretty _ = error "`Pragma`s are not specified" -- FIXME: pragmas are not specified

instance Pretty (Name a) where
  pretty (Name name) = pretty name

instance Pretty StrLit where
  pretty (StrLit string) = pretty $ show string

prettyCallStmtRest :: Stmt a -> Doc ann
prettyCallStmtRest (CallStmt _ _ expr actuals mTargets flowOrAliases) =
  pretty expr <>
  parens (commaPretty actuals) <>
  maybeSpacedL mTargets <>
  ifTrue (not $ null flowOrAliases) space <>
  hsep (pretty <$> flowOrAliases) <> semi
prettyCallStmtRest _ =
  error "`prettyCallStmtRest` is implemented only for call statements"

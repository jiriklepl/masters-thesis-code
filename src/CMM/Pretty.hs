{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Pretty
  (
  ) where

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
  ( Actual(..)
  , Alias(..)
  , Arm(..)
  , Asserts(..)
  , Body(..)
  , BodyItem(..)
  , CallAnnot(..)
  , Conv(..)
  , Datum(..)
  , Decl(..)
  , Endian(..)
  , Export(..)
  , Expr(..)
  , Flow(..)
  , Formal(..)
  , Import(..)
  , Init(..)
  , Kind(..)
  , KindName(..)
  , LValue(..)
  , Lit(..)
  , Name(..)
  , Op(..)
  , Pragma
  , Procedure(..)
  , Range(..)
  , Registers(..)
  , Section(..)
  , Size(..)
  , StackDecl(..)
  , Stmt(..)
  , StrLit(..)
  , TargetDirective(..)
  , Targets(..)
  , TopLevel(..)
  , Type(..)
  , Unit(..), Class (Class), Instance (Instance), Struct (Struct), ParaName (ParaName), ParaType (ParaType), ProcedureHeader (ProcedureHeader), ProcedureDecl (ProcedureDecl)
  )
import safe CMM.AST.Annot (Annot, Annotation(Annot))

commaSep :: [Doc ann] -> Doc ann
commaSep xs = hsep $ punctuate comma xs

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
  pretty (ImportDecl imports) =
    "import" <+> commaSep (pretty <$> imports) <> semi
  pretty (ExportDecl exports) =
    "export" <+> commaSep (pretty <$> exports) <> semi
  pretty (ConstDecl mType name expr) =
    "const" <+>
    maybeSpacedR mType <> pretty name <+> equals <+> pretty expr <> semi
  pretty (TypedefDecl type_ names) =
    "typedef" <+> pretty type_ <+> commaSep (pretty <$> names) <> semi
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
    "class" <+> commaSep (pretty <$> paraNames) <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Instance a) where
  pretty (Instance [] paraName methods) =
    "instance" <+> pretty paraName <+> bracesBlock methods
  pretty (Instance paraNames paraName methods) =
    "instance" <+> commaSep (pretty <$> paraNames) <+> darrow <+> pretty paraName <+> bracesBlock methods

instance Pretty (Struct a) where
  pretty (Struct paraName datums) =
    "struct" <+> pretty paraName <+> bracesBlock datums

instance Pretty (param a) => Pretty (ParaName param a) where
  pretty (ParaName name []) =
    pretty name
  pretty (ParaName name types) =
    pretty name <+> hsep (pretty <$> types)

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
  pretty (ExprInit exprs) = braces $ commaSep (pretty <$> exprs)
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
  pretty (Procedure header body) =
    pretty header <+> pretty body

instance Pretty (ProcedureDecl a) where
  pretty (ProcedureDecl header) =
    pretty header <> semi

instance Pretty (ProcedureHeader a) where
  pretty (ProcedureHeader mConv name formals Nothing) =
    maybeSpacedR mConv <> pretty name <> parens (commaSep $ pretty <$> formals)
  pretty (ProcedureHeader mConv name formals (Just type')) =
    maybeSpacedR mConv <> pretty name <> parens (commaSep $ pretty <$> formals) <+> pretty type'

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
    commaSep (pretty <$> lvalues) <+>
    equals <+> commaSep (pretty <$> exprs) <> semi
  pretty (PrimOpStmt left opName actuals flows) =
    pretty left <+>
    equals <+>
    "%%" <>
    pretty opName <>
    parens (commaSep $ pretty <$> actuals) <> hsep (pretty <$> flows) <> semi
  pretty callStmt@(CallStmt [] mConv _ _ _ _) =
    maybeSpacedR mConv <> prettyCallStmtRest callStmt
  pretty callStmt@(CallStmt kindedNames mConv _ _ _ _) =
    commaSep (pretty <$> kindedNames) <+>
    equals <> maybeSpacedL mConv <+> prettyCallStmtRest callStmt
  pretty (JumpStmt mConv expr actuals mTargets) =
    "jump" <+>
    maybeSpacedR mConv <>
    pretty expr <>
    parens (commaSep $ pretty <$> actuals) <> maybeSpacedL mTargets <> semi
  pretty (ReturnStmt mConv mChoices actuals) =
    maybeSpacedR mConv <> "return" <+>
    maybe
      mempty
      (\(left, right) -> angles $ pretty left <> slash <> pretty right)
      mChoices <>
    parens (commaSep $ pretty <$> actuals) <> semi
  pretty (LabelStmt name) = pretty name <> colon
  pretty (ContStmt name kindedNames) =
    "continuation" <+>
    pretty name <> parens (commaSep $ pretty <$> kindedNames) <> colon
  pretty (GotoStmt expr mTargets) =
    "goto" <+> pretty expr <> maybeSpacedL mTargets <> semi
  pretty (CutToStmt expr actuals flows) =
    "cut" <+>
    "to" <+>
    pretty expr <>
    parens (commaSep $ pretty <$> actuals) <> hsep (pretty <$> flows) <> semi

instance Pretty (KindName a) where
  pretty (KindName mKind name) = maybeSpacedR mKind <> pretty name

instance Pretty (Arm a) where
  pretty (Arm ranges body) =
    "case" <+> commaSep (pretty <$> ranges) <> colon <+> pretty body

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
  pretty (AlsoCutsTo names) =
    "also" <+> "cuts" <+> "to" <+> commaSep (pretty <$> names)
  pretty (AlsoUnwindsTo names) =
    "also" <+> "unwinds" <+> "to" <+> commaSep (pretty <$> names)
  pretty (AlsoReturnsTo names) =
    "also" <+> "returns" <+> "to" <+> commaSep (pretty <$> names)
  pretty AlsoAborts = "also" <+> "aborts"
  pretty NeverReturns = "never" <+> "returns"

instance Pretty (Alias a) where
  pretty (Reads names) = "reads" <+> commaSep (pretty <$> names)
  pretty (Writes names) = "writes" <+> commaSep (pretty <$> names)

instance Pretty (CallAnnot a) where
  pretty (AliasAnnot alias) = pretty alias
  pretty (FlowAnnot flow) = pretty flow

instance Pretty (Targets a) where
  pretty (Targets names) = "targets" <+> commaSep (pretty <$> names)

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
    "%" <> pretty name <> parens (commaSep $ pretty <$> actuals)

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
  pretty (ParaType type' types) = hsep $ pretty <$> (type':types)

instance Pretty Conv where
  pretty (Foreign string) = "foreign" <+> pretty string

instance Pretty (Asserts a) where
  pretty (AlignAssert int []) = "aligned" <+> pretty int
  pretty (AlignAssert int names) =
    "aligned" <+> pretty int <+> "in" <+> commaSep (pretty <$> names)
  pretty (InAssert names mInt) =
    "in" <+> commaSep (pretty <$> names) <> maybeSpacedL mInt

instance Pretty (Pragma a) where
  pretty _ = error "`Pragma`s are not specified" -- FIXME: pragmas are not specified

instance Pretty (Name a) where
  pretty (Name name) = pretty name

instance Pretty StrLit where
  pretty (StrLit string) = pretty $ show string

prettyCallStmtRest :: Stmt a -> Doc ann
prettyCallStmtRest (CallStmt _ _ expr actuals mTargets flowOrAliases) =
  pretty expr <> parens (commaSep $ pretty <$> actuals) <+>
  maybeSpacedR mTargets <> hsep (pretty <$> flowOrAliases) <> semi
prettyCallStmtRest _ =
  error "`prettyCallStmtRest` is implemented only for call statements"

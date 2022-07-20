{-# LANGUAGE Safe #-}

{-|
Module      : CMM.AST
Description : AST
Maintainer  : jiriklepl@seznam.cz

This module defines the abstract syntactic tree for the extended C-- language.
-}

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
  , line, pipe
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
  , maybeSpacedR, hsepPretty
  )
import safe CMM.Utils (backQuote, HasCallStack)
import safe qualified CMM.Lexer.Token as T

-- | This AST node represents the whole compilation unit (program)
newtype Unit a =
  Unit [Annot TopLevel a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Unit ())

instance Pretty (Unit a) where
  pretty =
    \case
      Unit topLevels -> vsep $ pretty <$> topLevels

-- | AST node for a top-level definition,
--   extended with `TopClass`, `TopInstance` and `TopStruct` constructors
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

-- | AST node for a data section (for initialized and uninitialized data)
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

-- | AST node for a declaration (global or local)
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

-- | AST node for a typeclass - specific to CHMMM
data Class a =
  Class
    [Annot (ParaName Type) a] -- ^ superclasses
    (Annot (ParaName Name) a) -- ^ name and type signature of the class
    (Maybe (Annot FunDeps a)) -- ^ functional dependencies
    [Annot ProcedureDecl a] -- ^ methods
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Class ())

instance Pretty (Class a) where
  pretty class' =
    T.className <+>
    case class' of
      Class paraNames paraName mFunDeps methods ->
        pParaNames <+> pretty paraName <> pFunDeps <+> bracesBlock methods
        where
          pParaNames
            | null paraNames = mempty
            | otherwise = commaPretty paraNames <+> darrow <> space
          pFunDeps
            | Just funDeps <- mFunDeps = space <> pipe <+> pretty funDeps
            | otherwise = mempty

-- | AST node for a typeclass instance - specific to CHMMM
data Instance a =
  Instance
    [Annot (ParaName Type) a] -- ^ superclasses
    (Annot (ParaName Type) a) -- ^ name and type signature of the instance
    [Annot Procedure a] -- ^ method instances
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

-- | AST node for structs (record types) - specific to CHMMM
data Struct a =
  Struct
    (Annot (ParaName Name) a) -- ^ name and type signature of the struct
    [Annot Datum a] -- ^ data (labels and fields)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Struct ())

instance Pretty (Struct a) where
  pretty =
    \case
      Struct paraName datums ->
        T.structName <+> pretty paraName <+> inBraces (prettyDatums datums)

-- | AST node for a type signature
data ParaName param a =
  ParaName (Name a) [Annot param a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (param ()) => Eq (ParaName param ())

instance Pretty (param a) => Pretty (ParaName param a) where
  pretty =
    \case
      ParaName name [] -> pretty name
      ParaName name types -> pretty name <+> hsep (pretty <$> types)

-- | AST node for a functional dependency set - specific to CHMMM
newtype FunDeps a =
  FunDeps [Annot FunDep a] -- ^ functional dependencies in the given FD set
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (FunDeps ())

instance Pretty (FunDeps a) where
  pretty = \case
    FunDeps funDeps -> commaPretty funDeps

-- | AST node for a functional dependency (From -> To) - specific to CHMMM
data FunDep a = FunDep
  { fdFrom :: [Annot Name a] -- ^ (type variables) the assumptions of the given dependency
  , fdTo :: [Annot Name a] -- ^ (type variables) the results of the given dependency
  }
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (FunDep ())

instance Pretty (FunDep a) where
  pretty = \case
    FunDep{fdFrom, fdTo} -> pFdFrom <> arrow <> pFdTo
      where
        pFdFrom
          | null fdFrom = mempty
          | otherwise = hsepPretty fdFrom <> space
        pFdTo
          | null fdTo = mempty
          | otherwise = space <> hsepPretty fdTo

-- | AST node for a target directive
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

-- | AST node for an import
data Import a =
  Import (Maybe StrLit) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Import a) where
  pretty =
    \case
      Import (Just string) name -> pretty string <+> T.asName <+> pretty name
      Import Nothing name -> pretty name

-- | AST node for an export
data Export a =
  Export (Name a) (Maybe StrLit)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Export a) where
  pretty =
    \case
      Export name (Just string) -> pretty name <+> T.asName <+> pretty string
      Export name Nothing -> pretty name

-- | AST node for an endian directive
data Endian
  = Little
  | Big
  deriving (Show, Data, Eq)

instance Pretty Endian where
  pretty =
    \case
      Little -> T.littleName
      Big -> T.bigName

-- | AST node for a datum
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
    _
      | null others -> pretty datum
      | otherwise -> pretty datum <> line <> prettyDatums others

-- | AST node for an initializer
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

-- | AST node for a register pack
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

-- | AST node for a size specifier
newtype Size a =
  Size (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Size ())

instance Pretty (Size a) where
  pretty =
    \case
      Size mExpr -> brackets $ maybe mempty pretty mExpr

-- | AST node for a body (procedure body, if statement body, etc.)
newtype Body a =
  Body [Annot BodyItem a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Body ())

instance Pretty (Body a) where
  pretty =
    \case
      Body bodyItems -> bracesBlock bodyItems

-- | AST node for a body item
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

-- | AST node for a procedure definition
data Procedure a =
  Procedure (Annot ProcedureHeader a) (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Procedure ())

instance Pretty (Procedure a) where
  pretty =
    \case
      Procedure header body -> pretty header <+> pretty body

-- | AST node for a procedure declaration - specific to CHMMM
--   (used solely for method definitions)
newtype ProcedureDecl a =
  ProcedureDecl (Annot ProcedureHeader a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (ProcedureDecl ())

instance Pretty (ProcedureDecl a) where
  pretty =
    \case
      ProcedureDecl header -> pretty header <> semi

-- | AST node for a procedure header
--   extended with `SemiFormal`
data ProcedureHeader a =
  ProcedureHeader
    (Maybe Conv)
    (Name a)
    [Annot Formal a]
    (Maybe [Annot SemiFormal a]) -- ^ specifies the return types
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

-- | AST node for a formal argument
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

-- | AST node for a semiformal argument (a return type) - specific to CHMMM
data SemiFormal a =
  SemiFormal
    (Maybe Kind) -- ^ kind specification
    (Annot Type a) -- ^ the type of the semiformal (there is no name)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (SemiFormal ())

instance Pretty (SemiFormal a) where
  pretty =
    \case
      SemiFormal mKind type' -> maybeSpacedR mKind <> pretty type'

-- | AST node for an actual argument
data Actual a =
  Actual (Maybe Kind) (Annot Expr a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Actual ())

instance Pretty (Actual a) where
  pretty =
    \case
      Actual mKind expr -> maybeSpacedR mKind <> pretty expr

-- | AST node for a kind specification
newtype Kind =
  Kind StrLit
  deriving (Show, Data, Eq)

instance Pretty Kind where
  pretty =
    \case
      Kind string -> pretty string

-- | AST node for a stackdata block
newtype StackDecl a =
  StackDecl [Annot Datum a]
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (StackDecl ())

instance Pretty (StackDecl a) where
  pretty =
    \case
      StackDecl datums -> T.stackdataName <+> inBraces (prettyDatums datums)

-- | AST node for a statement
--   extended with `DroppedStmt`
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
  | DroppedStmt (Name a) -- ^ dropped statement that a part of
                         --   the control flow does not constitute
                         --   a certain automatic management action
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
      DroppedStmt name ->
        T.droppedName <+> pretty name <> semi

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

-- | AST node for a name with a kind specifier
data KindName a =
  KindName (Maybe Kind) (Name a)
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (KindName a) where
  pretty =
    \case
      KindName mKind name -> maybeSpacedR mKind <> pretty name

-- | AST node for a switch arm
data Arm a =
  Arm [Annot Range a] (Annot Body a)
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Arm ())

instance Pretty (Arm a) where
  pretty =
    \case
      Arm ranges body -> T.caseName <+> commaPretty ranges <> colon <+> pretty body

-- | AST node for a switch arm range
data Range a =
  Range (Annot Expr a) (Maybe (Annot Expr a))
  deriving (Show, Functor, Foldable, Traversable, Data)

deriving instance Eq (Range ())

instance Pretty (Range a) where
  pretty =
    \case
      Range left Nothing -> pretty left
      Range left (Just right) -> pretty left <+> ddot <+> pretty right

-- | AST node for an lvalue reference
--   the `LVRef` is modified by making the type optional
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

-- | AST node for a flow assertion
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

-- | AST node for an alias assertion
data Alias a
  = Reads [Annot Name a]
  | Writes [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Alias a) where
  pretty =
    \case
      Reads names -> T.readsName <+> commaPretty names
      Writes names -> T.writesName <+> commaPretty names

-- | AST node for a call annotation (contains an assertion)
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

-- | AST node for a targets assertion
newtype Targets a =
  Targets [Annot Name a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Targets a) where
  pretty =
    \case
      Targets names -> T.targetsName <+> commaPretty names

-- | AST node for an expression
--   extended by `MemberExpr` (struct->field) for field accessors
data Expr a
  = LitExpr (Annot Lit a) (Maybe (Annot Type a))
  | LVExpr (Annot LValue a)
  | ParExpr (Annot Expr a)
  | BinOpExpr Op (Annot Expr a) (Annot Expr a)
  | ComExpr (Annot Expr a)
  | NegExpr (Annot Expr a)
  | InfixExpr (Name a) (Annot Expr a) (Annot Expr a)
  | PrefixExpr (Name a) [Annot Actual a]
  | MemberExpr
    (Annot Expr a) -- ^ expression that evaluates to a struct
    (Annot Name a) -- ^ field name
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

-- | AST node for a literal
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

-- | AST node for a type
--   extended with `TAuto`, `TPtr`, `TVoid`, `TBool`, `TLabel` and `TPar`
data Type a
  = TBits Int
  | TName (Name a)
  | TAuto (Maybe (Name a))
  | TPtr (Annot Type a) -- ^ pointer type derived from the given type
  | TVoid -- ^ void type
  | TBool -- ^ bool type (for results of conditionals)
  | TLabel -- ^ label type for label statements
  | TPar (Annot ParaType a) -- ^ parametrized type
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Type a) where
  pretty =
    \case
      TBits int -> T.bitsName <> pretty int
      TName name -> pretty name
      TAuto mName -> T.autoName <> maybe mempty (parens . pretty) mName
      TPtr t -> T.ptrName <> parens (pretty t)
      TBool -> T.boolName
      TVoid -> T.voidName
      TLabel -> T.labelName
      TPar paraType -> parens $ pretty paraType

-- | AST node for a parametrized type - specific to CHMMM
data ParaType a =
  ParaType (Annot Type a) [Annot Type a]
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (ParaType a) where
  pretty =
    \case
      ParaType type' types -> hsep $ pretty <$> (type' : types)

-- | AST node for a call convention
newtype Conv =
  Foreign StrLit
  deriving (Show, Data, Eq)

instance Pretty Conv where
  pretty =
    \case
      Foreign string -> T.foreignName <+> pretty string

-- | AST node for in-assertions and align-assertions
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

-- | AST node for a binary operator
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

-- | AST node for an identifier
newtype Name a =
  Name Text
  deriving (Show, Functor, Foldable, Traversable, Data, Eq)

instance Pretty (Name a) where
  pretty =
    \case
      Name name -> pretty name

-- | AST node for a string literal
newtype StrLit =
  StrLit Text
  deriving (Show, Data, Eq)

instance Pretty StrLit where
  pretty =
    \case
      StrLit string -> pretty $ show string

-- | AST node for a pragma
data Pragma a
  deriving (Functor, Foldable, Traversable, Data) -- NOTE: the manual does not specify at all

instance Eq (Pragma a) where
  (==) = error "pragmas are not implemented"

instance Show (Pragma a) where
  show = error "pragmas are not implemented"

instance Pretty (Pragma a) where
  pretty _ = error $ backQuote T.pragmaName <> "s are not specified" -- NOTE: pragmas are not specified

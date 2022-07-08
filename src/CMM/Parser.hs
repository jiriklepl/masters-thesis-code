{-# LANGUAGE Safe #-}

module CMM.Parser where

import safe Control.Applicative
  ( Alternative((<|>))
  , Applicative(liftA2)
  , (<**>)
  , liftA2
  , liftA3
  , optional
  )
import safe Data.Functor (($>))
import safe Data.Maybe (fromMaybe, isJust)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(lookAhead, token, try)
  , Parsec
  , SourcePos
  , between
  , choice
  , many
  , sepBy1
  , sepEndBy
  , sepEndBy1
  )

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.Control.Applicative ((<*<), (>*>), liftA4, liftA6)
import safe qualified CMM.Lexer.Token as T

type Parser = Parsec Void [Annot T.Token SourcePos]

type SourceParser a = Parser (Annot a SourcePos)

type ULocParser a = Parser (a SourcePos)

optionalL :: Parser [a] -> Parser [a]
optionalL = (fromMaybe [] <$>) . optional

-- | parses the given reserved keyword
keyword :: T.Reserved -> Parser T.Reserved
keyword name =
  flip token Set.empty $ \case
    Annot (T.Keyword name') _ ->
      if name == name'
        then Just name
        else Nothing
    _ -> Nothing

-- | parses the given list of reserved keywords consecutively
keywords :: [T.Reserved] -> Parser T.Reserved
keywords = foldl1 (*>) . (keyword <$>)

-- | parses the given symbol
symbol :: T.Token SourcePos -> Parser ()
symbol tok =
  flip token Set.empty $ \case
    Annot tok' _ ->
      if tok == tok'
        then Just ()
        else Nothing

-- | parses an identifier
identifier :: Parser (AST.Name SourcePos)
identifier =
  flip token Set.empty $ \case
    Annot (T.Ident n) _ -> Just $ AST.Name n
    _ -> Nothing

-- | parses a string literal
stringLiteral :: Parser AST.StrLit
stringLiteral =
  flip token Set.empty $ \case
    Annot (T.StrLit t) _ -> Just $ AST.StrLit t
    _ -> Nothing

-- | parses a bits type
bitsType :: Parser Int
bitsType =
  flip token Set.empty $ \case
    Annot (T.BitsType i) _ -> Just i
    _ -> Nothing

-- | parses a character literal
charLit :: Parser Char
charLit =
  flip token Set.empty $ \case
    Annot (T.CharLit c) _ -> Just c
    _ -> Nothing

-- | parses a floating point literal
floatLit :: Parser Float
floatLit =
  flip token Set.empty $ \case
    Annot (T.FloatLit f) _ -> Just f
    _ -> Nothing

-- | parses an integer literal
intLit :: Parser (Int, Bool)
intLit =
  flip token Set.empty $ \case
    Annot (T.IntLit i) _ -> Just i
    _ -> Nothing

-- | returns the current position
getPos :: Parser SourcePos
getPos =
  lookAhead $
  flip token Set.empty $ \case
    Annot _ pos -> Just pos

-- | annotates the result of the given parser with the current location
withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 withAnnot getPos

-- | parses a comma-separated list of identifiers
identifiers :: Parser [Annot AST.Name SourcePos]
identifiers = commaList $ withSourcePos identifier

-- | encloses the given parser into braces
braces :: Parser a -> Parser a
braces = between (symbol T.LBrace) (symbol T.RBrace)

-- | encloses the given parser into parentheses
parens :: Parser a -> Parser a
parens = between (symbol T.LParen) (symbol T.RParen)

-- | encloses the given parser into brackets
brackets :: Parser a -> Parser a
brackets = between (symbol T.LBracket) (symbol T.RBracket)

-- | encloses the given parser into angle brackets
angles :: Parser a -> Parser a
angles = between (symbol T.Lt) (symbol T.Gt)

-- | runs the parser multiple times, separated by commas
commaList :: Parser a -> Parser [a]
commaList = (`sepEndBy1` comma)

-- | parses the whole program
program :: SourceParser AST.Unit
program = unit <* symbol T.Eof

-- | parses a comma
comma :: Parser ()
comma = symbol T.Comma

-- | parses a colon
colon :: Parser ()
colon = symbol T.Colon

-- | parses a semicolon
semicolon :: Parser ()
semicolon = symbol T.Semicolon

-- | Parses the whole 'Unit'
unit :: SourceParser AST.Unit
unit = withSourcePos $ AST.Unit <$> many topLevel

-- | parses a top level definition
topLevel :: SourceParser AST.TopLevel
topLevel =
  withSourcePos $
  choice
    [ sectionTopLevel
    , classTopLevel
    , instanceTopLevel
    , structTopLevel
    , AST.TopDecl <$> decl
    , AST.TopProcedure <$> try procedure
    ]

-- | parses a toplevel definition in a section
sectionTopLevel :: ULocParser AST.TopLevel
sectionTopLevel =
  keyword T.Section *> liftA2 AST.TopSection stringLiteral (braces $ many section)

-- | parses a class top level
classTopLevel :: ULocParser AST.TopLevel
classTopLevel = keyword T.Class *> (AST.TopClass <$> class')

-- | parses a class
class' :: SourceParser AST.Class
class' =
  withSourcePos $
  choice
    [ try $
      liftA3 AST.Class (sepBy1 paraName comma <* symbol T.DArr) paraName classBody
    , liftA2 (AST.Class []) paraName classBody
    ]
  where
    classBody =
      braces $
      many (withSourcePos $ AST.ProcedureDecl <$> procedureHeader <* semicolon)

-- | parses an instance top level
instanceTopLevel :: ULocParser AST.TopLevel
instanceTopLevel = keyword T.Instance *> (AST.TopInstance <$> instance')

-- | parses an instance
instance' :: SourceParser AST.Instance
instance' =
  withSourcePos $
  choice
    [ try $
      liftA3
        AST.Instance
        (sepBy1 paraName comma <* symbol T.DArr)
        paraName
        (braces $ many procedureInstance)
    , liftA2 (AST.Instance []) paraName (braces $ many procedureInstance)
    ]

-- | parses a struct top level
structTopLevel :: ULocParser AST.TopLevel
structTopLevel = keyword T.Struct *> (AST.TopStruct <$> struct)

-- | parses a struct
struct :: SourceParser AST.Struct
struct = withSourcePos $ liftA2 AST.Struct paraName (braces $ many datum)


class ParaName' param where
  paraName :: SourceParser (AST.ParaName param)

instance ParaName' AST.Type where
  paraName = withSourcePos $ liftA2 AST.ParaName identifier (many type')

instance ParaName' AST.Name where
  paraName =
    withSourcePos $ liftA2 AST.ParaName identifier (many $ withSourcePos identifier)

-- | parses a section
section :: SourceParser AST.Section
section =
  withSourcePos $
  choice
    [ AST.SecDecl <$> decl
    , secSpan
    , AST.SecProcedure <$> try procedure
    , AST.SecDatum <$> datum
    ]

-- | parses a declaration
decl :: SourceParser AST.Decl
decl =
  withSourcePos $
  choice
    [ importDecl
    , exportDecl
    , constDecl
    , typedefDecl
    , pragmaDecl
    , targetDecl
    , try registerDecl
    ]

-- | parses an import declaration
importDecl :: ULocParser AST.Decl
importDecl = keyword T.Import *> (AST.ImportDecl <$> commaList import') <* semicolon

-- | parses an export declaration
exportDecl :: ULocParser AST.Decl
exportDecl = keyword T.Export *> (AST.ExportDecl <$> commaList export) <* semicolon

-- | parses a constant declaration
constDecl :: ULocParser AST.Decl
constDecl =
  keyword T.Const *>
  liftA2
    (uncurry AST.ConstDecl)
    (try (liftA2 (,) (optional type') identifier) <|> (Nothing, ) <$> identifier)
    (symbol T.EqSign *> expr) <*
  semicolon

-- | parses a typedef declaration
typedefDecl :: ULocParser AST.Decl
typedefDecl =
  keyword T.Typedef *> liftA2 AST.TypedefDecl type' identifiers <* semicolon

-- | parses a pragma declaration
pragmaDecl :: ULocParser AST.Decl
pragmaDecl = keyword T.Pragma *> liftA2 AST.PragmaDecl identifier (braces pragma)

-- | parses a target declaration
targetDecl :: ULocParser AST.Decl
targetDecl =
  keyword T.Target *> (AST.TargetDecl <$> many targetDirective) <* semicolon

-- | parses a target directive
targetDirective :: SourceParser AST.TargetDirective
targetDirective =
  withSourcePos $
  choice
    [ memSizeDirective
    , byteOrderDirective
    , pointerSizeDirective
    , wordSizeDirective
    ]

-- | parses a register declaration
registerDecl :: ULocParser AST.Decl
registerDecl = liftA2 AST.RegDecl invariant registers <* semicolon

-- | parses a memory size directive
memSizeDirective :: ULocParser AST.TargetDirective
memSizeDirective = keyword T.Memsize *> (AST.MemSize . fst <$> intLit)

-- | parses a byte order directive
byteOrderDirective :: ULocParser AST.TargetDirective
byteOrderDirective = keyword T.Byteorder *> (AST.ByteOrder <$> endian)

-- | parses an endian directive
endian :: Parser AST.Endian
endian = choice [keyword T.Little $> AST.Little, keyword T.Big $> AST.Big]

-- | parses a pointer size directive
pointerSizeDirective :: ULocParser AST.TargetDirective
pointerSizeDirective = keyword T.Pointersize *> (AST.PointerSize . fst <$> intLit)

-- | parses a word size directive
wordSizeDirective :: ULocParser AST.TargetDirective
wordSizeDirective = keyword T.Wordsize *> (AST.WordSize . fst <$> intLit)

-- | parses a procedure header
procedureHeader :: SourceParser AST.ProcedureHeader
procedureHeader =
  withSourcePos $
  liftA4
    AST.ProcedureHeader
    (optional convention)
    identifier
    formals
    (optional $ symbol T.Arr *> sepEndBy semiFormal comma)

-- | parses a method instance header
procedureInstanceHeader :: SourceParser AST.ProcedureHeader
procedureInstanceHeader =
  withSourcePos $
  liftA4
    AST.ProcedureHeader
    (optional convention)
    identifier
    formalsInstance
    (optional $ symbol T.Arr *> sepEndBy semiFormalInstance comma)

-- | parses a procedure
procedure :: SourceParser AST.Procedure
procedure = withSourcePos $ liftA2 AST.Procedure procedureHeader body

-- | parses a method instance
procedureInstance :: SourceParser AST.Procedure
procedureInstance = withSourcePos $ liftA2 AST.Procedure procedureInstanceHeader body

-- | parses a formal argument
formal :: SourceParser AST.Formal
formal = withSourcePos $ liftA4 AST.Formal mKind invariant type' identifier

-- | parses a formal argument of a method instance
formalInstance :: SourceParser AST.Formal
formalInstance = withSourcePos $ liftA4 AST.Formal mKind invariant justAutoType identifier

-- | parses a semiformal (return) of a procedure
semiFormal :: SourceParser AST.SemiFormal
semiFormal = withSourcePos $ liftA2 AST.SemiFormal mKind type'

-- | parses a semiformal (return) of a method instance
semiFormalInstance :: SourceParser AST.SemiFormal
semiFormalInstance = withSourcePos $ liftA2 AST.SemiFormal mKind justAutoType

-- | parses a list of formals
formals :: Parser [Annot AST.Formal SourcePos]
formals = parens $ formal `sepEndBy` comma

-- | parses a list of formals of a method instance
formalsInstance :: Parser [Annot AST.Formal SourcePos]
formalsInstance = parens $ formalInstance `sepEndBy` comma

-- | parses an "invariant" keyword
invariant :: Parser Bool
invariant = keyword T.Invariant $> True <|> pure False

-- | parses an actual argument
actual :: SourceParser AST.Actual
actual = withSourcePos $ liftA2 AST.Actual mKind expr

-- | parses a list of actual arguments
actuals :: Parser [Annot AST.Actual SourcePos]
actuals = parens $ actual `sepEndBy` comma

-- | parses a call convention
convention :: Parser AST.Conv
convention = keyword T.Foreign *> (AST.Foreign <$> stringLiteral)

-- | parses an import
import' :: SourceParser AST.Import
import' =
  withSourcePos $
  liftA2 AST.Import (optional (stringLiteral <* keyword T.As)) identifier

-- | parses an export
export :: SourceParser AST.Export
export =
  withSourcePos $
  liftA2 AST.Export identifier (optional (keyword T.As *> stringLiteral))

-- | parses a body
body :: SourceParser AST.Body
body = withSourcePos . braces $ AST.Body <$> many bodyItem

-- | parses a body item
bodyItem :: SourceParser AST.BodyItem
bodyItem =
  withSourcePos $
  AST.BodyDecl <$> try decl <|> AST.BodyStackDecl <$> stackDecl <|> AST.BodyStmt <$> stmt

-- | parses a section span
secSpan :: ULocParser AST.Section
secSpan = keyword T.Span *> liftA3 AST.SecSpan expr expr (braces $ many section)

-- | parses a datum
datum :: SourceParser AST.Datum
datum = withSourcePos $ choice [alignDatum, try labelDatum, justDatum]

-- | parses an "align" datum
alignDatum :: ULocParser AST.Datum
alignDatum = keyword T.Align *> (AST.DatumAlign . fst <$> intLit) <* semicolon

-- | parses  a label datum
labelDatum :: ULocParser AST.Datum
labelDatum = AST.DatumLabel <$> identifier <* colon

-- | parses a datum that defines a field
justDatum :: ULocParser AST.Datum
justDatum = liftA4 AST.Datum (fmap isJust . optional $ keyword T.New) type' (optional size) (optional init') <* semicolon

-- | parses an initializer
init' :: SourceParser AST.Init
init' = withSourcePos $ choice [stringInit, string16Init, initList]

-- | parses an initializer list
initList :: ULocParser AST.Init
initList = braces $ AST.ExprInit <$> commaList expr

-- | parses a string initializer
stringInit :: ULocParser AST.Init
stringInit = AST.StrInit <$> stringLiteral

-- | parses an utf16 string initializer
string16Init :: ULocParser AST.Init
string16Init = keyword T.Unicode *> (AST.Str16Init <$> parens stringLiteral)

-- | parses a size specifier
size :: SourceParser AST.Size
size = withSourcePos . brackets $ AST.Size <$> optional expr

-- | parses registers
registers :: SourceParser AST.Registers
registers =
  withSourcePos $
  liftA3
    AST.Registers
    mKind
    type'
    (commaList
       (liftA2
          (,)
          (withSourcePos identifier)
          (optional $ symbol T.EqSign *> stringLiteral)))

-- | parses a type
type' :: SourceParser AST.Type
type' = withSourcePos $ choice [parensType, ptrType, autoType, bitsType', nameType]

-- | parses an `AST.TPar`
parensType :: ULocParser AST.Type
parensType = AST.TPar <$> parens paraType

-- | parses a type variable
autoType :: ULocParser AST.Type
autoType = AST.TAuto <$> (keyword T.Auto *> optional (parens identifier))

-- | parses an anonymous type variable
justAutoType :: SourceParser AST.Type
justAutoType = withSourcePos $ AST.TAuto Nothing <$ keyword T.Auto

-- | parses a pointer type
ptrType :: ULocParser AST.Type
ptrType = keyword T.Ptr *> (AST.TPtr <$> withSourcePos parensType)

-- | parses a bits type
bitsType' :: ULocParser AST.Type
bitsType' = AST.TBits <$> bitsType

-- | parses a named type
nameType :: ULocParser AST.Type
nameType = AST.TName <$> identifier

-- | parses a type with parameters
paraType :: SourceParser AST.ParaType
paraType = withSourcePos $ liftA2 AST.ParaType type' (many type')

-- | optionally parses a kind
mKind :: Parser (Maybe AST.Kind)
mKind = optional $ AST.Kind <$> stringLiteral

-- | parses a pragma
pragma :: Parser a
pragma = error "Pragmas not specified" -- NOTE: pragmas not yet specified and with no explanation of functionality

-- | parses a stack declaration
stackDecl :: SourceParser AST.StackDecl
stackDecl =
  withSourcePos $ keyword T.Stackdata *> braces (AST.StackDecl <$> many datum)

-- | parses statement
stmt :: SourceParser AST.Stmt
stmt =
  withSourcePos $
  choice
    [ emptyStmt
    , ifStmt
    , switchStmt
    , spanStmt
    , jumpStmt
    , returnStmt
    , gotoStmt
    , contStmt
    , cutToStmt
    , try assignStmt
    , try primOpStmt
    , try labelStmt
    , callStmt
    ]

-- | parses an empty statement
emptyStmt :: ULocParser AST.Stmt
emptyStmt = semicolon $> AST.EmptyStmt

-- | parses an if statement
ifStmt :: ULocParser AST.Stmt
ifStmt =
  keyword T.If *> liftA3 AST.IfStmt expr body (optional $ keyword T.Else *> body)

-- | parses a switch statement
switchStmt :: ULocParser AST.Stmt
switchStmt = keyword T.Switch *> liftA2 AST.SwitchStmt expr (braces (many arm))

-- | parses a span statement
spanStmt :: ULocParser AST.Stmt
spanStmt = keyword T.Span *> liftA3 AST.SpanStmt expr expr body

-- | parses an assignment statement
assignStmt :: ULocParser AST.Stmt
assignStmt =
  liftA2 AST.AssignStmt (commaList lvalue <* symbol T.EqSign) (commaList expr) <*
  semicolon

-- | parses a primOp statement
primOpStmt :: ULocParser AST.Stmt
primOpStmt =
  liftA4
    AST.PrimOpStmt
    (identifier <* symbol T.EqSign)
    (symbol T.DPercent *> identifier)
    (optionalL actuals)
    (many flow) <*
  semicolon

-- | parses a call statement
callStmt :: ULocParser AST.Stmt
callStmt =
  liftA6
    AST.CallStmt
    (optionalL kindedNames <* symbol T.EqSign)
    (optional convention)
    expr
    actuals
    (optional targets)
    (many . withSourcePos $ AST.FlowAnnot <$> flow <|> AST.AliasAnnot <$> alias) <*
  semicolon

-- | parses a jump statement
jumpStmt :: ULocParser AST.Stmt
jumpStmt =
  liftA4
    AST.JumpStmt
    (optional convention <* keyword T.Jump)
    expr
    (optionalL actuals)
    (optional targets) <*
  semicolon

-- | parses a return statement
returnStmt :: ULocParser AST.Stmt
returnStmt =
  liftA3
    AST.ReturnStmt
    (optional convention <* keyword T.Return)
    (optional . angles $
     liftA2 (,) (restrictedExpr <* symbol T.Slash) restrictedExpr)
    (optionalL actuals) <*
  semicolon

-- | parses an lvalue
lvalue :: SourceParser AST.LValue
lvalue = withSourcePos $ try lvRef <|> lvName

-- | parses an lvalue reference
lvRef :: ULocParser AST.LValue
lvRef =
  liftA3 AST.LVRef (optional type') (symbol T.LBracket *> expr) (optional asserts) <*
  symbol T.RBracket

-- | parses a reference to a named object
lvName :: ULocParser AST.LValue
lvName = AST.LVName <$> identifier

-- | parses an assertion
asserts :: Parser (Annot AST.Asserts SourcePos)
asserts = withSourcePos $ alignAssert <|> inAssert

-- | parses an align assertion
alignAssert :: ULocParser AST.Asserts
alignAssert =
  liftA2
    AST.AlignAssert
    (keyword T.Aligned *> (fst <$> intLit))
    (optionalL $ keyword T.In *> (withSourcePos identifier `sepBy1` comma))

-- | parses an "in" assertion
inAssert :: ULocParser AST.Asserts
inAssert =
  liftA2
    AST.InAssert
    (keyword T.In *> (withSourcePos identifier `sepBy1` comma))
    (optional (keyword T.Aligned *> (fst <$> intLit)))

-- | parses a label statement
labelStmt :: ULocParser AST.Stmt
labelStmt = AST.LabelStmt <$> identifier <* colon

-- | parses a continuation statement
contStmt :: ULocParser AST.Stmt
contStmt =
  keyword T.Continuation *>
  liftA2 AST.ContStmt identifier (parens $ optionalL kindedNames) <*
  colon

-- | parses a goto statement
gotoStmt :: ULocParser AST.Stmt
gotoStmt =
  keyword T.Goto *> liftA2 AST.GotoStmt expr (optional targets) <* semicolon

-- | parses a cut to statement
cutToStmt :: ULocParser AST.Stmt
cutToStmt =
  keywords [T.Cut, T.To] *> liftA3 AST.CutToStmt expr actuals (many flow) <*
  semicolon

-- | parses a list of kinded names (names that can be given kinds)
kindedNames :: Parser [Annot AST.KindName SourcePos]
kindedNames = commaList . withSourcePos $ liftA2 AST.KindName mKind identifier

-- | parses an arm of a switch statement
arm :: SourceParser AST.Arm
arm =
  withSourcePos $ keyword T.Case *> liftA2 AST.Arm (commaList range <* colon) body

-- | parses a range of an arm
range :: SourceParser AST.Range
range = withSourcePos $ liftA2 AST.Range expr (optional $ symbol T.DotDot *> expr)

-- | parses a flow annotation
flow :: SourceParser AST.Flow
flow = withSourcePos $ alsoFlow <|> neverReturns

-- | parses an "also" flow annotation
alsoFlow :: ULocParser AST.Flow
alsoFlow =
  keyword T.Also *>
  choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

-- | parses an "also cuts to" flow annotation
alsoCutsTo :: ULocParser AST.Flow
alsoCutsTo = keywords [T.Cuts, T.To] *> (AST.AlsoCutsTo <$> identifiers)

-- | parses an "also unwinds to" flow annotation
alsoUnwindsTo :: ULocParser AST.Flow
alsoUnwindsTo = keywords [T.Unwinds, T.To] *> (AST.AlsoUnwindsTo <$> identifiers)

-- | parses an "also returns to" flow annotation
alsoReturnsTo :: ULocParser AST.Flow
alsoReturnsTo = keywords [T.Returns, T.To] *> (AST.AlsoReturnsTo <$> identifiers)

-- | parses an "also aborts" flow annotation
alsoAborts :: ULocParser AST.Flow
alsoAborts = keyword T.Aborts *> optional comma $> AST.AlsoAborts

-- | parses a "never returns" flow annotation
neverReturns :: ULocParser AST.Flow
neverReturns = keywords [T.Never, T.Returns] *> optional comma $> AST.NeverReturns

-- | parses an alias annotation
alias :: SourceParser AST.Alias
alias = withSourcePos $ readsAlias <|> writesAlias

-- | parses a reads alias
readsAlias :: ULocParser AST.Alias
readsAlias = keyword T.Reads *> (AST.Reads <$> identifiers)

-- | parses a writes alias
writesAlias :: ULocParser AST.Alias
writesAlias = keyword T.Writes *> (AST.Writes <$> identifiers)

-- | parses a targets annotation
targets :: SourceParser AST.Targets
targets = withSourcePos $ keyword T.Targets *> (AST.Targets <$> identifiers)

-- | parses an expression
expr :: SourceParser AST.Expr
expr = infixExpr

-- | parses a simple expression
simpleExpr :: SourceParser AST.Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

-- | parses a restricted expression (just literals, names, and parenthesized expressions)
restrictedExpr :: SourceParser AST.Expr
restrictedExpr = choice [litExpr, parExpr, nameExpr]

-- | parses an literal expression
litExpr :: SourceParser AST.Expr
litExpr =
  withSourcePos $
  liftA2
    AST.LitExpr
    (withSourcePos $ choice [charExpr, floatExpr, intExpr])
    (optional $ symbol T.DColon *> type')

-- | parses an integer literal expression
intExpr :: ULocParser AST.Lit
intExpr = AST.LitInt . fst <$> intLit

-- | parses a floating point literal expression
floatExpr :: ULocParser AST.Lit
floatExpr = AST.LitFloat <$> floatLit

-- | parses an character literal expression
charExpr :: ULocParser AST.Lit
charExpr = AST.LitChar <$> charLit

-- | parses an reference to a named object
nameExpr :: SourceParser AST.Expr
nameExpr = withSourcePos $ AST.LVExpr <$> withSourcePos lvName

-- | parses a dereference
refExpr :: SourceParser AST.Expr
refExpr = withSourcePos $ AST.LVExpr <$> withSourcePos lvRef

-- | parses a parenthesized expression
parExpr :: SourceParser AST.Expr
parExpr = withSourcePos $ AST.ParExpr <$> parens expr

-- SYMBOLIC OPERATORS --
-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4.1)
infixExpr :: SourceParser AST.Expr
infixExpr = opImplN ("`" :: Text) cmpExpr

-- | parses a comparison expression
cmpExpr :: SourceParser AST.Expr
cmpExpr =
  opImplN
    [ (AST.GeOp, T.Geq :: T.Token SourcePos)
    , (AST.GtOp, T.Gt)
    , (AST.LeOp, T.Leq)
    , (AST.LtOp, T.Lt)
    , (AST.NeqOp, T.Neq)
    , (AST.EqOp, T.Eq)
    ]
    orExpr

-- | parses an or expression
orExpr :: SourceParser AST.Expr
orExpr = opImplL (AST.OrOp, T.Pipe :: T.Token SourcePos) xorExpr

-- | parses an xor expression
xorExpr :: SourceParser AST.Expr
xorExpr = opImplL (AST.XorOp, T.Caret :: T.Token SourcePos) andExpr

-- | parses an and expression
andExpr :: SourceParser AST.Expr
andExpr = opImplL (AST.AndOp, T.Ampersand :: T.Token SourcePos) shExpr

-- | parses a shift expression
shExpr :: SourceParser AST.Expr
shExpr = opImplL [(AST.ShLOp, T.ShL :: T.Token SourcePos), (AST.ShROp, T.ShR)] addExpr

-- | parses an add (or sub) expression
addExpr :: SourceParser AST.Expr
addExpr =
  opImplL [(AST.AddOp, T.Plus :: T.Token SourcePos), (AST.SubOp, T.Minus)] mulExpr

-- | parses an mul (or div, mod) expression
mulExpr :: SourceParser AST.Expr
mulExpr =
  opImplL
    [(AST.DivOp, T.Slash :: T.Token SourcePos), (AST.MulOp, T.Star), (AST.ModOp, T.Percent)]
    negExpr

-- SYMBOLIC OPERATORS -- END

-- | parses a negation expression
negExpr :: SourceParser AST.Expr
negExpr =
  withSourcePos (symbol T.Minus *> (AST.NegExpr <$> negExpr)) <|>
  withSourcePos (symbol T.Tilde *> (AST.ComExpr <$> negExpr)) <|>
  memberExpr

-- | parses a member expression
memberExpr :: SourceParser AST.Expr
memberExpr = do
  expr' <- simpleExpr
  choice
    [ symbol T.Arr *>
      withSourcePos (AST.MemberExpr expr' <$> withSourcePos identifier)
    , return expr'
    ]

-- | parses a prefix expression
prefixExpr :: SourceParser AST.Expr
prefixExpr =
  withSourcePos $
  symbol T.Percent *> liftA2 AST.PrefixExpr identifier (optionalL actuals)

class OpImpl a where
  opImplL :: a -> SourceParser AST.Expr -> SourceParser AST.Expr
  opImplL x next = next <**> opRestImplL x next
  opImplN :: a -> SourceParser AST.Expr -> SourceParser AST.Expr
  opImplN x next = next <**> opRestImplN x next
  opRestImplL ::
       a
    -> SourceParser AST.Expr
    -> Parser (Annot AST.Expr SourcePos -> Annot AST.Expr SourcePos)
  opRestImplL x next =
    withAnnot <$> getPos <*< opRestInner x next >*> opRestImplL x next <|>
    pure id
  opRestImplN ::
       a
    -> SourceParser AST.Expr
    -> Parser (Annot AST.Expr SourcePos -> Annot AST.Expr SourcePos)
  opRestImplN x next = withAnnot <$> getPos <*< opRestInner x next <|> pure id
  opRestInner ::
       a -> SourceParser AST.Expr -> Parser (Annot AST.Expr SourcePos -> AST.Expr SourcePos)

instance OpImpl (AST.Op, T.Token SourcePos) where
  opRestInner (op, str) next = flip (AST.BinOpExpr op) <$> (symbol str *> next)

instance OpImpl x => OpImpl [x] where
  opRestInner xs next = foldl1 (<|>) (flip opRestInner next <$> xs)

instance OpImpl Text where
  opRestInner "`" next =
    symbol T.Backtick *> (flip . AST.InfixExpr <$> identifier) <* symbol T.Backtick <*>
    next
  opRestInner _ _ = error "Parser not implemented for this operator"

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Language.Parser where

import Control.Applicative hiding (many)
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.CMM

type Parser = Parsec Void Text

type SourceParser a = Parser (Annot SourcePos a)

type ULocParser a = Parser (a SourcePos)

maybeToMonoid :: Monoid a => Maybe a -> a
maybeToMonoid = fromMaybe mempty

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

keyword :: Text -> Parser ()
keyword = void . lexeme . string

stringLiteral :: Parser Text
stringLiteral =
  lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

charLiteral :: Parser Char
charLiteral = lexeme $ char '\'' *> L.charLiteral <* char '\''

name :: Parser Text
name = do
  n <- letterChar <|> otherChars
  ame <- T.pack <$> many (alphaNumChar <|> otherChars)
  return $ n `T.cons` ame
  where
    otherChars = oneOf ['_', '.', '$', '@']

withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 Annot getSourcePos

identifier :: Parser Name
identifier = Name <$> lexeme name

identifiers :: Parser [Name]
identifiers = commaList identifier

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

comma :: Parser ()
comma = void $ symbol ","

semicolon :: Parser ()
semicolon = void $ symbol ";"

commaList :: Parser a -> Parser [a]
commaList = (`sepEndBy1` comma)

integer :: Parser (Int, Bool)
integer =
  lexeme $
  (char '0' *>
   ((oneOf ['x', 'X'] *> ((, False) <$> L.hexadecimal)) <|>
    (, False) <$> L.octal <|>
    (,) 0 . isNothing <$> optional (oneOf ['u', 'U']))) <|>
  decimal

decimal :: Parser (Int, Bool)
decimal = do
  nums <- L.decimal
  unsigned <- optional $ oneOf ['u', 'U']
  return (nums, isNothing unsigned)

program :: SourceParser Unit
program = sc *> unit

-- | Parses the whole 'Unit'
unit :: SourceParser Unit
unit = withSourcePos $ Unit <$> many topLevel

topLevel :: SourceParser TopLevel
topLevel =
  withSourcePos $
  choice [sectionTopLevel, try $ TopProcedure <$> procedure, TopDecl <$> decl]

sectionTopLevel :: ULocParser TopLevel
sectionTopLevel =
  keyword "section" *> (TopSection <$> stringLiteral <*> braces (many section))

section :: SourceParser Section
section =
  withSourcePos $
  choice
    [ SecDecl <$> decl
    , secSpan
    , try $ SecProcedure <$> procedure
    , SecDatum <$> datum
    ]

decl :: SourceParser Decl
decl =
  withSourcePos $
  choice
    [ importDecl
    , exportDecl
    , constDecl
    , typedefDecl
    , pragmaDecl
    , targetDecl
    , registerDecl
    ]

importDecl :: ULocParser Decl
importDecl = keyword "import" *> (ImportDecl <$> commaList import_) <* semicolon

exportDecl :: ULocParser Decl
exportDecl = keyword "export" *> (ExportDecl <$> commaList export) <* semicolon

constDecl :: ULocParser Decl
constDecl = do
  keyword "const"
  try
    (do t <- typeToken
        declName <- identifier
        symbol "="
        expr <- expression
        semicolon
        return $ ConstDecl (Just t) declName expr) <|> do
    declName <- identifier
    symbol "="
    expr <- expression
    semicolon
    return $ ConstDecl Nothing declName expr

typedefDecl :: ULocParser Decl
typedefDecl =
  keyword "typedef" *> (TypedefDecl <$> typeToken <*> identifiers) <* semicolon

pragmaDecl :: ULocParser Decl
pragmaDecl = do
  keyword "pragma"
  pragmaName <- identifier
  p <- braces pragma
  return $ PragmaDecl pragmaName p

targetDecl :: ULocParser Decl
targetDecl =
  keyword "target" *> (TargetDecl <$> many targetDirective) <* semicolon

targetDirective :: SourceParser TargetDirective
targetDirective =
  withSourcePos $
  choice
    [ memSizeDirective
    , byteOrderDirective
    , pointerSizeDirective
    , wordSizeDirective
    ]

registerDecl :: ULocParser Decl
registerDecl =
  ((RegDecl . (Invariant <$) <$> optional (keyword "invariant")) <*> registers) <*
  semicolon

memSizeDirective :: ULocParser TargetDirective
memSizeDirective = do
  keyword "memsize"
  (memSize, False) <- integer
  return $ MemSize memSize

byteOrderDirective :: ULocParser TargetDirective
byteOrderDirective = keyword "byteorder" *> (ByteOrder <$> endian)

endian :: Parser Endian
endian = choice [keyword "little" $> Little, keyword "big" $> Big]

pointerSizeDirective :: ULocParser TargetDirective
pointerSizeDirective = do
  keyword "pointersize"
  (pointerSize, False) <- integer
  return $ PointerSize pointerSize

wordSizeDirective :: ULocParser TargetDirective
wordSizeDirective = do
  keyword "wordsize"
  (wordSize, False) <- integer
  return $ PointerSize wordSize

procedure :: SourceParser Procedure
procedure =
  withSourcePos $ do
    conv <- optional convention
    procName <- identifier
    formals <- parens $ formal `sepEndBy` comma -- TODO: discuss later
    Procedure conv procName formals <$> body

formal :: SourceParser Formal
formal =
  withSourcePos $ do
    k <- optional kind
    invar <- optional (keyword "invariant")
    t <- typeToken
    Formal k (Invariant <$ invar) t <$> identifier

actual :: SourceParser Actual
actual = withSourcePos $ (Actual <$> optional kind) <*> expression

actuals :: Parser [Annot SourcePos Actual]
actuals = parens $ actual `sepEndBy` comma

convention :: Parser Conv
convention = keyword "foreign" >> Foreign <$> stringLiteral

import_ :: SourceParser Import
import_ =
  withSourcePos $
  Import <$> optional (stringLiteral <* keyword "as") <*> identifier

export :: SourceParser Export
export =
  withSourcePos $
  Export <$> identifier <*> optional (keyword "as" *> stringLiteral)

body :: SourceParser Body
body = withSourcePos . braces $ Body <$> many bodyItem

bodyItem :: SourceParser BodyItem
bodyItem = withSourcePos $ BodyStackDecl <$> stackDecl <|> BodyStmt <$> stmt

secSpan :: ULocParser Section
secSpan = do
  left <- expression
  right <- expression
  sections <- many section
  return $ SecSpan left right sections

datum :: SourceParser Datum
datum = withSourcePos $ choice [try alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser Datum
alignDatum = do
  keyword "align"
  (align, False) <- integer
  semicolon
  return $ DatumAlign align

labelDatum :: ULocParser Datum
labelDatum = DatumLabel <$> identifier <* symbol ":"

justDatum :: ULocParser Datum
justDatum =
  Datum <$> typeToken <*> optional size <*> optional init_ <* semicolon

init_ :: SourceParser Init
init_ = withSourcePos $ choice [stringInit, string16Init, initList]

initList :: ULocParser Init
initList = braces $ ExprInit <$> commaList expression

stringInit :: ULocParser Init
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser Init
string16Init = do
  keyword "unicode"
  str <- parens stringLiteral
  return . Str16Init $ String16 str -- TODO: 16bit strings not yet implemented

size :: SourceParser Size
size = withSourcePos . brackets $ Size <$> optional expression

registers :: SourceParser Registers
registers =
  withSourcePos $ do
    k <- optional kind
    t <- typeToken
    nvals <-
      commaList
        (do n <- identifier
            val <-
              optional $ do
                symbol "="
                stringLiteral
            return (n, val))
    return $ Registers k t nvals

typeToken :: SourceParser Type
typeToken = withSourcePos $ bitsType <|> nameType

bitsType :: ULocParser Type
bitsType = lexeme $ string "bits" >> TBits <$> L.decimal

nameType :: ULocParser Type
nameType = TName <$> identifier

kind :: Parser Kind
kind = Kind <$> stringLiteral

pragma :: Parser a
pragma = undefined -- TODO pragmas not yet specified and with no explanation of functionality

stackDecl :: SourceParser StackDecl
stackDecl =
  withSourcePos $ keyword "stackdata" *> braces (StackDecl <$> many datum)

stmt :: SourceParser Stmt
stmt =
  withSourcePos $
  choice
    [ emptyStmt
    , try ifStmt
    , try switchStmt
    , try spanStmt
    , try assignStmt
    , try primOpStmt
    , try jumpStmt
    , try returnStmt
    , try labelStmt
    , try contStmt
    , try gotoStmt
    , try cutToStmt
    , callStmt
    ] -- TODO: this is utter BS

emptyStmt :: ULocParser Stmt
emptyStmt = semicolon $> EmptyStmt

ifStmt :: ULocParser Stmt
ifStmt = do
  keyword "if"
  expr <- expression
  ifBody <- body
  elseBody <- optional $ keyword "else" >> body
  return $ IfStmt expr ifBody elseBody

switchStmt :: ULocParser Stmt
switchStmt =
  keyword "switch" *> (SwitchStmt <$> expression <*> braces (many arm))

spanStmt :: ULocParser Stmt
spanStmt = keyword "span" *> (SpanStmt <$> expression <*> expression <*> body)

assignStmt :: ULocParser Stmt
assignStmt =
  liftA2
    AssignStmt
    (commaList lvalue <* symbol "=")
    (commaList expression <* semicolon)

primOpStmt :: ULocParser Stmt
primOpStmt = do
  lName <- identifier
  symbol "="
  symbol "%%"
  rName <- identifier
  mActuals <- optional actuals
  flows <- many flow
  semicolon
  return $ PrimOpStmt lName rName (maybeToMonoid mActuals) flows

callStmt :: ULocParser Stmt
callStmt = do
  mKindNames <- optional kindedNames <* symbol "="
  mConv <- optional convention
  expr <- expression
  acts <- actuals
  mTargs <- optional targets
  annots <- many (Left <$> flow <|> Right <$> alias)
  semicolon
  return $ CallStmt (maybeToMonoid mKindNames) mConv expr acts mTargs annots

jumpStmt :: ULocParser Stmt
jumpStmt = do
  mConv <- optional convention
  keyword "jump"
  expr <- expression
  mActuals <- optional actuals
  mTargs <- optional targets
  semicolon
  return $ JumpStmt mConv expr (maybeToMonoid mActuals) mTargs

returnStmt :: ULocParser Stmt
returnStmt = do
  mConv <- optional convention
  keyword "return"
  mExprs <- optional . angles $ liftA2 (,) expression expression
  mActuals <- optional actuals
  semicolon
  return $ ReturnStmt mConv mExprs (maybeToMonoid mActuals)

lvalue :: SourceParser LValue
lvalue = withSourcePos $ try lvRef <|> lvName

lvRef :: ULocParser LValue
lvRef =
  (LVRef <$> typeToken <*> (symbol "[" *> expression) <*> optional assertions) <*
  symbol "]"

lvName :: ULocParser LValue
lvName = LVName <$> identifier

assertions :: Parser (Annot SourcePos Asserts)
assertions = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser Asserts
alignAssert =
  liftA2
    AlignAssert
    (keyword "aligned" *> (fst <$> integer))
    (maybeToMonoid <$> optional (keyword "in" *> (identifier `sepBy1` comma)))

inAssert :: ULocParser Asserts
inAssert =
  liftA2
    InAssert
    (keyword "in" *> (identifier `sepBy1` comma))
    (optional (keyword "aligned" *> (fst <$> integer)))

labelStmt :: ULocParser Stmt
labelStmt = LabelStmt <$> identifier <* symbol ":"

contStmt :: ULocParser Stmt
contStmt = do
  keyword "continuation"
  n <- identifier
  params <- parens $ optional kindedNames
  symbol ":"
  return $ ContStmt n (maybeToMonoid params)

gotoStmt :: ULocParser Stmt
gotoStmt = do
  keyword "goto"
  expr <- expression
  mTargets <- optional targets
  semicolon
  return $ GotoStmt expr mTargets

cutToStmt :: ULocParser Stmt
cutToStmt = do
  keyword "cut"
  keyword "to"
  expr <- expression
  acts <- actuals
  mFlow <- many flow
  semicolon
  return $ CutToStmt expr acts mFlow

kindedNames :: Parser [Annot SourcePos KindName]
kindedNames =
  commaList
    (do k <- optional kind
        pos <- getSourcePos
        Annot pos . KindName k <$> identifier)

arm :: SourceParser Arm
arm =
  withSourcePos $
  keyword "case" *> (Arm <$> commaList range <* symbol ":") <*> body

range :: SourceParser Range
range =
  withSourcePos $
  (Range <$> expression) <*> optional (symbol ".." *> expression)

flow :: SourceParser Flow
flow = withSourcePos $ alsoFlow <|> neverReturns

alsoFlow :: ULocParser Flow
alsoFlow =
  keyword "also" *>
  choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

alsoCutsTo :: ULocParser Flow
alsoCutsTo = keyword "cuts" *> keyword "to" *> (AlsoCutsTo <$> identifiers)

alsoUnwindsTo :: ULocParser Flow
alsoUnwindsTo =
  keyword "unwinds" *> keyword "to" *> (AlsoUnwindsTo <$> identifiers)

alsoReturnsTo :: ULocParser Flow
alsoReturnsTo =
  keyword "returns" *> keyword "to" *> (AlsoReturnsTo <$> identifiers)

alsoAborts :: ULocParser Flow
alsoAborts = keyword "aborts" *> optional comma $> AlsoAborts

neverReturns :: ULocParser Flow
neverReturns =
  keyword "never" *> keyword "returns" *> optional comma $> NeverReturns

alias :: SourceParser Alias
alias = withSourcePos $ readsAlias <|> writesAlias

readsAlias :: ULocParser Alias
readsAlias = keyword "reads" *> (Reads <$> identifiers)

writesAlias :: ULocParser Alias
writesAlias = keyword "writes" *> (Writes <$> identifiers)

targets :: SourceParser Targets
targets = withSourcePos $ keyword "targets" *> (Targets <$> identifiers)

expression :: SourceParser Expr
expression = try infixExpr <|> binOpExpr

simpleExpr :: SourceParser Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

litExpr :: SourceParser Expr
litExpr = do
  literal <- choice [intExpr, floatExpr, charExpr]
  mType <- optional $ symbol "::" *> typeToken
  withSourcePos . return $ LitExpr literal mType

intExpr :: SourceParser Lit
intExpr = withSourcePos $ LitInt . fst <$> integer

floatExpr :: SourceParser Lit
floatExpr = intExpr -- TODO: implement for floats

charExpr :: SourceParser Lit
charExpr = withSourcePos $ LitChar <$> charLiteral

nameExpr :: SourceParser Expr
nameExpr = withSourcePos $ NameExpr <$> identifier

refExpr :: SourceParser Expr
refExpr =
  withSourcePos $ do
    t <- typeToken
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ RefExpr t expr mAsserts

parExpr :: SourceParser Expr
parExpr = withSourcePos $ ParExpr <$> parens expression

binOpExpr :: SourceParser Expr
binOpExpr = makeExprParser simpleExpr binOpTable

binOpTable :: [[Operator Parser (Annot annot Expr)]]
binOpTable =
  [ [prefix "-" NegExpr, prefix "~" ComExpr]
  , [ binary "/" $ BinOpExpr DivOp
    , binary "*" $ BinOpExpr MulOp
    , binary "%" $ BinOpExpr ModOp
    ]
  , [binary "-" $ BinOpExpr SubOp, binary "+" $ BinOpExpr AddOp]
  , [binary ">>" $ BinOpExpr ShROp, binary "<<" $ BinOpExpr ShLOp]
  , [binary "&" $ BinOpExpr AndOp]
  , [binary "^" $ BinOpExpr XorOp]
  , [binary "|" $ BinOpExpr OrOp]
  , [ comparison ">=" $ BinOpExpr GeOp
    , comparison ">" $ BinOpExpr GtOp
    , comparison "<=" $ BinOpExpr LeOp
    , comparison "<" $ BinOpExpr LtOp
    , comparison "!=" $ BinOpExpr NeqOp
    , comparison "==" $ BinOpExpr EqOp
    ]
  ]

prefix ::
     Text
  -> (Annotation annot node -> node)
  -> Operator Parser (Annotation annot node)
prefix op f = Prefix ((\expr@(Annot a _) -> Annot a (f expr)) <$ symbol op)

binary ::
     Text
  -> (Annotation annot node -> Annotation annot node -> node)
  -> Operator Parser (Annotation annot node)
binary op f =
  InfixL ((\left@(Annot a _) right -> Annot a (f left right)) <$ symbol op)

comparison ::
     Text
  -> (Annotation annot node -> Annotation annot node -> node)
  -> Operator Parser (Annotation annot node)
comparison op f =
  InfixN ((\left@(Annot a _) right -> Annot a (f left right)) <$ symbol op)

infixExpr :: SourceParser Expr
infixExpr = do
  left <- binOpExpr
  n <- symbol "`" *> (Name <$> name) <* symbol "`"
  withSourcePos $ InfixExpr left n <$> binOpExpr

prefixExpr :: SourceParser Expr
prefixExpr = do
  symbol "%"
  n <- identifier
  mActuals <- optional actuals
  withSourcePos . return $ PrefixExpr n (maybeToMonoid mActuals)

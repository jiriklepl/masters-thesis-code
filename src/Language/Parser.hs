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
commaList = flip sepEndBy1 comma

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
unit = withSourcePos (Unit <$> many (withSourcePos topLevel))

topLevel :: ULocParser TopLevel
topLevel =
  choice
    [ sectionTopLevel
    , try $ TopProcedure <$> withSourcePos procedure
    , TopDecl <$> withSourcePos decl
    ]

sectionTopLevel :: ULocParser TopLevel
sectionTopLevel =
  keyword "section" *>
  (TopSection <$> stringLiteral <*> braces (many $ withSourcePos section))

section :: ULocParser Section
section =
  choice
    [ SecDecl <$> withSourcePos decl
    , secSpan
    , try $ SecProcedure <$> withSourcePos procedure
    , SecDatum <$> withSourcePos datum
    ]

decl :: ULocParser Decl
decl =
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
importDecl =
  keyword "import" *> (ImportDecl <$> commaList (withSourcePos import_)) <*
  semicolon

exportDecl :: ULocParser Decl
exportDecl =
  keyword "export" *> (ExportDecl <$> commaList (withSourcePos export)) <*
  semicolon

constDecl :: ULocParser Decl
constDecl = do
  keyword "const"
  try
    (do t <- withSourcePos typeToken
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
  keyword "typedef" *>
  (withSourcePos typeToken <**> (flip TypedefDecl <$> identifiers)) <*
  semicolon

pragmaDecl :: ULocParser Decl
pragmaDecl = do
  keyword "pragma"
  pragmaName <- identifier
  p <- braces pragma
  return $ PragmaDecl pragmaName p

targetDecl :: ULocParser Decl
targetDecl =
  keyword "target" *> (TargetDecl <$> many (withSourcePos targetDirective)) <*
  semicolon

targetDirective :: ULocParser TargetDirective
targetDirective =
  choice
    [ memSizeDirective
    , byteOrderDirective
    , pointerSizeDirective
    , wordSizeDirective
    ]

registerDecl :: ULocParser Decl
registerDecl =
  ((RegDecl . (Invariant <$) <$> optional (keyword "invariant")) <*>
   withSourcePos registers) <*
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

procedure :: ULocParser Procedure
procedure = do
  conv <- optional convention
  procName <- identifier
  formals <- parens $ withSourcePos formal `sepEndBy` comma -- TODO: discuss later
  procBody <- withSourcePos $ braces body
  return $ Procedure conv procName formals procBody

formal :: ULocParser Formal
formal = do
  k <- optional kind
  invar <- optional (keyword "invariant")
  t <- withSourcePos typeToken
  Formal k (Invariant <$ invar) t <$> identifier

actual :: ULocParser Actual
actual = (Actual <$> optional kind) <*> expression

convention :: Parser Conv
convention = keyword "foreign" >> Foreign <$> stringLiteral

import_ :: ULocParser Import
import_ = Import <$> optional (stringLiteral <* keyword "as") <*> identifier

export :: ULocParser Export
export = Export <$> identifier <*> optional (keyword "as" *> stringLiteral)

body :: ULocParser Body
body = Body <$> many (withSourcePos bodyItem)

bodyItem :: ULocParser BodyItem
bodyItem =
  BodyStackDecl <$> withSourcePos stackDecl <|> BodyStmt <$> withSourcePos stmt

secSpan :: ULocParser Section
secSpan = do
  left <- expression
  right <- expression
  sections <- many (withSourcePos section)
  return $ SecSpan left right sections

datum :: ULocParser Datum
datum = choice [alignDatum, try labelDatum, justDatum]

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
  Datum <$> withSourcePos typeToken <*> optional (withSourcePos size) <*>
  optional (withSourcePos init_) <*
  semicolon

init_ :: ULocParser Init
init_ = choice [stringInit, string16Init, initList]

initList :: ULocParser Init
initList = braces $ ExprInit <$> commaList expression

stringInit :: ULocParser Init
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser Init
string16Init = do
  keyword "unicode"
  str <- parens stringLiteral
  return $ Str16Init (String16 str) -- TODO: 16bit strings not yet implemented

size :: ULocParser Size
size = brackets $ Size <$> optional expression

registers :: ULocParser Registers
registers = do
  k <- optional kind
  t <- withSourcePos typeToken
  nvals <-
    commaList
      (do n <- identifier
          val <-
            optional $ do
              symbol "="
              stringLiteral
          return (n, val))
  return $ Registers k t nvals

typeToken :: ULocParser Type
typeToken = bitsType <|> nameType

bitsType :: ULocParser Type
bitsType = lexeme $ string "bits" >> TBits <$> L.decimal

nameType :: ULocParser Type
nameType = TName <$> identifier

kind :: Parser Kind
kind = Kind <$> stringLiteral

pragma :: Parser a
pragma = undefined -- TODO pragmas not yet specified and with no explanation of functionality

stackDecl :: ULocParser StackDecl
stackDecl =
  keyword "stackdata" *> braces (StackDecl <$> many (withSourcePos datum))

stmt :: ULocParser Stmt
stmt =
  choice
    [ emptyStmt
    , ifStmt
    , try switchStmt
    , spanStmt
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
  ifBody <- withSourcePos $ braces body
  elseBody <- optional $ keyword "else" >> braces (withSourcePos body)
  return $ IfStmt expr ifBody elseBody

switchStmt :: ULocParser Stmt
switchStmt =
  keyword "switch" *>
  (expression <**> (flip SwitchStmt <$> braces (many (withSourcePos arm))))

spanStmt :: ULocParser Stmt
spanStmt = do
  keyword "span"
  lExpr <- expression
  rExpr <- expression
  b <- braces (withSourcePos body)
  return $ SpanStmt lExpr rExpr b

assignStmt :: ULocParser Stmt
assignStmt =
  liftA2
    AssignStmt
    (commaList (withSourcePos lvalue) <* symbol "=")
    (commaList expression <* semicolon)

primOpStmt :: ULocParser Stmt
primOpStmt = do
  lName <- identifier
  symbol "="
  symbol "%%"
  rName <- identifier
  mActuals <- optional . parens $ sepEndBy (withSourcePos actual) comma
  flows <- many (withSourcePos flow)
  semicolon
  return $ PrimOpStmt lName rName (fromMaybe [] mActuals) flows

callStmt :: ULocParser Stmt
callStmt = do
  mKindNames <- optional kindedNames <* symbol "="
  mConv <- optional convention
  expr <- expression
  actuals <- parens $ sepEndBy (withSourcePos actual) comma
  mTargs <- optional (withSourcePos targets)
  annots <- many (Left <$> withSourcePos flow <|> Right <$> withSourcePos alias)
  semicolon
  return $ CallStmt (fromMaybe [] mKindNames) mConv expr actuals mTargs annots

jumpStmt :: ULocParser Stmt
jumpStmt = do
  mConv <- optional convention
  keyword "jump"
  expr <- expression
  mActuals <- optional . parens $ sepEndBy (withSourcePos actual) comma
  mTargs <- optional (withSourcePos targets)
  semicolon
  return $ JumpStmt mConv expr (fromMaybe [] mActuals) mTargs

returnStmt :: ULocParser Stmt
returnStmt = do
  mConv <- optional convention
  keyword "return"
  mExprs <- optional . angles $ liftA2 (,) expression expression
  mActuals <- optional . parens $ sepEndBy (withSourcePos actual) comma
  semicolon
  return $ ReturnStmt mConv mExprs (fromMaybe [] mActuals)

lvalue :: ULocParser LValue
lvalue = try lvRef <|> lvName

lvRef :: ULocParser LValue
lvRef = do
  t <- withSourcePos typeToken
  (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
  return $ LVRef t expr mAsserts

lvName :: ULocParser LValue
lvName = LVName <$> identifier

assertions :: Parser (Annot SourcePos Asserts)
assertions = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser Asserts
alignAssert =
  liftA2
    AlignAssert
    (keyword "aligned" *> (fst <$> integer))
    (fromMaybe [] <$> optional (keyword "in" *> sepBy1 identifier comma))

inAssert :: ULocParser Asserts
inAssert =
  liftA2
    InAssert
    (keyword "in" *> sepBy1 identifier comma)
    (optional (keyword "aligned" *> (fst <$> integer)))

labelStmt :: ULocParser Stmt
labelStmt = LabelStmt <$> identifier <* symbol ":"

contStmt :: ULocParser Stmt
contStmt = do
  keyword "continuation"
  n <- identifier
  params <- parens $ optional kindedNames
  symbol ":"
  return $ ContStmt n (fromMaybe [] params)

gotoStmt :: ULocParser Stmt
gotoStmt = do
  keyword "goto"
  expr <- expression
  mTargets <- optional (withSourcePos targets)
  semicolon
  return $ GotoStmt expr mTargets

cutToStmt :: ULocParser Stmt
cutToStmt = do
  keyword "cut"
  keyword "to"
  expr <- expression
  actuals <- parens $ sepEndBy (withSourcePos actual) comma
  mFlow <- many (withSourcePos flow)
  semicolon
  return $ CutToStmt expr actuals mFlow

kindedNames :: Parser [Annot SourcePos KindName]
kindedNames =
  commaList
    (do k <- optional kind
        pos <- getSourcePos
        Annot pos . KindName k <$> identifier)

arm :: ULocParser Arm
arm =
  keyword "case" *> (Arm <$> commaList (withSourcePos range) <* symbol ":") <*>
  withSourcePos (braces body)

range :: ULocParser Range
range = expression <**> (flip Range <$> optional (symbol ".." *> expression))

flow :: ULocParser Flow
flow = alsoFlow <|> neverReturns

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

alias :: ULocParser Alias
alias = readsAlias <|> writesAlias

readsAlias :: ULocParser Alias
readsAlias = keyword "reads" *> (Reads <$> identifiers)

writesAlias :: ULocParser Alias
writesAlias = keyword "writes" *> (Writes <$> identifiers)

targets :: ULocParser Targets
targets = keyword "targets" *> (Targets <$> identifiers)

expression :: SourceParser Expr
expression = try infixExpr <|> binOpExpr

simpleExpr :: SourceParser Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

litExpr :: SourceParser Expr
litExpr = do
  literal <- choice [intExpr, floatExpr, charExpr]
  mType <- optional $ symbol "::" *> withSourcePos typeToken
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
    t <- withSourcePos typeToken
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ RefExpr t expr mAsserts

parExpr :: SourceParser Expr
parExpr =
  withSourcePos $ do
    pExpr <- parens expression
    return $ ParExpr pExpr

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
  mActuals <- optional . parens $ sepEndBy (withSourcePos actual) comma
  withSourcePos . return $ PrefixExpr n (fromMaybe [] mActuals)

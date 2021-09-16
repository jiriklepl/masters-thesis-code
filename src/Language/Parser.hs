{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Language.Parser where

import Control.Applicative hiding (many)
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Prelude
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Foldable
import Language.CMM

type Parser = Parsec Void Text

type SourceParser a = Parser (Annot SourcePos a)

type ULocParser a = Parser (a SourcePos)

maybeToMonoid :: Monoid a => Maybe a -> a
maybeToMonoid = fromMaybe mempty

liftA4 ::
     Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
liftA5 f a b c d e = liftA4 f a b c d <*> e

liftA6 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g -> h)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
  -> f h
liftA6 f a b c d e g = liftA5 f a b c d e <*> g

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ = void . L.symbol sc

keyword :: Text -> Parser ()
keyword k = void . lexeme $ try (string k <* notFollowedBy alphaNumChar)

keywords :: [Text] -> Parser ()
keywords = traverse_ keyword

stringLiteral :: Parser Text
stringLiteral =
  lexeme $ char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))

charLiteral :: Parser Char
charLiteral = lexeme $ char '\'' *> L.charLiteral <* char '\''

name :: Parser Text
name =
  T.pack <$>
  liftA2 (:) (letterChar <|> otherChars) (many (alphaNumChar <|> otherChars))
  where
    otherChars = oneOf ['_', '.', '$', '@']

withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 Annot getSourcePos

identifier :: Parser Name
identifier = Name <$> lexeme name

identifiers :: Parser [Name]
identifiers = commaList identifier

braces :: Parser a -> Parser a
braces = between (symbol_ "{") (symbol_ "}")

parens :: Parser a -> Parser a
parens = between (symbol_ "(") (symbol_ ")")

brackets :: Parser a -> Parser a
brackets = between (symbol_ "[") (symbol_ "]")

angles :: Parser a -> Parser a
angles = between (symbol_ "<") (symbol_ ">")

comma :: Parser ()
comma = symbol_ ","

semicolon :: Parser ()
semicolon = symbol_ ";"

eqSign :: Parser ()
eqSign = symbol_ "="

colon :: Parser ()
colon = symbol_ ":"

commaList :: Parser a -> Parser [a]
commaList = (`sepEndBy1` comma)

int :: Parser (Int, Bool)
int = signedInt <|> unsignedInt

signedInt :: Parser (Int, Bool)
signedInt = lexeme (char '-' *> ((, True) . negate <$> L.decimal))

unsignedInt :: Parser (Int, Bool)
unsignedInt =
  lexeme $
  (char '0' *>
   ((oneOf ['x', 'X'] *> ((, False) <$> L.hexadecimal)) <|>
    (, False) <$> L.octal <|>
    (0, ) <$> unsignedSpec)) <|>
  decimal

decimal :: Parser (Int, Bool)
decimal = liftA2 (,) L.decimal unsignedSpec

unsignedSpec :: Parser Bool
unsignedSpec = isNothing <$> optional (oneOf ['u', 'U'])

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
  keyword "section" *> liftA2 TopSection stringLiteral (braces (many section))

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
constDecl =
  keyword "const" *>
  liftA2
    (uncurry ConstDecl)
    (try (liftA2 (,) (optional typeToken) identifier) <|>
     (Nothing, ) <$> identifier)
    (eqSign *> expression) <*
  semicolon

typedefDecl :: ULocParser Decl
typedefDecl =
  keyword "typedef" *> liftA2 TypedefDecl typeToken identifiers <* semicolon

pragmaDecl :: ULocParser Decl
pragmaDecl = keyword "pragma" *> liftA2 PragmaDecl identifier (braces pragma)

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
registerDecl = liftA2 RegDecl invariant registers <* semicolon

memSizeDirective :: ULocParser TargetDirective
memSizeDirective = keyword "memsize" *> (MemSize . fst <$> unsignedInt)

byteOrderDirective :: ULocParser TargetDirective
byteOrderDirective = keyword "byteorder" *> (ByteOrder <$> endian)

endian :: Parser Endian
endian = choice [keyword "little" $> Little, keyword "big" $> Big]

pointerSizeDirective :: ULocParser TargetDirective
pointerSizeDirective =
  keyword "pointersize" *> (PointerSize . fst <$> unsignedInt)

wordSizeDirective :: ULocParser TargetDirective
wordSizeDirective = keyword "wordsize" *> (WordSize . fst <$> unsignedInt)

procedure :: SourceParser Procedure
procedure =
  withSourcePos $ liftA4 Procedure (optional convention) identifier formals body

formal :: SourceParser Formal
formal =
  withSourcePos $ liftA4 Formal (optional kind) invariant typeToken identifier

formals :: Parser [Annot SourcePos Formal]
formals = parens $ formal `sepEndBy` comma

invariant :: Parser Bool
invariant = try (keyword "invariant") $> True <|> pure False

actual :: SourceParser Actual
actual = withSourcePos $ liftA2 Actual (optional kind) expression

actuals :: Parser [Annot SourcePos Actual]
actuals = parens $ actual `sepEndBy` comma

convention :: Parser Conv
convention = keyword "foreign" *> (Foreign <$> stringLiteral)

import_ :: SourceParser Import
import_ =
  withSourcePos $
  liftA2 Import (optional (stringLiteral <* keyword "as")) identifier

export :: SourceParser Export
export =
  withSourcePos $
  liftA2 Export identifier (optional (keyword "as" *> stringLiteral))

body :: SourceParser Body
body = withSourcePos . braces $ Body <$> many bodyItem

bodyItem :: SourceParser BodyItem
bodyItem = withSourcePos $ BodyStackDecl <$> stackDecl <|> BodyStmt <$> stmt

secSpan :: ULocParser Section
secSpan = liftA3 SecSpan expression expression (many section)

datum :: SourceParser Datum
datum = withSourcePos $ choice [try alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser Datum
alignDatum = keyword "align" *> (DatumAlign . fst <$> unsignedInt) <* semicolon

labelDatum :: ULocParser Datum
labelDatum = DatumLabel <$> identifier <* colon

justDatum :: ULocParser Datum
justDatum = liftA3 Datum typeToken (optional size) (optional init_) <* semicolon

init_ :: SourceParser Init
init_ = withSourcePos $ choice [stringInit, string16Init, initList]

initList :: ULocParser Init
initList = braces $ ExprInit <$> commaList expression

stringInit :: ULocParser Init
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser Init
string16Init = keyword "unicode" *> (Str16Init <$> parens stringLiteral)

size :: SourceParser Size
size = withSourcePos . brackets $ Size <$> optional expression

registers :: SourceParser Registers
registers =
  withSourcePos $
  liftA3
    Registers
    (optional kind)
    typeToken
    (commaList (liftA2 (,) identifier (optional $ eqSign *> stringLiteral)))

typeToken :: SourceParser Type
typeToken = withSourcePos $ bitsType <|> nameType

bitsType :: ULocParser Type
bitsType = lexeme $ string "bits" *> (TBits <$> L.decimal)

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
    ] -- TODO: this is utter BS

emptyStmt :: ULocParser Stmt
emptyStmt = semicolon $> EmptyStmt

ifStmt :: ULocParser Stmt
ifStmt =
  keyword "if" *>
  liftA3 IfStmt expression body (optional $ keyword "else" *> body)

switchStmt :: ULocParser Stmt
switchStmt =
  keyword "switch" *> liftA2 SwitchStmt expression (braces (many arm))

spanStmt :: ULocParser Stmt
spanStmt = keyword "span" *> liftA3 SpanStmt expression expression body

assignStmt :: ULocParser Stmt
assignStmt =
  AssignStmt <$>
  liftA2 zip (commaList lvalue <* eqSign) (commaList expression <* semicolon)

primOpStmt :: ULocParser Stmt
primOpStmt =
  liftA4
    PrimOpStmt
    (identifier <* eqSign)
    (symbol "%%" *> identifier)
    (maybeToMonoid <$> optional actuals)
    (many flow) <*
  semicolon

callStmt :: ULocParser Stmt
callStmt =
  liftA6
    CallStmt
    (maybeToMonoid <$> (optional kindedNames <* eqSign))
    (optional convention)
    expression
    actuals
    (optional targets)
    (many (Left <$> flow <|> Right <$> alias)) <*
  semicolon

jumpStmt :: ULocParser Stmt
jumpStmt =
  liftA4
    JumpStmt
    (optional convention <* keyword "jump")
    expression
    (maybeToMonoid <$> optional actuals)
    (optional targets) <*
  semicolon

returnStmt :: ULocParser Stmt
returnStmt =
  liftA3
    ReturnStmt
    (optional convention <* keyword "return")
    (optional . angles $
     liftA2 (,) (restrictedExpr <* symbol "/") restrictedExpr)
    (maybeToMonoid <$> optional actuals) <*
  semicolon

lvalue :: SourceParser LValue
lvalue = withSourcePos $ try lvRef <|> lvName

lvRef :: ULocParser LValue
lvRef =
  liftA3 LVRef typeToken (symbol "[" *> expression) (optional assertions) <*
  symbol "]"

lvName :: ULocParser LValue
lvName = LVName <$> identifier

assertions :: Parser (Annot SourcePos Asserts)
assertions = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser Asserts
alignAssert =
  liftA2
    AlignAssert
    (keyword "aligned" *> (fst <$> int))
    (maybeToMonoid <$> optional (keyword "in" *> (identifier `sepBy1` comma)))

inAssert :: ULocParser Asserts
inAssert =
  liftA2
    InAssert
    (keyword "in" *> (identifier `sepBy1` comma))
    (optional (keyword "aligned" *> (fst <$> int)))

labelStmt :: ULocParser Stmt
labelStmt = LabelStmt <$> identifier <* colon

contStmt :: ULocParser Stmt
contStmt =
  keyword "continuation" *>
  liftA2 ContStmt identifier (maybeToMonoid <$> parens (optional kindedNames)) <*
  colon

gotoStmt :: ULocParser Stmt
gotoStmt =
  keyword "goto" *> liftA2 GotoStmt expression (optional targets) <* semicolon

cutToStmt :: ULocParser Stmt
cutToStmt =
  keywords ["cut", "to"] *> liftA3 CutToStmt expression actuals (many flow) <*
  semicolon

kindedNames :: Parser [Annot SourcePos KindName]
kindedNames =
  commaList . withSourcePos $ liftA2 KindName (optional kind) identifier

arm :: SourceParser Arm
arm =
  withSourcePos $ keyword "case" *> liftA2 Arm (commaList range <* colon) body

range :: SourceParser Range
range =
  withSourcePos $ liftA2 Range expression (optional (symbol ".." *> expression))

flow :: SourceParser Flow
flow = withSourcePos $ alsoFlow <|> neverReturns

alsoFlow :: ULocParser Flow
alsoFlow =
  keyword "also" *>
  choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

alsoCutsTo :: ULocParser Flow
alsoCutsTo = keywords ["cuts", "to"] *> (AlsoCutsTo <$> identifiers)

alsoUnwindsTo :: ULocParser Flow
alsoUnwindsTo = keywords ["unwinds", "to"] *> (AlsoUnwindsTo <$> identifiers)

alsoReturnsTo :: ULocParser Flow
alsoReturnsTo = keywords ["returns", "to"] *> (AlsoReturnsTo <$> identifiers)

alsoAborts :: ULocParser Flow
alsoAborts = keyword "aborts" *> optional comma $> AlsoAborts

neverReturns :: ULocParser Flow
neverReturns = keywords ["never", "returns"] *> optional comma $> NeverReturns

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

restrictedExpr :: SourceParser Expr
restrictedExpr = choice [litExpr, parExpr, nameExpr]

litExpr :: SourceParser Expr
litExpr =
  withSourcePos $
  liftA2
    LitExpr
    (choice [intExpr, floatExpr, charExpr])
    (optional $ symbol "::" *> typeToken)

intExpr :: SourceParser Lit
intExpr = withSourcePos $ LitInt . fst <$> int

floatExpr :: SourceParser Lit
floatExpr = intExpr -- TODO: implement for floats

charExpr :: SourceParser Lit
charExpr = withSourcePos $ LitChar <$> charLiteral

nameExpr :: SourceParser Expr
nameExpr = withSourcePos $ LVExpr <$> withSourcePos lvName

refExpr :: SourceParser Expr
refExpr = withSourcePos $ LVExpr <$> withSourcePos lvRef

parExpr :: SourceParser Expr
parExpr = withSourcePos $ ParExpr <$> parens expression

binOpExpr :: SourceParser Expr
binOpExpr = try cmpExpr <|> orExpr

cmpExpr :: SourceParser Expr
cmpExpr =
  withSourcePos $
  flip
    (\case
       ">=" -> BinOpExpr GeOp
       ">" -> BinOpExpr GtOp
       "<=" -> BinOpExpr LeOp
       "<" -> BinOpExpr LtOp
       "!=" -> BinOpExpr NeqOp
       _ -> BinOpExpr EqOp) <$>
  orExpr <*>
  choice (symbol <$> [">=", ">", "<=", "<", "!=", "=="]) <*>
  orExpr

-- TODO: change right associativity to left

orExpr :: SourceParser Expr
orExpr =
  try
    (do left <- xorExpr
        let annot = takeAnnot left
        symbol_ "|"
        right <- orExpr
        return . Annot annot $ BinOpExpr OrOp left right) <|>
  xorExpr

xorExpr :: SourceParser Expr
xorExpr =
  try
    (do left <- andExpr
        let annot = takeAnnot left
        symbol_ "^"
        right <- xorExpr
        return . Annot annot $ BinOpExpr XorOp left right) <|>
  andExpr

andExpr :: SourceParser Expr
andExpr =
  try
    (do left <- shExpr
        let annot = takeAnnot left
        symbol_ "&"
        right <- andExpr
        return . Annot annot $ BinOpExpr AndOp left right) <|>
  shExpr

shExpr :: SourceParser Expr
shExpr =
  try
    (do left <- addExpr
        let annot = takeAnnot left
        middle <- choice [symbol ">>", symbol "<<"]
        right <- shExpr
        return . Annot annot $
          (\case
             ">>" -> BinOpExpr ShROp left right
             _ -> BinOpExpr ShLOp left right)
            middle) <|>
  addExpr

addExpr :: SourceParser Expr
addExpr =
  try
    (do left <- mulExpr
        let annot = takeAnnot left
        middle <- choice [symbol "-", symbol "+"]
        right <- addExpr
        return . Annot annot $
          (\case
             "-" -> BinOpExpr SubOp left right
             _ -> BinOpExpr AddOp left right)
            middle) <|>
  mulExpr

mulExpr :: SourceParser Expr
mulExpr =
  try
    (do left <- negExpr
        let annot = takeAnnot left
        middle <- choice [symbol "/", symbol "*", symbol "%"]
        right <- mulExpr
        return . Annot annot $
          (\case
             "/" -> BinOpExpr DivOp left right
             "*" -> BinOpExpr MulOp left right
             _ -> BinOpExpr ModOp left right)
            middle) <|>
  negExpr

negExpr :: SourceParser Expr
negExpr =
  withSourcePos
    ((symbol "-" *> (NegExpr <$> negExpr)) <|>
     (symbol "~" *> (ComExpr <$> negExpr))) <|>
  simpleExpr

infixExpr :: SourceParser Expr
infixExpr =
  withSourcePos $
  liftA3
    InfixExpr
    binOpExpr
    (symbol "`" *> (Name <$> name) <* symbol "`")
    binOpExpr

prefixExpr :: SourceParser Expr
prefixExpr =
  withSourcePos $
  symbol "%" *>
  liftA2 PrefixExpr identifier (maybeToMonoid <$> optional actuals)

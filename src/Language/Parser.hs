{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Language.AST
import Language.AST.Utils

type Parser = Parsec Void Text

type SourceParser a = Parser (Annot a SourcePos)

type ULocParser a = Parser (a SourcePos)

optionalL :: Parser [a] -> Parser [a]
optionalL = (fromMaybe [] <$>) . optional

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

infixl 4 <*<

(<*<) :: Parser (b -> c) -> Parser (a -> b) -> Parser (a -> c)
(<*<) = liftA2 (.)

infixl 4 >*>

(>*>) :: Parser (a -> b) -> Parser (b -> c) -> Parser (a -> c)
(>*>) = liftA2 (flip (.))

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

stringLiteral :: Parser StrLit
stringLiteral =
  lexeme $ char '"' *> (StrLit . T.pack <$> manyTill L.charLiteral (char '"'))

charLiteral :: Parser Char
charLiteral = lexeme $ char '\'' *> L.charLiteral <* char '\''

name :: Parser Text
name =
  T.pack <$>
  liftA2 (:) (letterChar <|> otherChars) (many (alphaNumChar <|> otherChars))
  where
    otherChars = oneOf ['_', '.', '$', '@']

withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 withAnnot getSourcePos

identifier :: ULocParser Name
identifier = Name <$> lexeme name

identifiers :: Parser [Annot Name SourcePos]
identifiers = commaList $ withSourcePos identifier

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

float :: Parser Float
float = lexeme L.float

program :: SourceParser Unit
program = sc *> unit <* eof

-- | Parses the whole 'Unit'
unit :: SourceParser Unit
unit = withSourcePos $ Unit <$> many topLevel

topLevel :: SourceParser TopLevel
topLevel =
  withSourcePos $
  choice [sectionTopLevel, TopDecl <$> decl, TopProcedure <$> try procedure]

sectionTopLevel :: ULocParser TopLevel
sectionTopLevel =
  keyword "section" *> liftA2 TopSection stringLiteral (braces $ many section)

section :: SourceParser Section
section =
  withSourcePos $
  choice
    [ SecDecl <$> decl
    , secSpan
    , SecProcedure <$> try procedure
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
    , try registerDecl
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
    (eqSign *> expr) <*
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
formal = withSourcePos $ liftA4 Formal mKind invariant typeToken identifier

formals :: Parser [Annot Formal SourcePos]
formals = parens $ formal `sepEndBy` comma

invariant :: Parser Bool
invariant = try (keyword "invariant") $> True <|> pure False

actual :: SourceParser Actual
actual = withSourcePos $ liftA2 Actual mKind expr

actuals :: Parser [Annot Actual SourcePos]
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
bodyItem =
  withSourcePos $
  BodyDecl <$> try decl <|> BodyStackDecl <$> stackDecl <|> BodyStmt <$> stmt

secSpan :: ULocParser Section
secSpan = keyword "span" *> liftA3 SecSpan expr expr (braces $ many section)

datum :: SourceParser Datum
datum = withSourcePos $ choice [alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser Datum
alignDatum = keyword "align" *> (DatumAlign . fst <$> unsignedInt) <* semicolon

labelDatum :: ULocParser Datum
labelDatum = DatumLabel <$> identifier <* colon

justDatum :: ULocParser Datum
justDatum = liftA3 Datum typeToken (optional size) (optional init_) <* semicolon

init_ :: SourceParser Init
init_ = withSourcePos $ choice [stringInit, string16Init, initList]

initList :: ULocParser Init
initList = braces $ ExprInit <$> commaList expr

stringInit :: ULocParser Init
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser Init
string16Init = keyword "unicode" *> (Str16Init <$> parens stringLiteral)

size :: SourceParser Size
size = withSourcePos . brackets $ Size <$> optional expr

registers :: SourceParser Registers
registers =
  withSourcePos $
  liftA3
    Registers
    mKind
    typeToken
    (commaList
       (liftA2
          (,)
          (withSourcePos identifier)
          (optional $ eqSign *> stringLiteral)))

typeToken :: SourceParser Type
typeToken = withSourcePos $ bitsType <|> nameType

bitsType :: ULocParser Type
bitsType = lexeme $ string "bits" *> (TBits <$> L.decimal)

nameType :: ULocParser Type
nameType = TName <$> identifier

mKind :: Parser (Maybe Kind)
mKind = optional $ Kind <$> stringLiteral

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
    , try switchStmt
    , spanStmt
    , try jumpStmt
    , try returnStmt
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
  keyword "if" *> liftA3 IfStmt expr body (optional $ keyword "else" *> body)

switchStmt :: ULocParser Stmt
switchStmt = keyword "switch" *> liftA2 SwitchStmt expr (braces (many arm))

spanStmt :: ULocParser Stmt
spanStmt = keyword "span" *> liftA3 SpanStmt expr expr body

assignStmt :: ULocParser Stmt
assignStmt =
  liftA2 AssignStmt (commaList lvalue <* eqSign) (commaList expr) <* semicolon

primOpStmt :: ULocParser Stmt
primOpStmt =
  liftA4
    PrimOpStmt
    (identifier <* eqSign)
    (symbol "%%" *> identifier)
    (optionalL actuals)
    (many flow) <*
  semicolon

callStmt :: ULocParser Stmt
callStmt =
  liftA6
    CallStmt
    (optionalL kindedNames <* eqSign)
    (optional convention)
    expr
    actuals
    (optional targets)
    (many . withSourcePos $ FlowAnnot <$> flow <|> AliasAnnot <$> alias) <*
  semicolon

jumpStmt :: ULocParser Stmt
jumpStmt =
  liftA4
    JumpStmt
    (optional convention <* keyword "jump")
    expr
    (optionalL actuals)
    (optional targets) <*
  semicolon

returnStmt :: ULocParser Stmt
returnStmt =
  liftA3
    ReturnStmt
    (optional convention <* keyword "return")
    (optional . angles $
     liftA2 (,) (restrictedExpr <* symbol "/") restrictedExpr)
    (optionalL actuals) <*
  semicolon

lvalue :: SourceParser LValue
lvalue = withSourcePos $ try lvRef <|> lvName

lvRef :: ULocParser LValue
lvRef =
  liftA3 LVRef typeToken (symbol "[" *> expr) (optional asserts) <* symbol "]"

lvName :: ULocParser LValue
lvName = LVName <$> identifier

asserts :: Parser (Annot Asserts SourcePos)
asserts = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser Asserts
alignAssert =
  liftA2
    AlignAssert
    (keyword "aligned" *> (fst <$> int))
    (optionalL $ keyword "in" *> (withSourcePos identifier `sepBy1` comma))

inAssert :: ULocParser Asserts
inAssert =
  liftA2
    InAssert
    (keyword "in" *> (withSourcePos identifier `sepBy1` comma))
    (optional (keyword "aligned" *> (fst <$> int)))

labelStmt :: ULocParser Stmt
labelStmt = LabelStmt <$> identifier <* colon

contStmt :: ULocParser Stmt
contStmt =
  keyword "continuation" *>
  liftA2 ContStmt identifier (parens $ optionalL kindedNames) <*
  colon

gotoStmt :: ULocParser Stmt
gotoStmt =
  keyword "goto" *> liftA2 GotoStmt expr (optional targets) <* semicolon

cutToStmt :: ULocParser Stmt
cutToStmt =
  keywords ["cut", "to"] *> liftA3 CutToStmt expr actuals (many flow) <*
  semicolon

kindedNames :: Parser [Annot KindName SourcePos]
kindedNames = commaList . withSourcePos $ liftA2 KindName mKind identifier

arm :: SourceParser Arm
arm =
  withSourcePos $ keyword "case" *> liftA2 Arm (commaList range <* colon) body

range :: SourceParser Range
range = withSourcePos $ liftA2 Range expr (optional (symbol ".." *> expr))

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

expr :: SourceParser Expr
expr = infixExpr

simpleExpr :: SourceParser Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

restrictedExpr :: SourceParser Expr
restrictedExpr = choice [litExpr, parExpr, nameExpr]

litExpr :: SourceParser Expr
litExpr =
  withSourcePos $
  liftA2
    LitExpr
    (withSourcePos $ choice [charExpr, try floatExpr, intExpr])
    (optional $ symbol "::" *> typeToken)

intExpr :: ULocParser Lit
intExpr = LitInt . fst <$> int

floatExpr :: ULocParser Lit
floatExpr = LitFloat <$> float

charExpr :: ULocParser Lit
charExpr = LitChar <$> charLiteral

nameExpr :: SourceParser Expr
nameExpr = withSourcePos $ LVExpr <$> withSourcePos lvName

refExpr :: SourceParser Expr
refExpr = withSourcePos $ LVExpr <$> withSourcePos lvRef

parExpr :: SourceParser Expr
parExpr = withSourcePos $ ParExpr <$> parens expr

class OpImpl a where
  opImplL :: a -> SourceParser Expr -> SourceParser Expr
  opImplL x next = next <**> opRestImplL x next
  opImplN :: a -> SourceParser Expr -> SourceParser Expr
  opImplN x next = next <**> opRestImplN x next
  opRestImplL ::
       a
    -> SourceParser Expr
    -> Parser (Annot Expr SourcePos -> Annot Expr SourcePos)
  opRestImplL x next =
    withAnnot <$> getSourcePos <*< opRestInner x next >*> opRestImplL x next <|>
    pure id
  opRestImplN ::
       a
    -> SourceParser Expr
    -> Parser (Annot Expr SourcePos -> Annot Expr SourcePos)
  opRestImplN x next =
    withAnnot <$> getSourcePos <*< opRestInner x next <|> pure id
  opRestInner ::
       a -> SourceParser Expr -> Parser (Annot Expr SourcePos -> Expr SourcePos)

instance OpImpl (Op, Text) where
  opRestInner (op, str) next = flip (BinOpExpr op) <$> (symbol str *> next)

instance OpImpl x => OpImpl [x] where
  opRestInner xs next = foldl1 (<|>) (flip opRestInner next <$> xs)

instance OpImpl Text where
  opRestInner "`" next =
    symbol "`" *> (flip . InfixExpr . Name <$> name) <* symbol "`" <*> next
  opRestInner _ _ = undefined

infixExpr :: SourceParser Expr
infixExpr = opImplN ("`" :: Text) cmpExpr

cmpExpr :: SourceParser Expr
cmpExpr =
  opImplN
    [ (GeOp, ">=" :: Text)
    , (GtOp, ">")
    , (LeOp, "<=")
    , (LtOp, "<")
    , (NeqOp, "!=")
    , (EqOp, "==")
    ]
    orExpr

orExpr :: SourceParser Expr
orExpr = opImplL (OrOp, "|" :: Text) xorExpr

xorExpr :: SourceParser Expr
xorExpr = opImplL (XorOp, "^" :: Text) andExpr

andExpr :: SourceParser Expr
andExpr = opImplL (XorOp, "&" :: Text) shExpr

shExpr :: SourceParser Expr
shExpr = opImplL [(ShLOp, "<<" :: Text), (ShROp, ">>")] addExpr

addExpr :: SourceParser Expr
addExpr = opImplL [(AddOp, "+" :: Text), (SubOp, "-")] mulExpr

mulExpr :: SourceParser Expr
mulExpr = opImplL [(DivOp, "/" :: Text), (MulOp, "*"), (ModOp, "%")] negExpr

negExpr :: SourceParser Expr
negExpr =
  withSourcePos (symbol "-" *> (NegExpr <$> negExpr)) <|>
  withSourcePos (symbol "~" *> (ComExpr <$> negExpr)) <|>
  simpleExpr

prefixExpr :: SourceParser Expr
prefixExpr =
  withSourcePos $ symbol "%" *> liftA2 PrefixExpr identifier (optionalL actuals)

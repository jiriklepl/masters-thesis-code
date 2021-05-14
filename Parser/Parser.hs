{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative hiding (many)
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import Language

-- TODO: add ;s and ,s where you forgot
-- TODO: rename refExpr to derefExpr?

data ParserState = ParserState
    { dummy1 :: [String]
    , dummy2 :: [String]
    }
    deriving (Show)

initState :: ParserState
initState = ParserState { dummy1 = [], dummy2 = [] }

type StateMonad = State ParserState
type Parser a = ParsecT Void Text StateMonad a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword = lexeme . string

stringLiteral :: Parser Text
stringLiteral = lexeme $ char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

charLiteral :: Parser Char
charLiteral = lexeme $ char '\'' *> L.charLiteral <* char '\''

name :: Parser Text
name = do
    n <- letterChar <|> otherChars
    ame <- T.pack <$> many (alphaNumChar <|> otherChars)
    return $ n `T.cons` ame
    where otherChars = oneOf ['_','.','$','@']

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

integer :: Parser (Int, Bool)
integer = lexeme $
    do char '0' >> oneOf ['x','X'] >> (, False) <$> L.hexadecimal
    <|> do char '0' >> (, False) <$> L.octal
    <|> do decimal

decimal :: Parser (Int, Bool)
decimal = do
    nums <- L.decimal
    unsigned <- optional $ oneOf ['u','U']
    return (nums, isNothing unsigned)

-- | Parses the whole 'Unit'
unit :: Parser (Unit SourcePos)
unit = do
    pos <- getSourcePos
    topLevels <- many topLevel
    return $ Unit topLevels pos

topLevel :: Parser (TopLevel SourcePos)
topLevel = sectionTopLevel <|> TopDecl <$> decl <|> TopProcedure <$> procedure

sectionTopLevel :: Parser (TopLevel SourcePos)
sectionTopLevel = do
    keyword "section"
    pos <- getSourcePos
    name <- stringLiteral
    sections <- braces $ many section
    return $ TopSection name sections pos

section :: Parser (Section SourcePos)
section = SecDecl <$> decl <|> secSpan <|> try (SecProcedure <$> procedure) <|> SecDatum <$> datum

decl :: Parser (Decl SourcePos)
decl = importDecl <|> exportDecl <|> constDecl <|> typedefDecl <|> pragmaDecl <|> targetDecl <|> registerDecl

importDecl :: Parser (Decl SourcePos)
importDecl = do
    keyword "import"
    pos <- getSourcePos
    imports <- sepBy1 import_ (symbol ",")
    return $ ImportDecl imports pos

exportDecl :: Parser (Decl SourcePos)
exportDecl = do
    keyword "export"
    pos <- getSourcePos
    exports <- sepBy1 export (symbol ",")
    return $ ExportDecl exports pos

constDecl :: Parser (Decl SourcePos)
constDecl = do
    keyword "const"
    pos <- getSourcePos
    try (do
        t <- type_
        declName <- Name <$> name
        symbol "="
        expr <- expression
        return $ ConstDecl (Just t) declName expr pos)
      <|> do
        declName <- Name <$> name
        symbol "="
        expr <- expression
        return $ ConstDecl Nothing declName expr pos

typedefDecl :: Parser (Decl SourcePos)
typedefDecl = do
    keyword "typedef"
    pos <- getSourcePos
    t <- type_
    names <- sepBy1 (Name <$> name) (symbol ",")
    return $ TypedefDecl t names pos

pragmaDecl :: Parser (Decl SourcePos)
pragmaDecl = do
    keyword "pragma"
    pos <- getSourcePos
    pragmaName <- Name <$> name
    p <- braces pragma
    return $ PragmaDecl pragmaName p pos

targetDecl :: Parser (Decl SourcePos)
targetDecl = do
    keyword "target"
    pos <- getSourcePos
    directives <- many targetDirective
    return $ TargetDecl directives pos

targetDirective :: Parser (TargetDirective SourcePos)
targetDirective = memSizeDirective <|> byteOrderDirective <|> pointerSizeDirective <|> wordSizeDirective

memSizeDirective :: Parser (TargetDirective SourcePos)
memSizeDirective = do
    keyword "memsize"
    pos <- getSourcePos
    (memSize, False) <- integer
    return $ MemSize memSize pos

byteOrderDirective :: Parser (TargetDirective SourcePos)
byteOrderDirective = do
    keyword "byteorder"
    pos <- getSourcePos
    e <- endian
    return $ ByteOrder e pos

endian :: Parser Endian
endian = do keyword "little" >> return Little
    <|> do keyword "big" >> return Big

pointerSizeDirective :: Parser (TargetDirective SourcePos)
pointerSizeDirective = do
    keyword "pointersize"
    pos <- getSourcePos
    (pointerSize, False) <- integer
    return $ PointerSize pointerSize pos

wordSizeDirective :: Parser (TargetDirective SourcePos)
wordSizeDirective = do
    keyword "wordsize"
    pos <- getSourcePos
    (wordSize, False) <- integer
    return $ PointerSize wordSize pos

procedure :: Parser (Procedure SourcePos)
procedure = do
    conv <- optional convention
    pos <- getSourcePos
    procName <- Name <$> name
    formals <- parens $ sepBy formal (symbol ",")
    procBody <- braces body
    return $ Procedure conv procName formals procBody pos

formal :: Parser (Formal SourcePos)
formal = do
    k <- optional kind
    invar <- optional (keyword "invariant")
    t <- type_
    pos <- getSourcePos
    formalName <- Name <$> name
    return $ Formal k (Invariant <$ invar) t formalName pos

actual :: Parser (Actual SourcePos)
actual = do
    k <- optional kind
    pos <- getSourcePos
    expr <- expression
    return $ Actual k expr pos

convention :: Parser Conv
convention = keyword "foreign" >> Foreign <$> stringLiteral

import_ :: Parser (Import SourcePos)
import_ = do
    as <-  optional (stringLiteral >>= (\n -> keyword "as" >> return n))
    pos <- getSourcePos
    importName  <- Name <$> name
    return $ Import as importName pos

export :: Parser (Export SourcePos)
export = do
    pos <- getSourcePos
    exportName  <- Name <$> name
    as <-  optional (keyword "as" >> stringLiteral)
    return $ Export exportName as pos

body :: Parser (Body SourcePos)
body = do
    pos <- getSourcePos
    items <- many bodyItem
    return $ Body items pos

bodyItem :: Parser (BodyItem SourcePos)
bodyItem = BodyDecl <$> decl <|> BodyStackDecl <$> stackDecl <|> BodyStmt <$> stmt

secSpan :: Parser (Section SourcePos)
secSpan = getSourcePos <**> liftA3 SecSpan expression expression (many section)

datum :: Parser (Datum SourcePos)
datum = alignDatum <|> try labelDatum <|> justDatum

alignDatum :: Parser (Datum SourcePos)
alignDatum = do
    pos <- getSourcePos
    keyword "align"
    (align, False) <- integer
    return $ DatumAlign align pos

labelDatum :: Parser (Datum SourcePos )
labelDatum = do
    pos <- getSourcePos
    labelName <- Name <$> name
    return $ DatumLabel labelName pos

justDatum :: Parser (Datum SourcePos )
justDatum = do
    pos <- getSourcePos
    t <- type_
    s <- optional size
    i <- optional init_
    return $ Datum t s i pos

init_ :: Parser (Init SourcePos)
init_ = stringInit <|> string16Init <|> initList

initList :: Parser (Init SourcePos)
initList = braces $ do
    pos <- getSourcePos
    exprs <- sepBy1 expression (symbol ",")
    return $ ExprInit exprs pos

stringInit :: Parser (Init SourcePos)
stringInit = do
    pos <- getSourcePos
    str <- stringLiteral
    return $ StrInit str pos

string16Init :: Parser (Init SourcePos)
string16Init = do
    pos <- getSourcePos
    keyword "unicode"
    str <- parens stringLiteral
    return $ Str16Init (String16 str) pos -- TODO

size :: Parser (Size SourcePos)
size = brackets $ do
    pos <- getSourcePos
    expr <-  optional expression
    return $ Size expr pos

registerDecl :: Parser (Decl SourcePos)
registerDecl = do
    invar <- optional (keyword "invariant")
    pos <- getSourcePos
    regs <- registers
    return $ RegDecl (Invariant <$ invar) regs pos

registers :: Parser (Registers SourcePos)
registers = do
    k <- optional kind
    t <- type_
    pos <- getSourcePos
    nvals <- sepEndBy1 ( do
        n <- Name <$> name
        val <- optional $ do
            symbol "="
            stringLiteral
        return (n, val)) (symbol ",")
    return $ Registers k t nvals pos

type_ :: Parser (Type SourcePos)
type_ = bitsType <|> nameType

bitsType :: Parser (Type SourcePos)
bitsType = do
    pos <- getSourcePos
    string "bits"
    bits <- L.decimal
    return $ TBits bits pos

nameType :: Parser (Type SourcePos)
nameType = do
    pos <- getSourcePos
    n <- Name <$> name
    return $ TName n pos

kind :: Parser Kind
kind = Kind <$> stringLiteral

pragma = undefined -- TODO


stackDecl :: Parser (StackDecl SourcePos)
stackDecl = do
    keyword "stackdata"
    pos <- getSourcePos
    datums <- braces $ many datum
    return $ StackDecl datums pos

stmt :: Parser (Stmt SourcePos)
stmt = emptyStmt <|> ifStmt <|> switchStmt <|> spanStmt <|> assignStmt <|> primOpStmt <|> callStmt <|> jumpStmt <|> returnStmt <|> labelStmt <|> contStmt <|> gotoStmt <|> cutToStmt

emptyStmt :: Parser (Stmt SourcePos)
emptyStmt = do
    pos <- getSourcePos
    symbol ";"
    return $ EmptyStmt pos

ifStmt :: Parser (Stmt SourcePos)
ifStmt = do
    pos <- getSourcePos
    keyword "if"
    expr <- expression
    b <- braces body
    els <- optional $ keyword "else" >> braces body
    return $ IfStmt expr b els pos

switchStmt :: Parser (Stmt SourcePos)
switchStmt = do
    pos <- getSourcePos
    keyword "switch"
    expr <- expression
    arms <- braces $ many arm
    return $ SwitchStmt expr arms pos

spanStmt :: Parser (Stmt SourcePos)
spanStmt = do
    pos <- getSourcePos
    keyword "span"
    lExpr <- expression
    rExpr <- expression
    b <- braces body
    return $ SpanStmt lExpr rExpr b pos

assignStmt :: Parser (Stmt SourcePos)
assignStmt = do
    pos <- getSourcePos
    lvals <- sepEndBy1 lvalue (symbol ",")
    symbol "="
    exprs <- sepEndBy1 expression (symbol ",")
    symbol ";"
    return $ AssignStmt lvals exprs pos

primOpStmt :: Parser (Stmt SourcePos)
primOpStmt = do
    pos <- getSourcePos
    lName <- Name <$> name
    symbol "="
    symbol "%%"
    rName <- Name <$> name
    mActuals <- optional . parens $ sepEndBy actual (symbol ",")
    flows <- many flow
    symbol ";"
    return $ PrimOpStmt lName rName (fromMaybe [] mActuals) flows pos


callStmt :: Parser (Stmt SourcePos)
callStmt = do
    pos <- getSourcePos
    mKindNames <- optional (kindedNames <* symbol "=")
    mConv <- optional convention
    expr <- expression
    actuals <- parens $ sepEndBy actual (symbol ",")
    mTargs <- optional targets
    annots <- many $ Left <$> flow <|> Right <$> alias
    symbol ";"
    return $ CallStmt (fromMaybe [] mKindNames) mConv expr actuals mTargs annots pos

jumpStmt :: Parser (Stmt SourcePos)
jumpStmt = do
    pos <- getSourcePos
    mConv <- optional convention
    keyword "jump"
    expr <- expression
    mActuals <- optional . parens $ sepEndBy actual (symbol ",")
    mTargs <- optional targets
    symbol ";"
    return $ JumpStmt mConv expr (fromMaybe [] mActuals) mTargs pos




returnStmt :: Parser (Stmt SourcePos)
returnStmt = do
    pos <- getSourcePos
    mConv <- optional convention
    keyword "return"
    mExprs <- optional . angles $ liftA2 (,) expression expression
    mActuals <- optional . parens $ sepEndBy actual (symbol ",")
    symbol ";"
    return $ ReturnStmt mConv mExprs (fromMaybe [] mActuals) pos

lvalue :: Parser (LValue SourcePos)
lvalue = try lvRef <|> lvName

lvRef :: Parser (LValue SourcePos)
lvRef = do
    pos <- getSourcePos
    t <- type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ LVRef t expr mAsserts pos

lvName :: Parser (LValue SourcePos)
lvName = do
    pos <- getSourcePos
    n <- Name <$> name
    return $ LVName n pos

assertions = alignAssert <|> inAssert

alignAssert :: Parser (Asserts SourcePos)
alignAssert = getSourcePos <**> liftA2 AlignAssert
    (keyword "aligned" *> (fst <$> integer))
    (fromMaybe  [] <$> optional (keyword "in" *> sepBy1 (Name <$> name) (symbol ",")))

inAssert :: Parser (Asserts SourcePos)
inAssert = getSourcePos <**> liftA2 InAssert
    (keyword "in" *> sepBy1 (Name <$> name) (symbol ","))
    (optional (keyword "aligned" *> (fst <$> integer)))

labelStmt :: Parser (Stmt SourcePos)
labelStmt = do
    pos <- getSourcePos
    n <- Name <$> name
    symbol ":"
    return $ LabelStmt n pos

contStmt :: Parser (Stmt SourcePos)
contStmt = do
    keyword "continuation"
    pos <- getSourcePos
    n <- Name <$> name
    params <- parens $ optional kindedNames
    return $ ContStmt n (fromMaybe [] params) pos

gotoStmt :: Parser (Stmt SourcePos)
gotoStmt = do
    keyword "goto"
    pos <- getSourcePos
    expr <- expression
    mTargets  <- optional targets
    return $ GotoStmt expr mTargets pos

cutToStmt :: Parser (Stmt SourcePos)
cutToStmt = do
    keyword "cut"
    keyword "to"
    pos <- getSourcePos
    expr <- expression
    actuals <- parens $ sepBy actual (symbol ",")
    mFlow <- many flow
    return $ CutToStmt expr actuals mFlow pos

kindedNames :: Parser [KindName SourcePos]
kindedNames = sepBy1 (do
    k <- optional kind
    pos <- getSourcePos
    n <- Name <$> name
    return $ KindName k n pos) (symbol ",")

arm :: Parser (Arm SourcePos)
arm = do
    pos <- getSourcePos
    keyword "case"
    ranges <- sepBy1 range (symbol ",")
    symbol ":"
    b <- braces body
    return $ Arm ranges b pos

range :: Parser (Range SourcePos)
range = do
    pos <- getSourcePos
    lExpr <- expression
    rExpr <- optional $ symbol ".." >> expression
    return $ Range lExpr rExpr pos

flow :: Parser (Flow SourcePos)
flow = alsoFlow <|> neverReturns

alsoFlow :: Parser (Flow SourcePos)
alsoFlow = keyword "also" >> (alsoCutsTo <|> alsoUnwindsTo <|> alsoReturnsTo <|> alsoAborts)

alsoCutsTo :: Parser (Flow SourcePos)
alsoCutsTo = do
    pos <- getSourcePos
    keyword "cuts"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoCutsTo names pos

alsoUnwindsTo :: Parser (Flow SourcePos)
alsoUnwindsTo = do
    pos <- getSourcePos
    keyword "unwinds"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoUnwindsTo names pos

alsoReturnsTo :: Parser (Flow SourcePos)
alsoReturnsTo = do
    pos <- getSourcePos
    keyword "returns"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoReturnsTo names pos

alsoAborts :: Parser (Flow SourcePos)
alsoAborts = do
    pos <- getSourcePos
    keyword "aborts"
    optional (symbol ",")
    return $ AlsoAborts pos

neverReturns :: Parser (Flow SourcePos)
neverReturns = do
    pos <- getSourcePos
    keyword "never"
    keyword "returns"
    optional (symbol ",")
    return $ NeverReturns pos

alias :: Parser (Alias SourcePos)
alias = readsAlias <|> writesAlias

readsAlias :: Parser (Alias SourcePos)
readsAlias = do
    pos <- getSourcePos
    keyword "reads"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Reads names pos

writesAlias :: Parser (Alias SourcePos)
writesAlias = do
    pos <- getSourcePos
    keyword "writes"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Writes names pos

targets :: Parser (Targets SourcePos)
targets = do
    pos <- getSourcePos
    keyword "targets"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Targets names pos

expression :: Parser (Expr SourcePos)
expression = litExpr
         <|> nameExpr
         <|> refExpr
         <|> parExpr
         <|> binOpExpr
         <|> comExpr
         <|> negExpr
         <|> infixExpr
         <|> prefixExpr

litExpr = getSourcePos <**> liftA2 LitExpr (intExpr <|> floatExpr <|> charExpr) (optional $ symbol "::" *> type_)

intExpr :: Parser (Lit SourcePos)
intExpr = getSourcePos <**>  (LitInt . fst <$> integer)

floatExpr = undefined

charExpr :: Parser (Lit SourcePos)
charExpr = getSourcePos <**> (LitChar <$> charLiteral)

nameExpr :: Parser (Expr SourcePos)
nameExpr = do
    pos <- getSourcePos
    n <- Name <$> name
    return $ NameExpr n pos

refExpr :: Parser (Expr SourcePos)
refExpr = do
    pos <- getSourcePos
    t <- type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ RefExpr t expr mAsserts pos

parExpr :: Parser (Expr SourcePos)
parExpr = do
    pos <- getSourcePos
    pExpr <- parens  expression
    return $ ParExpr pExpr pos

binOpExpr :: Parser (Expr SourcePos)
binOpExpr = makeExprParser expression binOpTable

binOpTable = [ [ prefix "-" $ flip NegExpr
               , prefix "~" $ flip ComExpr
               ]
             , [ binary "/" . flip3 $ flip BinOpExpr DivOp
               , binary "*" . flip3 $ flip BinOpExpr MulOp
               , binary "%" . flip3 $ flip BinOpExpr ModOp
               ]
             , [ binary "-" . flip3 $ flip BinOpExpr SubOp
               , binary "+" . flip3 $ flip BinOpExpr AddOp
               ]
             , [ binary ">>" . flip3 $ flip BinOpExpr ShROp
               , binary "<<" . flip3 $ flip BinOpExpr ShLOp
               ]
             , [ binary "&" . flip3 $ flip BinOpExpr AndOp
               ]
             , [ binary "^" . flip3 $ flip BinOpExpr XorOp
               ]
             , [ binary "|" . flip3 $ flip BinOpExpr OrOp
               ]
             , [ binary ">=" . flip3 $ flip BinOpExpr GeOp
               , binary ">" . flip3 $ flip BinOpExpr GtOp
               , binary "<=" . flip3 $ flip BinOpExpr LeOp
               , binary "<" . flip3 $ flip BinOpExpr LtOp
               , binary "!=" . flip3 $ flip BinOpExpr NeqOp
               , binary "==" . flip3 $ flip BinOpExpr EqOp
               ]
            ]

prefix name f = Prefix (f <$ symbol name <*> getSourcePos)

binary name f = InfixL (f <$ symbol name <*> getSourcePos)

compare name f = InfixN (f <$ symbol name <*> getSourcePos)

flip3 f c a b = f a b c

comExpr :: Parser (Expr SourcePos)
comExpr = do
    pos <- getSourcePos
    symbol "~"
    expr <- expression
    return $ ComExpr expr pos

negExpr :: Parser (Expr SourcePos)
negExpr = do
    pos <- getSourcePos
    symbol "-"
    expr <- expression
    return $ NegExpr expr pos

infixExpr :: Parser (Expr SourcePos)
infixExpr = getSourcePos <**> liftA3 InfixExpr expression (symbol "`" *> (Name <$> name) <* symbol "`") expression

prefixExpr = do
    pos <- getSourcePos
    symbol "%"
    n <- Name <$> name
    mActuals <- optional . parens $ sepEndBy actual (symbol ",")
    return $ PrefixExpr n (fromMaybe [] mActuals) pos


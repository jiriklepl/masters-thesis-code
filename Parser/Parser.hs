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

type Parser a = Parsec Void Text a

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
unit = getSourcePos <**> (do
    topLevels <- many (getSourcePos <**> topLevel)
    return $ Unit topLevels)

topLevel :: Parser (SourcePos -> TopLevel SourcePos)
topLevel = sectionTopLevel <|> (TopDecl .) <$> decl <|> (TopProcedure .) <$> procedure

sectionTopLevel :: Parser (SourcePos -> TopLevel SourcePos)
sectionTopLevel = do
    keyword "section"
    name <- stringLiteral
    sections <- braces $ many (getSourcePos <**> section)
    return $ TopSection name sections

section :: Parser (SourcePos -> Section SourcePos)
section = (SecDecl .) <$> decl <|> secSpan <|> try ((SecProcedure .) <$> procedure) <|> (SecDatum .) <$> datum

decl :: Parser (SourcePos -> Decl SourcePos)
decl = importDecl <|> exportDecl <|> constDecl <|> typedefDecl <|> pragmaDecl <|> targetDecl <|> registerDecl

importDecl :: Parser (SourcePos -> Decl SourcePos)
importDecl = do
    keyword "import"
    imports <- sepEndBy1 (getSourcePos <**> import_) (symbol ",")
    symbol ";"
    return $ ImportDecl imports

exportDecl :: Parser (SourcePos -> Decl SourcePos)
exportDecl = do
    keyword "export"
    exports <- sepEndBy1 (getSourcePos <**> export) (symbol ",")
    symbol ";"
    return $ ExportDecl exports

constDecl :: Parser (SourcePos -> Decl SourcePos)
constDecl = do
    keyword "const"
    try (do
        t <- getSourcePos <**> type_
        declName <- Name <$> name
        symbol "="
        expr <- expression
        symbol ";"
        return $ ConstDecl (Just t) declName . expr)
      <|> do
        declName <- Name <$> name
        symbol "="
        expr <- expression
        symbol ";"
        return $ ConstDecl Nothing declName . expr

typedefDecl :: Parser (SourcePos -> Decl SourcePos)
typedefDecl = do
    keyword "typedef"
    t <- type_
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    symbol ";"
    return $ flip TypedefDecl names . t

pragmaDecl :: Parser (SourcePos -> Decl SourcePos)
pragmaDecl = do
    keyword "pragma"
    pragmaName <- Name <$> name
    p <- braces pragma
    return $ PragmaDecl pragmaName . p

targetDecl :: Parser (SourcePos -> Decl SourcePos)
targetDecl = do
    keyword "target"
    directives <- many (getSourcePos <**> targetDirective)
    symbol ";"
    return $ TargetDecl directives

targetDirective :: Parser (SourcePos -> TargetDirective SourcePos)
targetDirective = memSizeDirective <|> byteOrderDirective <|> pointerSizeDirective <|> wordSizeDirective

registerDecl :: Parser (SourcePos -> Decl SourcePos)
registerDecl = do
    invar <- optional (keyword "invariant")
    regs <- registers
    symbol ";"
    return $ RegDecl (Invariant <$ invar) . regs

memSizeDirective :: Parser (SourcePos -> TargetDirective SourcePos)
memSizeDirective = do
    keyword "memsize"
    (memSize, False) <- integer
    return $ MemSize memSize

byteOrderDirective :: Parser (SourcePos -> TargetDirective SourcePos)
byteOrderDirective = do
    keyword "byteorder"
    ByteOrder <$> endian

endian :: Parser Endian
endian = do keyword "little" >> return Little
    <|> do keyword "big" >> return Big

pointerSizeDirective :: Parser (SourcePos -> TargetDirective SourcePos)
pointerSizeDirective = do
    keyword "pointersize"
    (pointerSize, False) <- integer
    return $ PointerSize pointerSize

wordSizeDirective :: Parser (SourcePos -> TargetDirective SourcePos)
wordSizeDirective = do
    keyword "wordsize"
    (wordSize, False) <- integer
    return $ PointerSize wordSize

procedure :: Parser (SourcePos -> Procedure SourcePos)
procedure = do
    conv <- optional convention
    procName <- Name <$> name
    formals <- parens $ (getSourcePos <**> formal) `sepEndBy` symbol "," -- TODO: discuss later
    procBody <- braces body
    return $ Procedure conv procName formals . procBody

formal :: Parser (SourcePos -> Formal SourcePos)
formal = do
    pos <- getSourcePos
    k <- optional kind
    invar <- optional (keyword "invariant")
    t <- type_
    formalName <- Name <$> name
    return $ flip (Formal k (Invariant <$ invar)) formalName  . t

actual :: Parser (SourcePos -> Actual SourcePos)
actual = do
    k <- optional kind
    expr <- expression
    return $ Actual k . expr

convention :: Parser Conv
convention = keyword "foreign" >> Foreign <$> stringLiteral

import_ :: Parser (SourcePos -> Import SourcePos)
import_ = do
    as <-  optional (stringLiteral <* keyword "as")
    importName  <- Name <$> name
    return $ Import as importName

export :: Parser (SourcePos -> Export SourcePos)
export = do
    exportName  <- Name <$> name
    as <-  optional (keyword "as" >> stringLiteral)
    return $ Export exportName as

body :: Parser (SourcePos -> Body SourcePos)
body = do
    items <- many (getSourcePos <**> bodyItem)
    return $ Body items

bodyItem :: Parser (SourcePos -> BodyItem SourcePos)
bodyItem = (BodyDecl .) <$> decl <|> (BodyStackDecl .) <$> stackDecl <|> (BodyStmt .) <$> stmt

secSpan :: Parser (SourcePos -> Section SourcePos)
secSpan = do
    left <- expression
    right <- getSourcePos <**> expression
    sections <- many (getSourcePos <**> section)
    return (\sourcePos -> SecSpan (left sourcePos) right sections)

datum :: Parser (SourcePos -> Datum SourcePos)
datum = alignDatum <|> try labelDatum <|> justDatum

alignDatum :: Parser (SourcePos -> Datum SourcePos)
alignDatum = do
    keyword "align"
    (align, False) <- integer
    symbol ";"
    return $ DatumAlign align

labelDatum :: Parser (SourcePos -> Datum SourcePos)
labelDatum = do
    labelName <- Name <$> name
    symbol ":"
    return $ DatumLabel labelName

justDatum :: Parser (SourcePos -> Datum SourcePos)
justDatum = do
    t <- type_
    s <- optional (getSourcePos <**> size)
    i <- optional (getSourcePos <**> init_)
    symbol ";"
    return (\sourcePos -> Datum (t sourcePos) s i)

init_ :: Parser (SourcePos -> Init SourcePos)
init_ = stringInit <|> string16Init <|> initList

initList :: Parser (SourcePos -> Init SourcePos)
initList = braces $ do
    exprs <- (getSourcePos <**> expression) `sepEndBy1` symbol ","
    return $ ExprInit exprs

stringInit :: Parser (SourcePos -> Init SourcePos)
stringInit = do
    str <- stringLiteral
    return $ StrInit str

string16Init :: Parser (SourcePos -> Init SourcePos)
string16Init = do
    keyword "unicode"
    str <- parens stringLiteral
    return $ Str16Init (String16 str) -- TODO

size :: Parser (SourcePos -> Size SourcePos)
size = brackets $ do
    expr <-  optional (getSourcePos <**> expression)
    return $ Size expr

registers :: Parser (SourcePos -> Registers SourcePos)
registers = do
    k <- optional kind
    t <- type_
    nvals <- sepEndBy1 ( do
        n <- Name <$> name
        val <- optional $ do
            symbol "="
            stringLiteral
        return (n, val)) (symbol ",")
    return $ flip (Registers k) nvals . t

type_ :: Parser (SourcePos -> Type SourcePos)
type_ = bitsType <|> nameType

bitsType :: Parser (SourcePos -> Type SourcePos)
bitsType = do
    string "bits"
    bits <- L.decimal
    return $ TBits bits

nameType :: Parser (SourcePos -> Type SourcePos)
nameType = do
    n <- Name <$> name
    return $ TName n

kind :: Parser Kind
kind = Kind <$> stringLiteral

pragma = undefined -- TODO

stackDecl :: Parser (SourcePos -> StackDecl SourcePos)
stackDecl = do
    keyword "stackdata"
    datums <- braces $ many (getSourcePos <**> datum)
    return $ StackDecl datums

stmt :: Parser (SourcePos -> Stmt SourcePos)
stmt = emptyStmt <|> ifStmt <|> switchStmt <|> spanStmt <|> assignStmt <|> primOpStmt <|> callStmt <|> jumpStmt <|> returnStmt <|> labelStmt <|> contStmt <|> gotoStmt <|> cutToStmt

emptyStmt :: Parser (SourcePos -> Stmt SourcePos)
emptyStmt = do
    symbol ";"
    return $ EmptyStmt

ifStmt :: Parser (SourcePos -> Stmt SourcePos)
ifStmt = do
    keyword "if"
    expr <- getSourcePos <**> expression
    ifBody <- braces body
    elseBody <- optional $ keyword "else" >> braces (getSourcePos <**> body)
    return $ flip (IfStmt expr) elseBody . ifBody

switchStmt :: Parser (SourcePos -> Stmt SourcePos)
switchStmt = do
    keyword "switch"
    expr <- expression
    arms <- braces $ many (getSourcePos <**> arm)
    return $ flip SwitchStmt arms . expr

spanStmt :: Parser (SourcePos -> Stmt SourcePos)
spanStmt = do
    keyword "span"
    lExpr <- expression
    rExpr <- getSourcePos <**> expression
    b <- braces (getSourcePos <**> body)
    return (\sourcePos -> SpanStmt (lExpr sourcePos) rExpr b)

assignStmt :: Parser (SourcePos -> Stmt SourcePos)
assignStmt = do
    lvals <- sepEndBy1 (getSourcePos <**> lvalue) (symbol ",")
    symbol "="
    exprs <- sepEndBy1 (getSourcePos <**> expression) (symbol ",")
    symbol ";"
    return $ AssignStmt lvals exprs

primOpStmt :: Parser (SourcePos -> Stmt SourcePos)
primOpStmt = do
    lName <- Name <$> name
    symbol "="
    symbol "%%"
    rName <- Name <$> name
    mActuals <- optional . parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    flows <- many (getSourcePos <**> flow)
    symbol ";"
    return $ PrimOpStmt lName rName (fromMaybe [] mActuals) flows

callStmt :: Parser (SourcePos -> Stmt SourcePos)
callStmt = do
    mKindNames <- optional (kindedNames <* symbol "=")
    mConv <- optional convention
    expr <- expression
    actuals <- parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    mTargs <- optional (getSourcePos <**> targets)
    annots <- many $ getSourcePos <**> ((Left .) <$> flow <|> (Right .) <$> alias)
    symbol ";"
    return (\sourcePos -> CallStmt (fromMaybe [] mKindNames) mConv (expr sourcePos) actuals mTargs annots)

jumpStmt :: Parser (SourcePos -> Stmt SourcePos)
jumpStmt = do
    mConv <- optional convention
    keyword "jump"
    expr <- expression
    mActuals <- optional . parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    mTargs <- optional (getSourcePos <**> targets)
    symbol ";"
    return (\sourcePos -> JumpStmt mConv (expr sourcePos) (fromMaybe [] mActuals) mTargs)

returnStmt :: Parser (SourcePos -> Stmt SourcePos)
returnStmt = do
    mConv <- optional convention
    keyword "return"
    mExprs <- optional . angles $ liftA2 (,) (getSourcePos <**> expression) (getSourcePos <**> expression)
    mActuals <- optional . parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    symbol ";"
    return $ ReturnStmt mConv mExprs (fromMaybe [] mActuals)

lvalue :: Parser (SourcePos -> LValue SourcePos)
lvalue = try lvRef <|> lvName

lvRef :: Parser (SourcePos -> LValue SourcePos)
lvRef = do
    t <- getSourcePos <**> type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ flip (LVRef t) mAsserts . expr

lvName :: Parser (SourcePos -> LValue SourcePos)
lvName = do
    n <- Name <$> name
    return $ LVName n

assertions = alignAssert <|> inAssert

alignAssert :: Parser (Asserts SourcePos)
alignAssert = getSourcePos <**> liftA2 AlignAssert
    (keyword "aligned" *> (fst <$> integer))
    (fromMaybe  [] <$> optional (keyword "in" *> sepBy1 (Name <$> name) (symbol ",")))

inAssert :: Parser (Asserts SourcePos)
inAssert = getSourcePos <**> liftA2 InAssert
    (keyword "in" *> sepBy1 (Name <$> name) (symbol ","))
    (optional (keyword "aligned" *> (fst <$> integer)))

labelStmt :: Parser (SourcePos -> Stmt SourcePos)
labelStmt = do
    n <- Name <$> name
    symbol ":"
    return $ LabelStmt n

contStmt :: Parser (SourcePos -> Stmt SourcePos)
contStmt = do
    keyword "continuation"
    n <- Name <$> name
    params <- parens $ optional kindedNames
    symbol ":"
    return $ ContStmt n (fromMaybe [] params)

gotoStmt :: Parser (SourcePos -> Stmt SourcePos)
gotoStmt = do
    keyword "goto"
    expr <- expression
    mTargets  <- optional (getSourcePos <**> targets)
    symbol ";"
    return $ flip GotoStmt mTargets . expr

cutToStmt :: Parser (SourcePos -> Stmt SourcePos)
cutToStmt = do
    keyword "cut"
    keyword "to"
    expr <- expression
    actuals <- parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    mFlow <- many (getSourcePos <**> flow)
    symbol ";"
    return (\sourcePos -> CutToStmt (expr sourcePos) actuals mFlow)

kindedNames :: Parser [KindName SourcePos]
kindedNames = sepEndBy1 (do
    k <- optional kind
    pos <- getSourcePos
    n <- Name <$> name
    return $ KindName k n pos) (symbol ",")

arm :: Parser (SourcePos -> Arm SourcePos)
arm = do
    keyword "case"
    ranges <- sepEndBy1 (getSourcePos <**> range) (symbol ",")
    symbol ":"
    b <- braces body
    return $ Arm ranges . b

range :: Parser (SourcePos -> Range SourcePos)
range = do
    lExpr <- expression
    rExpr <- optional $ symbol ".." >> (getSourcePos <**> expression)
    return $ flip Range rExpr . lExpr

flow :: Parser (SourcePos -> Flow SourcePos)
flow = alsoFlow <|> neverReturns

alsoFlow :: Parser (SourcePos -> Flow SourcePos)
alsoFlow = keyword "also" >> (alsoCutsTo <|> alsoUnwindsTo <|> alsoReturnsTo <|> alsoAborts)

alsoCutsTo :: Parser (SourcePos -> Flow SourcePos)
alsoCutsTo = do
    keyword "cuts"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoCutsTo names

alsoUnwindsTo :: Parser (SourcePos -> Flow SourcePos)
alsoUnwindsTo = do
    keyword "unwinds"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoUnwindsTo names

alsoReturnsTo :: Parser (SourcePos -> Flow SourcePos)
alsoReturnsTo = do
    keyword "returns"
    keyword "to"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ AlsoReturnsTo names

alsoAborts :: Parser (SourcePos -> Flow SourcePos)
alsoAborts = do
    keyword "aborts"
    optional (symbol ",")
    return AlsoAborts

neverReturns :: Parser (SourcePos -> Flow SourcePos)
neverReturns = do
    keyword "never"
    keyword "returns"
    optional (symbol ",")
    return NeverReturns

alias :: Parser (SourcePos -> Alias SourcePos)
alias = readsAlias <|> writesAlias

readsAlias :: Parser (SourcePos -> Alias SourcePos)
readsAlias = do
    keyword "reads"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Reads names

writesAlias :: Parser (SourcePos -> Alias SourcePos)
writesAlias = do
    keyword "writes"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Writes names

targets :: Parser (SourcePos -> Targets SourcePos)
targets = do
    keyword "targets"
    names <- sepEndBy1 (Name <$> name) (symbol ",")
    return $ Targets names

expression :: Parser (SourcePos -> Expr SourcePos)
expression = litExpr
         <|> nameExpr
         <|> refExpr
         <|> parExpr
         <|> binOpExpr
         <|> comExpr
         <|> negExpr
         <|> infixExpr
         <|> prefixExpr

litExpr :: Parser (SourcePos -> Expr SourcePos)
litExpr = do
    literal <- intExpr <|> floatExpr <|> charExpr
    t <- optional $ symbol "::" *> (getSourcePos <**> type_)
    return (\sourcePos -> LitExpr (literal sourcePos) t)

intExpr :: Parser (SourcePos -> Lit SourcePos)
intExpr = LitInt . fst <$> integer

floatExpr = undefined

charExpr :: Parser (SourcePos -> Lit SourcePos)
charExpr = LitChar <$> charLiteral

nameExpr :: Parser (SourcePos -> Expr SourcePos)
nameExpr = do
    n <- Name <$> name
    return $ NameExpr n

refExpr :: Parser (SourcePos -> Expr SourcePos)
refExpr = do
    t <- getSourcePos <**> type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return (\sourcePos -> RefExpr t (expr sourcePos) mAsserts)

parExpr :: Parser (SourcePos -> Expr SourcePos)
parExpr = do
    pExpr <- parens expression
    return $ ParExpr . pExpr

binOpExpr :: Parser (SourcePos -> Expr SourcePos)
binOpExpr = makeExprParser expression binOpTable

binOpTable = [ [ prefix "-" NegExpr
               , prefix "~" ComExpr
               ]
             , [ binary "/" $ flip BinOpExpr DivOp
               , binary "*" $ flip BinOpExpr MulOp
               , binary "%" $ flip BinOpExpr ModOp
               ]
             , [ binary "-" $ flip BinOpExpr SubOp
               , binary "+" $ flip BinOpExpr AddOp
               ]
             , [ binary ">>" $ flip BinOpExpr ShROp
               , binary "<<" $ flip BinOpExpr ShLOp
               ]
             , [ binary "&" $ flip BinOpExpr AndOp
               ]
             , [ binary "^" $ flip BinOpExpr XorOp
               ]
             , [ binary "|" $ flip BinOpExpr OrOp
               ]
             , [ binary ">=" $ flip BinOpExpr GeOp
               , binary ">" $ flip BinOpExpr GtOp
               , binary "<=" $ flip BinOpExpr LeOp
               , binary "<" $ flip BinOpExpr LtOp
               , binary "!=" $ flip BinOpExpr NeqOp
               , binary "==" $ flip BinOpExpr EqOp
               ]
            ]

prefix name f = Prefix ((f .) <$ symbol name)

binary name f = InfixL ((\left right sourcePos -> f (left sourcePos) (right sourcePos)) <$ symbol name)

compare name f = InfixN ((\left right sourcePos -> f (left sourcePos) (right sourcePos)) <$ symbol name)

flip3 f c a b = f a b c

comExpr :: Parser (SourcePos -> Expr SourcePos)
comExpr = do
    symbol "~"
    expr <- expression
    return $ ComExpr . expr

negExpr :: Parser (SourcePos -> Expr SourcePos)
negExpr = do
    symbol "-"
    expr <- expression
    return $ NegExpr . expr

infixExpr :: Parser (SourcePos -> Expr SourcePos)
infixExpr = do
    left <- expression
    n <- symbol "`" *> (Name <$> name) <* symbol "`"
    right <- getSourcePos <**> expression
    return (\sourcePos -> InfixExpr (left sourcePos) n right)

prefixExpr :: Parser (SourcePos -> Expr SourcePos)
prefixExpr = do
    symbol "%"
    n <- Name <$> name
    mActuals <- optional . parens $ sepEndBy (getSourcePos <**> actual) (symbol ",")
    return $ PrefixExpr n (fromMaybe [] mActuals)

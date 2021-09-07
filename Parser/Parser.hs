{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Maybe
import Data.Functor
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative hiding (many)
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import Language

-- | Functional composition for applicative functors
(<*<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) = liftA2 (.)

-- | Functional composition for applicative functors (reversed)
(>*>) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>*>) = liftA2 (flip (.))

type Parser a = Parsec Void Text a
type ULocParser a = Parser (SourcePos -> a)

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

withSourcePos :: ULocParser a -> Parser a
withSourcePos = (getSourcePos <**>)

identifier :: Parser Name
identifier = Name <$> lexeme name

identifiers :: Parser [Name]
identifiers = sepEndBy1 identifier (symbol ",")

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
    (char '0' *>
        ((oneOf ['x','X'] *> ((, False) <$> L.hexadecimal))
        <|> (, False) <$> L.octal
        <|> (,) 0 . isNothing <$> optional (oneOf ['u','U'])))
    <|> decimal

decimal :: Parser (Int, Bool)
decimal = do
    nums <- L.decimal
    unsigned <- optional $ oneOf ['u','U']
    return (nums, isNothing unsigned)

-- | Parses the whole 'Unit'
unit :: Parser (Unit SourcePos)
unit = withSourcePos (Unit <$> many (withSourcePos topLevel))

topLevel :: ULocParser (TopLevel SourcePos)
topLevel = choice
    [ sectionTopLevel
    , try $ (TopProcedure .) <$> procedure
    , (TopDecl .) <$> decl
    ]

sectionTopLevel :: ULocParser (TopLevel SourcePos)
sectionTopLevel = keyword "section" *> (TopSection <$> stringLiteral <*> braces (many $ withSourcePos section))

section :: ULocParser (Section SourcePos)
section = choice [(SecDecl .) <$> decl, secSpan, try ((SecProcedure .) <$> procedure), (SecDatum .) <$> datum]

decl :: ULocParser (Decl SourcePos)
decl = choice [importDecl, exportDecl, constDecl, typedefDecl, pragmaDecl, targetDecl, registerDecl]

importDecl :: ULocParser (Decl SourcePos)
importDecl = keyword "import" *> (ImportDecl <$> sepEndBy1 (withSourcePos import_) (symbol ",")) <* symbol ";"

exportDecl :: ULocParser (Decl SourcePos)
exportDecl = keyword "export" *> (ExportDecl <$> sepEndBy1 (withSourcePos export) (symbol ",")) <* symbol ";"

constDecl :: ULocParser (Decl SourcePos)
constDecl = do
    keyword "const"
    try (do
        t <- withSourcePos type_
        declName <- identifier
        symbol "="
        expr <- expression
        symbol ";"
        return $ ConstDecl (Just t) declName . expr)
      <|> do
        declName <- identifier
        symbol "="
        expr <- expression
        symbol ";"
        return $ ConstDecl Nothing declName . expr

typedefDecl :: ULocParser (Decl SourcePos)
typedefDecl = keyword "typedef" *> (type_ >*> (flip TypedefDecl <$> identifiers)) <* symbol ";"

pragmaDecl :: ULocParser (Decl SourcePos)
pragmaDecl = do
    keyword "pragma"
    pragmaName <- identifier
    p <- braces pragma
    return $ PragmaDecl pragmaName . p

targetDecl :: ULocParser (Decl SourcePos)
targetDecl = keyword "target" *> (TargetDecl <$> many (withSourcePos targetDirective)) <* symbol ";"

targetDirective :: ULocParser (TargetDirective SourcePos)
targetDirective = choice [memSizeDirective, byteOrderDirective, pointerSizeDirective, wordSizeDirective]

registerDecl :: ULocParser (Decl SourcePos)
registerDecl = ((RegDecl . (Invariant <$) <$> optional (keyword "invariant")) <*< registers) <* symbol ";"

memSizeDirective :: ULocParser (TargetDirective SourcePos)
memSizeDirective = do
    keyword "memsize"
    (memSize, False) <- integer
    return $ MemSize memSize

byteOrderDirective :: ULocParser (TargetDirective SourcePos)
byteOrderDirective = keyword "byteorder" *> (ByteOrder <$> endian)

endian :: Parser Endian
endian = choice [keyword "little" $> Little, keyword "big" $> Big]

pointerSizeDirective :: ULocParser (TargetDirective SourcePos)
pointerSizeDirective = do
    keyword "pointersize"
    (pointerSize, False) <- integer
    return $ PointerSize pointerSize

wordSizeDirective :: ULocParser (TargetDirective SourcePos)
wordSizeDirective = do
    keyword "wordsize"
    (wordSize, False) <- integer
    return $ PointerSize wordSize

procedure :: ULocParser (Procedure SourcePos)
procedure = do
    conv <- optional convention
    procName <- identifier
    formals <- parens $ withSourcePos formal `sepEndBy` symbol "," -- TODO: discuss later
    procBody <- braces body
    return $ Procedure conv procName formals . procBody

formal :: ULocParser (Formal SourcePos)
formal = do
    pos <- getSourcePos
    k <- optional kind
    invar <- optional (keyword "invariant")
    t <- type_
    formalName <- identifier
    return $ flip (Formal k (Invariant <$ invar)) formalName  . t

actual :: ULocParser (Actual SourcePos)
actual = (Actual <$> optional kind) <*< expression

convention :: Parser Conv
convention = keyword "foreign" >> Foreign <$> stringLiteral

import_ :: ULocParser (Import SourcePos)
import_ = Import <$> optional (stringLiteral <* keyword "as") <*> identifier

export :: ULocParser (Export SourcePos)
export = do
    exportName <- identifier
    as <-  optional (keyword "as" >> stringLiteral)
    return $ Export exportName as

body :: ULocParser (Body SourcePos)
body = Body <$> many (withSourcePos bodyItem)

bodyItem :: ULocParser (BodyItem SourcePos)
bodyItem = (BodyStackDecl .) <$> stackDecl <|> (BodyStmt .) <$> stmt

secSpan :: ULocParser (Section SourcePos)
secSpan = do
    left <- expression
    right <- withSourcePos expression
    sections <- many (withSourcePos section)
    return (\sourcePos -> SecSpan (left sourcePos) right sections)

datum :: ULocParser (Datum SourcePos)
datum = choice [alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser (Datum SourcePos)
alignDatum = do
    keyword "align"
    (align, False) <- integer
    symbol ";"
    return $ DatumAlign align

labelDatum :: ULocParser (Datum SourcePos)
labelDatum = do
    labelName <- identifier
    symbol ":"
    return $ DatumLabel labelName

justDatum :: ULocParser (Datum SourcePos)
justDatum = do
    t <- type_
    s <- optional (withSourcePos size)
    i <- optional (withSourcePos init_)
    symbol ";"
    return (\sourcePos -> Datum (t sourcePos) s i)

init_ :: ULocParser (Init SourcePos)
init_ = choice [stringInit, string16Init, initList]

initList :: ULocParser (Init SourcePos)
initList = braces $ ExprInit <$> withSourcePos expression `sepEndBy1` symbol ","

stringInit :: ULocParser (Init SourcePos)
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser (Init SourcePos)
string16Init = do
    keyword "unicode"
    str <- parens stringLiteral
    return $ Str16Init (String16 str) -- TODO

size :: ULocParser (Size SourcePos)
size = brackets $ Size <$> optional (withSourcePos expression)

registers :: ULocParser (Registers SourcePos)
registers = do
    k <- optional kind
    t <- type_
    nvals <- sepEndBy1 ( do
        n <- identifier
        val <- optional $ do
            symbol "="
            stringLiteral
        return (n, val)) (symbol ",")
    return $ flip (Registers k) nvals . t

type_ :: ULocParser (Type SourcePos)
type_ = bitsType <|> nameType

bitsType :: ULocParser (Type SourcePos)
bitsType = lexeme $ string "bits" >> TBits <$> L.decimal

nameType :: ULocParser (Type SourcePos)
nameType = TName <$> identifier

kind :: Parser Kind
kind = Kind <$> stringLiteral

pragma = undefined -- TODO pragmas not yet specified and with no explanation of functionality

stackDecl :: ULocParser (StackDecl SourcePos)
stackDecl =
    keyword "stackdata" *>
    braces (StackDecl <$> many (withSourcePos datum))

stmt :: ULocParser (Stmt SourcePos)
stmt = choice
    [ emptyStmt
    , ifStmt
    , switchStmt
    , spanStmt
    , try assignStmt
    , try primOpStmt
    , try jumpStmt
    , try returnStmt
    , try labelStmt
    , try contStmt
    , try gotoStmt
    , try cutToStmt
    , callStmt ] -- TODO: this is utter BS

emptyStmt :: ULocParser (Stmt SourcePos)
emptyStmt = symbol ";" $> EmptyStmt

ifStmt :: ULocParser (Stmt SourcePos)
ifStmt = do
    keyword "if"
    expr <- withSourcePos expression
    ifBody <- braces body
    elseBody <- optional $ keyword "else" >> braces (withSourcePos body)
    return $ flip (IfStmt expr) elseBody . ifBody

switchStmt :: ULocParser (Stmt SourcePos)
switchStmt = keyword "switch" *> (expression >*> (flip SwitchStmt <$> braces (many (withSourcePos arm))))

spanStmt :: ULocParser (Stmt SourcePos)
spanStmt = do
    keyword "span"
    lExpr <- expression
    rExpr <- withSourcePos expression
    b <- braces (withSourcePos body)
    return (\sourcePos -> SpanStmt (lExpr sourcePos) rExpr b)

assignStmt :: ULocParser (Stmt SourcePos)
assignStmt = liftA2 AssignStmt
    (sepEndBy1 (withSourcePos lvalue) (symbol ",") <* symbol "=")
    (sepEndBy1 (withSourcePos expression) (symbol ",") <* symbol ";")

primOpStmt :: ULocParser (Stmt SourcePos)
primOpStmt = do
    lName <- identifier
    symbol "="
    symbol "%%"
    rName <- identifier
    mActuals <- optional . parens $ sepEndBy (withSourcePos actual) (symbol ",")
    flows <- many (withSourcePos flow)
    symbol ";"
    return $ PrimOpStmt lName rName (fromMaybe [] mActuals) flows

callStmt :: ULocParser (Stmt SourcePos)
callStmt = do
    mKindNames <- optional (kindedNames <* symbol "=")
    mConv <- optional convention
    expr <- expression
    actuals <- parens $ sepEndBy (withSourcePos actual) (symbol ",")
    mTargs <- optional (withSourcePos targets)
    annots <- many $ withSourcePos ((Left .) <$> flow <|> (Right .) <$> alias)
    symbol ";"
    return (\sourcePos -> CallStmt (fromMaybe [] mKindNames) mConv (expr sourcePos) actuals mTargs annots)

jumpStmt :: ULocParser (Stmt SourcePos)
jumpStmt = do
    mConv <- optional convention
    keyword "jump"
    expr <- expression
    mActuals <- optional . parens $ sepEndBy (withSourcePos actual) (symbol ",")
    mTargs <- optional (withSourcePos targets)
    symbol ";"
    return (\sourcePos -> JumpStmt mConv (expr sourcePos) (fromMaybe [] mActuals) mTargs)

returnStmt :: ULocParser (Stmt SourcePos)
returnStmt = do
    mConv <- optional convention
    keyword "return"
    mExprs <- optional . angles $ liftA2 (,) (withSourcePos expression) (withSourcePos expression)
    mActuals <- optional . parens $ sepEndBy (withSourcePos actual) (symbol ",")
    symbol ";"
    return $ ReturnStmt mConv mExprs (fromMaybe [] mActuals)

lvalue :: ULocParser (LValue SourcePos)
lvalue = try lvRef <|> lvName

lvRef :: ULocParser (LValue SourcePos)
lvRef = do
    t <- withSourcePos type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return $ flip (LVRef t) mAsserts . expr

lvName :: ULocParser (LValue SourcePos)
lvName = LVName <$> identifier

assertions :: Parser (Asserts SourcePos)
assertions = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser (Asserts SourcePos)
alignAssert = liftA2 AlignAssert
    (keyword "aligned" *> (fst <$> integer))
    (fromMaybe  [] <$> optional (keyword "in" *> sepBy1 identifier (symbol ",")))

inAssert :: ULocParser (Asserts SourcePos)
inAssert = liftA2 InAssert
    (keyword "in" *> sepBy1 identifier (symbol ","))
    (optional (keyword "aligned" *> (fst <$> integer)))

labelStmt :: ULocParser (Stmt SourcePos)
labelStmt = LabelStmt <$> identifier <* symbol ":"

contStmt :: ULocParser (Stmt SourcePos)
contStmt = do
    keyword "continuation"
    n <- identifier
    params <- parens $ optional kindedNames
    symbol ":"
    return $ ContStmt n (fromMaybe [] params)

gotoStmt :: ULocParser (Stmt SourcePos)
gotoStmt = do
    keyword "goto"
    expr <- expression
    mTargets  <- optional (withSourcePos targets)
    symbol ";"
    return $ flip GotoStmt mTargets . expr

cutToStmt :: ULocParser (Stmt SourcePos)
cutToStmt = do
    keyword "cut"
    keyword "to"
    expr <- expression
    actuals <- parens $ sepEndBy (withSourcePos actual) (symbol ",")
    mFlow <- many (withSourcePos flow)
    symbol ";"
    return (\sourcePos -> CutToStmt (expr sourcePos) actuals mFlow)

kindedNames :: Parser [KindName SourcePos]
kindedNames = sepEndBy1 (do
    k <- optional kind
    pos <- getSourcePos
    n <- identifier
    return $ KindName k n pos) (symbol ",")

arm :: ULocParser (Arm SourcePos)
arm = keyword "case" *> (Arm <$> sepEndBy1 (withSourcePos range) (symbol ",") <* symbol ":") <*< braces body

range :: ULocParser (Range SourcePos)
range = expression >*> (flip Range <$> optional (symbol ".." *> withSourcePos expression))

flow :: ULocParser (Flow SourcePos)
flow = alsoFlow <|> neverReturns

alsoFlow :: ULocParser (Flow SourcePos)
alsoFlow = keyword "also" *> choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

alsoCutsTo :: ULocParser (Flow SourcePos)
alsoCutsTo = keyword "cuts" *> keyword "to" *> (AlsoCutsTo <$> identifiers)

alsoUnwindsTo :: ULocParser (Flow SourcePos)
alsoUnwindsTo = keyword "unwinds" *> keyword "to" *> (AlsoUnwindsTo <$> identifiers)

alsoReturnsTo :: ULocParser (Flow SourcePos)
alsoReturnsTo = keyword "returns" *> keyword "to" *> (AlsoReturnsTo <$> identifiers)

alsoAborts :: ULocParser (Flow SourcePos)
alsoAborts = keyword "aborts" *> optional (symbol ",") $> AlsoAborts

neverReturns :: ULocParser (Flow SourcePos)
neverReturns = keyword "never" *> keyword "returns" *> optional (symbol ",") $> NeverReturns

alias :: ULocParser (Alias SourcePos)
alias = readsAlias <|> writesAlias

readsAlias :: ULocParser (Alias SourcePos)
readsAlias = keyword "reads" *> (Reads <$> identifiers)

writesAlias :: ULocParser (Alias SourcePos)
writesAlias = keyword "writes" *> (Writes <$> identifiers)

targets :: ULocParser (Targets SourcePos)
targets = keyword "targets" *> (Targets <$> identifiers)

expression :: ULocParser (Expr SourcePos)
expression = try infixExpr <|> binOpExpr

simpleExpr :: ULocParser (Expr SourcePos)
simpleExpr = choice
    [ litExpr
    , parExpr
    , prefixExpr
    , try refExpr
    , nameExpr
    ]

litExpr :: ULocParser (Expr SourcePos)
litExpr = do
    literal <- choice [intExpr, floatExpr, charExpr]
    t <- optional $ symbol "::" *> withSourcePos type_
    return (\sourcePos -> LitExpr (literal sourcePos) t)

intExpr :: ULocParser (Lit SourcePos)
intExpr = LitInt . fst <$> integer

floatExpr = intExpr -- TODO

charExpr :: ULocParser (Lit SourcePos)
charExpr = LitChar <$> charLiteral

nameExpr :: ULocParser (Expr SourcePos)
nameExpr = NameExpr <$> identifier

refExpr :: ULocParser (Expr SourcePos)
refExpr = do
    t <- withSourcePos type_
    (expr, mAsserts) <- brackets $ liftA2 (,) expression (optional assertions)
    return (\sourcePos -> RefExpr t (expr sourcePos) mAsserts)

parExpr :: ULocParser (Expr SourcePos)
parExpr = do
    pExpr <- parens expression
    return $ ParExpr . pExpr

binOpExpr :: ULocParser (Expr SourcePos)
binOpExpr = makeExprParser simpleExpr binOpTable

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
             , [ comparison ">=" $ flip BinOpExpr GeOp
               , comparison ">" $ flip BinOpExpr GtOp
               , comparison "<=" $ flip BinOpExpr LeOp
               , comparison "<" $ flip BinOpExpr LtOp
               , comparison "!=" $ flip BinOpExpr NeqOp
               , comparison "==" $ flip BinOpExpr EqOp
               ]
            ]

prefix name f = Prefix ((f .) <$ symbol name)

binary name f = InfixL ((\rightPos left right sourcePos -> f (left sourcePos) (right rightPos)) <$> (symbol name *> getSourcePos))

comparison name f = InfixN ((\rightPos left right sourcePos -> f (left sourcePos) (right rightPos)) <$> (symbol name *> getSourcePos))

infixExpr :: ULocParser (Expr SourcePos)
infixExpr = do
    left <- binOpExpr
    n <- symbol "`" *> (Name <$> name) <* symbol "`"
    right <- withSourcePos binOpExpr
    return (\sourcePos -> InfixExpr (left sourcePos) n right)

prefixExpr :: ULocParser (Expr SourcePos)
prefixExpr = do
    symbol "%"
    n <- identifier
    mActuals <- optional . parens $ sepEndBy (withSourcePos actual) (symbol ",")
    return $ PrefixExpr n (fromMaybe [] mActuals)

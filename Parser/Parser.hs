{-# LANGUAGE TupleSections #-}

module Parser (someFunc) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Control.Monad.State as State
import Language

data ParserState = ParserState
    { dummy1 :: [String]
    , dummy2 :: [String]
    }

type StateMonad = State ParserState
type Parser a = ParsecT Void Text StateMonad a

sc :: Parser ()
sc = L.space space1 (L.skipLineComment (T.pack "//")) (L.skipBlockComment (T.pack "/*") (T.pack "*/"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser Text
keyword = lexeme . string

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

name :: Parser Text
name = do
    n <- letterChar <|> oneOf "_.$@"
    ame <- T.pack <$> many (alphaNumChar <|> oneOf "_.$@")
    return $ n `T.cons` ame

braces :: Parser a -> Parser a
braces = between (symbol (T.pack "{")) (symbol (T.pack "}"))

parens :: Parser a -> Parser a
parens = between (symbol (T.pack "(")) (symbol (T.pack ")"))

integer :: Parser (Int, Bool)
integer = lexeme $
    do char '0' >> oneOf "xX" >> (, False) <$> L.hexadecimal
    <|> do char '0' >> (, False) <$> L.octal
    <|> do decimal

decimal :: Parser (Int, Bool)
decimal = do
    nums <- L.decimal
    unsigned <- optional (oneOf "uU")
    return (nums, isNothing unsigned)

-- | Parses the whole 'Unit'
unit :: Parser (Unit SourcePos)
unit = do
    pos <- getSourcePos
    topLevels <- many topLevel
    return $ Unit topLevels pos

topLevel :: Parser (TopLevel SourcePos)
topLevel = sectionTopLevel <|> TopDecl <$> decl <|> TopProcedure <$> procedure

sectionTopLevel = do
    keyword (T.pack "section")
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
    keyword (T.pack "import")
    pos <- getSourcePos
    imports <- sepBy1 import_ (symbol (T.pack ","))
    return $ ImportDecl imports pos

exportDecl :: Parser (Decl SourcePos)
exportDecl = do
    keyword (T.pack "export")
    pos <- getSourcePos
    exports <- sepBy1 export (symbol (T.pack ","))
    return $ ExportDecl exports pos

constDecl :: Parser (Decl SourcePos)
constDecl = do
    keyword (T.pack "const")
    pos <- getSourcePos
    try (do
        t <- type_
        declName <- Name <$> name
        expr <- expression
        return $ ConstDecl (Just t) declName expr pos)
      <|> do
        declName <- Name <$> name
        expr <- expression
        return $ ConstDecl Nothing declName expr pos

typedefDecl :: Parser (Decl SourcePos)
typedefDecl = do
    keyword (T.pack "typedef")
    pos <- getSourcePos
    t <- type_
    names <- sepBy1 (Name <$> name) (symbol (T.pack ","))
    return $ TypedefDecl t names pos

pragmaDecl :: Parser (Decl SourcePos)
pragmaDecl = do
    keyword (T.pack "pragma")
    pos <- getSourcePos
    pragmaName <- Name <$> name
    p <- braces pragma
    return $ PragmaDecl pragmaName p pos

targetDecl :: Parser (Decl SourcePos)
targetDecl = do
    keyword (T.pack "target")
    pos <- getSourcePos
    directives <- many targetDirective
    return $ TargetDecl directives pos

targetDirective :: Parser (TargetDirective SourcePos)
targetDirective = memSizeDirective <|> byteOrderDirective <|> pointerSizeDirective <|> wordSizeDirective

memSizeDirective :: Parser (TargetDirective SourcePos)
memSizeDirective = do
    keyword (T.pack "memsize")
    pos <- getSourcePos
    (memSize, False) <- integer
    return $ MemSize memSize pos

byteOrderDirective :: Parser (TargetDirective SourcePos)
byteOrderDirective = do
    keyword (T.pack "byteorder")
    pos <- getSourcePos
    e <- endian
    return $ ByteOrder e pos

endian :: Parser Endian
endian = do keyword (T.pack "little") >> return Little
    <|> do keyword (T.pack "big") >> return Big

pointerSizeDirective :: Parser (TargetDirective SourcePos)
pointerSizeDirective = do
    keyword (T.pack "pointersize")
    pos <- getSourcePos
    (pointerSize, False) <- integer
    return $ PointerSize pointerSize pos

wordSizeDirective :: Parser (TargetDirective SourcePos)
wordSizeDirective = do
    keyword (T.pack "wordsize")
    pos <- getSourcePos
    (wordSize, False) <- integer
    return $ PointerSize wordSize pos

procedure :: Parser (Procedure SourcePos)
procedure = do
    conv <- optional convention
    pos <- getSourcePos
    procName <- Name <$> name
    formals <- parens $ sepBy formal (symbol (T.pack ","))
    procBody <- braces body
    return $ Procedure conv procName formals procBody pos

import_ :: Parser (Import SourcePos)
import_ = do
    as <-  optional (name >>= (\n -> keyword (T.pack "as") >> return n))
    pos <- getSourcePos
    importName  <- Name <$> name
    return $ Import as importName pos

export :: Parser (Export SourcePos)
export = do
    pos <- getSourcePos
    exportName  <- Name <$> name
    as <-  optional (keyword (T.pack "as") >> name)
    return $ Export exportName as pos

body :: Parser (Body SourcePos)
body = do
    pos <- getSourcePos
    items <- many bodyItem
    return $ Body items pos

bodyItem :: Parser (BodyItem SourcePos)
bodyItem = undefined -- TODO

formal = undefined

convention :: Parser Conv
convention = undefined -- TODO

secSpan = undefined -- TODO
datum = undefined -- TODO


registerDecl = undefined -- TODO
type_ = undefined -- TODO
expression = undefined -- TODO
pragma = undefined -- TODO


someFunc :: IO ()
someFunc = putStrLn "someFunc"

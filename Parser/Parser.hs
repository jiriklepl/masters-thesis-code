module Parser (someFunc) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T
import Data.Text (Text)
import qualified Control.Monad.State as State
import qualified Text.Megaparsec.Char.Lexer as L
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
decl = importDecl <|> exportDecl <|> constDecl <|> typedefDecl <|> registerDecl <|> pragmaDecl <|> targetDecl

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

procedure = undefined -- TODO
secSpan = undefined -- TODO
datum = undefined -- TODO
import_ = undefined -- TODO
export = undefined -- TODO

typedefDecl = undefined -- TODO
registerDecl = undefined -- TODO
pragmaDecl = undefined -- TODO
targetDecl = undefined -- TODO
type_ = undefined -- TODO
expression = undefined -- TODO


someFunc :: IO ()
someFunc = putStrLn "someFunc"

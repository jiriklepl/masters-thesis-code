{-# LANGUAGE Safe #-}

module CMM.Lexer
  ( Lexer
  , tokenize
  ) where

import safe CMM.AST.Annot (Annot, withAnnot)
import safe Control.Applicative
  ( Alternative((<|>))
  , Applicative(liftA2)
  , optional
  )
import safe Data.Functor (($>), void)
import safe Data.Maybe (isNothing)
import safe qualified Data.Text as T
import safe Data.Text (Text)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(eof, notFollowedBy, try, label, withRecovery)
  , Parsec
  , SourcePos
  , choice
  , getSourcePos
  , many
  , manyTill
  , oneOf, ParseError, anySingle, registerParseError
  )
import safe Text.Megaparsec.Char
  ( alphaNumChar
  , char
  , letterChar
  , space1
  , string
  )
import safe qualified Text.Megaparsec.Char.Lexer as L

import safe qualified CMM.Lexer.Token as T
import safe CMM.Lexer.Token (Token)
import safe Data.List.Extra ( snoc )

type Lexer = Parsec Void Text


type SourceLexer = Lexer (Annot Token SourcePos)

type ULocLexer = Lexer (Token SourcePos)

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

keyword :: Text -> Lexer ()
keyword k = void . label (T.unpack k) . lexeme $ try (string k <* notFollowedBy alphaNumChar)

stringLiteral :: ULocLexer
stringLiteral =
  lexeme $ char '"' *> (T.StrLit . T.pack <$> manyTill L.charLiteral (char '"'))

charLiteral :: ULocLexer
charLiteral = lexeme $ char '\'' *> (T.CharLit <$> L.charLiteral) <* char '\''

identifier :: ULocLexer
identifier =
  lexeme $
  T.Ident . T.pack <$> liftA2 (:) (letterChar <|> otherIdentChars) identifierRest

identifierRest :: Lexer String
identifierRest = many $ alphaNumChar <|> otherIdentChars

otherIdentChars :: Lexer Char
otherIdentChars = oneOf ['_', '.', '$', '@']

withSourcePos :: ULocLexer -> SourceLexer
withSourcePos = liftA2 withAnnot getSourcePos

int :: ULocLexer
int = T.IntLit <$> (signedInt <|> unsignedInt)

signedInt :: Lexer (Int, Bool)
signedInt = lexeme (char '-' *> ((, True) . negate <$> L.decimal))

unsignedInt :: Lexer (Int, Bool)
unsignedInt =
  lexeme $
  (char '0' *>
   ((oneOf ['x', 'X'] *> ((, False) <$> L.hexadecimal)) <|>
    (, False) <$> L.octal <|>
    (0, ) <$> unsignedSpec)) <|>
  decimal

decimal :: Lexer (Int, Bool)
decimal = liftA2 (,) L.decimal unsignedSpec

unsignedSpec :: Lexer Bool
unsignedSpec = isNothing <$> optional (oneOf ['u', 'U'])

float :: ULocLexer
float = T.FloatLit <$> lexeme L.float

token :: SourceLexer
token =
  withSourcePos $
  choice
    [ keyword T.abortsName $> T.Keyword T.Aborts
    , keyword T.alignName $> T.Keyword T.Align
    , keyword T.alignedName $> T.Keyword T.Aligned
    , keyword T.alsoName $> T.Keyword T.Also
    , keyword T.asName $> T.Keyword T.As
    , keyword T.autoName $> T.Keyword T.Auto
    , keyword T.bigName $> T.Keyword T.Big
    , keyword T.byteorderName $> T.Keyword T.Byteorder
    , keyword T.caseName $> T.Keyword T.Case
    , keyword T.className $> T.Keyword T.Class
    , keyword T.constName $> T.Keyword T.Const
    , keyword T.continuationName $> T.Keyword T.Continuation
    , keyword T.cutName $> T.Keyword T.Cut
    , keyword T.cutsName $> T.Keyword T.Cuts
    , keyword T.elseName $> T.Keyword T.Else
    , keyword T.equalName $> T.Keyword T.Equal
    , keyword T.exportName $> T.Keyword T.Export
    , keyword T.foreignName $> T.Keyword T.Foreign
    , keyword T.gotoName $> T.Keyword T.Goto
    , keyword T.ifName $> T.Keyword T.If
    , keyword T.importName $> T.Keyword T.Import
    , keyword T.inName $> T.Keyword T.In
    , keyword T.instanceName $> T.Keyword T.Instance
    , keyword T.invariantName $> T.Keyword T.Invariant
    , keyword T.invisibleName $> T.Keyword T.Invisible
    , keyword T.jumpName $> T.Keyword T.Jump
    , keyword T.littleName $> T.Keyword T.Little
    , keyword T.memsizeName $> T.Keyword T.Memsize
    , keyword T.neverName $> T.Keyword T.Never
    , keyword T.newName $> T.Keyword T.New
    , keyword T.ptrName $> T.Keyword T.Ptr
    , keyword T.pointersizeName $> T.Keyword T.Pointersize
    , keyword T.pragmaName $> T.Keyword T.Pragma
    , keyword T.readsName $> T.Keyword T.Reads
    , keyword T.registerName $> T.Keyword T.Register
    , keyword T.returnName $> T.Keyword T.Return
    , keyword T.returnsName $> T.Keyword T.Returns
    , keyword T.sectionName $> T.Keyword T.Section
    , keyword T.semiName $> T.Keyword T.Semi
    , keyword T.spanName $> T.Keyword T.Span
    , keyword T.stackdataName $> T.Keyword T.Stackdata
    , keyword T.structName $> T.Keyword T.Struct
    , keyword T.switchName $> T.Keyword T.Switch
    , keyword T.targetName $> T.Keyword T.Target
    , keyword T.targetsName $> T.Keyword T.Targets
    , keyword T.toName $> T.Keyword T.To
    , keyword T.typedefName $> T.Keyword T.Typedef
    , keyword T.unicodeName $> T.Keyword T.Unicode
    , keyword T.unwindsName $> T.Keyword T.Unwinds
    , keyword T.wordsizeName $> T.Keyword T.Wordsize
    , keyword T.writesName $> T.Keyword T.Writes
    , symbol "{" $> T.LBrace
    , symbol "}" $> T.RBrace
    , symbol "(" $> T.LParen
    , symbol ")" $> T.RParen
    , symbol "[" $> T.LBracket
    , symbol "]" $> T.RBracket
    , symbol "<" *> choice [symbol "<" $> T.ShL, symbol "=" $> T.Leq, pure T.Lt]
    , symbol ">" *> choice [symbol ">" $> T.ShR, symbol "=" $> T.Geq, pure T.Gt]
    , symbol "]" $> T.RBracket
    , symbol "," $> T.Comma
    , symbol ".." $> T.DotDot
    , symbol ":" *> choice [symbol ":" $> T.DColon, pure T.Colon]
    , symbol ";" $> T.Semicolon
    , symbol "`" $> T.Backtick
    , symbol "%" *> choice [symbol "%" $> T.DPercent, pure T.Percent]
    , symbol "~" $> T.Tilde
    , symbol "-" *> choice [symbol ">" $> T.Arr, pure T.Minus]
    , symbol "+" $> T.Plus
    , symbol "/" $> T.Slash
    , symbol "*" $> T.Star
    , symbol "&" $> T.Ampersand
    , symbol "|" $> T.Pipe
    , symbol "^" $> T.Caret
    , symbol "!=" $> T.Neq
    , symbol "=" *> choice [symbol "=" $> T.Eq, symbol ">" $> T.DArr, pure T.EqSign]
    , stringLiteral
    , charLiteral
    , try float
    , int
    , lexeme $
      string T.bitsName *>
      choice
        [ T.BitsType <$> L.decimal
        , T.Ident . T.pack . (T.bitsName ++) <$> identifierRest
        , pure $ T.Keyword T.Bits
        ]
    , identifier
    ]

tokenSafe :: SourceLexer
tokenSafe =
  token >?= \e -> registerParseError e *> anySingle *> tokenSafe

(>?=) :: MonadParsec e s m => m a
  -> (ParseError s e -> m a)
  -> m a
(>?=) = flip withRecovery

tokenize :: Lexer [Annot Token SourcePos]
tokenize = sc *> liftA2 snoc (many tokenSafe) (withSourcePos  $ T.Eof <$ eof)

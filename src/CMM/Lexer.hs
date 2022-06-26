{-# LANGUAGE Safe #-}

module CMM.Lexer
  ( Lexer
  , Reserved(..)
  , Token(..)
  , tokenize
  ) where

import safe CMM.AST.Annot (Annot, withAnnot)
import safe Control.Applicative
  ( Alternative((<|>))
  , Applicative((*>), (<*), liftA2, pure)
  , optional
  )
import safe Data.Bool (Bool(False, True))
import safe Data.Char (Char)
import safe Data.Eq (Eq)
import safe Data.Function (($), (.), flip)
import safe Data.Functor (Functor, ($>), (<$>), void)
import safe Data.Int (Int)
import safe Data.List ((++))
import safe Data.Maybe (isNothing)
import safe Data.Ord (Ord)
import safe Data.String (String, IsString)
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
import safe Text.Show (Show)

import safe CMM.Data.Float (Float)
import safe CMM.Data.Num (Num(negate))
import Data.Monoid
import CMM.Lexer.Token

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
  lexeme $ char '"' *> (StrLit . T.pack <$> manyTill L.charLiteral (char '"'))

charLiteral :: ULocLexer
charLiteral = lexeme $ char '\'' *> (CharLit <$> L.charLiteral) <* char '\''

identifier :: ULocLexer
identifier =
  lexeme $
  Ident . T.pack <$> liftA2 (:) (letterChar <|> otherIdentChars) identifierRest

identifierRest :: Lexer String
identifierRest = many $ alphaNumChar <|> otherIdentChars

otherIdentChars :: Lexer Char
otherIdentChars = oneOf ['_', '.', '$', '@']

withSourcePos :: ULocLexer -> SourceLexer
withSourcePos = liftA2 withAnnot getSourcePos

int :: ULocLexer
int = IntLit <$> (signedInt <|> unsignedInt)

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
float = FloatLit <$> lexeme L.float

token :: SourceLexer
token =
  withSourcePos $
  choice
    [ keyword abortsName $> Keyword Aborts
    , keyword alignName $> Keyword Align
    , keyword alignedName $> Keyword Aligned
    , keyword alsoName $> Keyword Also
    , keyword asName $> Keyword As
    , keyword autoName $> Keyword Auto
    , keyword bigName $> Keyword Big
    , keyword byteorderName $> Keyword Byteorder
    , keyword caseName $> Keyword Case
    , keyword className $> Keyword Class
    , keyword constName $> Keyword Const
    , keyword continuationName $> Keyword Continuation
    , keyword cutName $> Keyword Cut
    , keyword cutsName $> Keyword Cuts
    , keyword elseName $> Keyword Else
    , keyword equalName $> Keyword Equal
    , keyword exportName $> Keyword Export
    , keyword foreignName $> Keyword Foreign
    , keyword gotoName $> Keyword Goto
    , keyword ifName $> Keyword If
    , keyword importName $> Keyword Import
    , keyword inName $> Keyword In
    , keyword instanceName $> Keyword Instance
    , keyword invariantName $> Keyword Invariant
    , keyword invisibleName $> Keyword Invisible
    , keyword jumpName $> Keyword Jump
    , keyword littleName $> Keyword Little
    , keyword memsizeName $> Keyword Memsize
    , keyword neverName $> Keyword Never
    , keyword pointersizeName $> Keyword Pointersize
    , keyword pragmaName $> Keyword Pragma
    , keyword readsName $> Keyword Reads
    , keyword registerName $> Keyword Register
    , keyword returnName $> Keyword Return
    , keyword returnsName $> Keyword Returns
    , keyword sectionName $> Keyword Section
    , keyword semiName $> Keyword Semi
    , keyword spanName $> Keyword Span
    , keyword stackdataName $> Keyword Stackdata
    , keyword structName $> Keyword Struct
    , keyword switchName $> Keyword Switch
    , keyword targetName $> Keyword Target
    , keyword targetsName $> Keyword Targets
    , keyword toName $> Keyword To
    , keyword typedefName $> Keyword Typedef
    , keyword unicodeName $> Keyword Unicode
    , keyword unwindsName $> Keyword Unwinds
    , keyword wordsizeName $> Keyword Wordsize
    , keyword writesName $> Keyword Writes
    , symbol "{" $> LBrace
    , symbol "}" $> RBrace
    , symbol "(" $> LParen
    , symbol ")" $> RParen
    , symbol "[" $> LBracket
    , symbol "]" $> RBracket
    , symbol "<" *> choice [symbol "<" $> ShL, symbol "=" $> Leq, pure Lt]
    , symbol ">" *> choice [symbol ">" $> ShR, symbol "=" $> Geq, pure Gt]
    , symbol "]" $> RBracket
    , symbol "," $> Comma
    , symbol ".." $> DotDot
    , symbol ":" *> choice [symbol ":" $> DColon, pure Colon]
    , symbol ";" $> Semicolon
    , symbol "`" $> Backtick
    , symbol "%" *> choice [symbol "%" $> DPercent, pure Percent]
    , symbol "~" $> Tilde
    , symbol "-" *> choice [symbol ">" $> Arr, pure Minus]
    , symbol "+" $> Plus
    , symbol "/" $> Slash
    , symbol "*" $> Star
    , symbol "&" $> Ampersand
    , symbol "|" $> Pipe
    , symbol "^" $> Caret
    , symbol "!=" $> Neq
    , symbol "=" *> choice [symbol "=" $> Eq, symbol ">" $> DArr, pure EqSign]
    , stringLiteral
    , charLiteral
    , try float
    , int
    , lexeme $
      string bitsName *>
      choice
        [ BitsType <$> L.decimal
        , Ident . T.pack . (bitsName ++) <$> identifierRest
        , pure $ Keyword Bits
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
tokenize = sc *> manyTill tokenSafe eof

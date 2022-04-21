{-# LANGUAGE Safe #-}

module CMM.Lexer
  ( Lexer
  , Reserved(..)
  , Token(..)
  , tokenize
  ) where

import safe Prelude (Float, Num(negate))

import safe CMM.AST.Annot (Annot, withAnnot)
import safe Control.Applicative
  ( Alternative((<|>))
  , Applicative((*>), (<*), liftA2, pure)
  , optional
  )
import safe Data.Bool (Bool(False, True))
import safe Data.Char (Char)
import safe Data.Eq (Eq)
import safe Data.Function (($), (.))
import safe Data.Functor (Functor, ($>), (<$>), void)
import safe Data.Int (Int)
import safe Data.List ((++))
import safe Data.Maybe (isNothing)
import safe Data.Ord (Ord)
import safe Data.String (String)
import safe qualified Data.Text as T
import safe Data.Text (Text)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(eof, notFollowedBy, try)
  , Parsec
  , SourcePos
  , choice
  , getSourcePos
  , many
  , manyTill
  , oneOf
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

type Lexer = Parsec Void Text

data Reserved
  = Aborts
  | Align
  | Aligned
  | Also
  | As
  | Auto
  | Big
  | Bits
  | Byteorder
  | Case
  | Class
  | Const
  | Continuation
  | Cut
  | Cuts
  | Else
  | Equal
  | Export
  | Foreign
  | Goto
  | If
  | Import
  | In
  | Instance
  | Invariant
  | Invisible
  | Jump
  | Little
  | Memsize
  | Never
  | Pointersize
  | Pragma
  | Reads
  | Register
  | Return
  | Returns
  | Section
  | Semi
  | Span
  | Stackdata
  | Struct
  | Switch
  | Target
  | Targets
  | To
  | Typedef
  | Unicode
  | Unwinds
  | Writes
  | Wordsize
  deriving (Eq, Ord, Show)

data Token a
  = Keyword Reserved
  | Ident Text
  | StrLit Text
  | BitsType Int
  | CharLit Char
  | FloatLit Float
  | IntLit (Int, Bool)
  | Arr
  | DArr
  | Colon
  | DColon
  | Semicolon
  | LBrace
  | RBrace
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Lt
  | Gt
  | Leq
  | Geq
  | ShL
  | ShR
  | Eq
  | Neq
  | Comma
  | Backtick
  | Percent
  | DPercent
  | Tilde
  | Minus
  | Plus
  | Slash
  | Star
  | Ampersand
  | Pipe
  | EqSign
  | DotDot
  | Caret
  deriving (Functor, Eq, Ord, Show)

type SourceLexer = Lexer (Annot Token SourcePos)

type ULocLexer = Lexer (Token SourcePos)

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

keyword :: Text -> Lexer ()
keyword k = void . lexeme $ try (string k <* notFollowedBy alphaNumChar)

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
    [ keyword "aborts" $> Keyword Aborts
    , keyword "align" $> Keyword Align
    , keyword "aligned" $> Keyword Aligned
    , keyword "also" $> Keyword Also
    , keyword "as" $> Keyword As
    , keyword "auto" $> Keyword Auto
    , keyword "big" $> Keyword Big
    , keyword "byteorder" $> Keyword Byteorder
    , keyword "case" $> Keyword Case
    , keyword "class" $> Keyword Class
    , keyword "const" $> Keyword Const
    , keyword "continuation" $> Keyword Continuation
    , keyword "cut" $> Keyword Cut
    , keyword "cuts" $> Keyword Cuts
    , keyword "else" $> Keyword Else
    , keyword "equal" $> Keyword Equal
    , keyword "export" $> Keyword Export
    , keyword "foreign" $> Keyword Foreign
    , keyword "goto" $> Keyword Goto
    , keyword "if" $> Keyword If
    , keyword "import" $> Keyword Import
    , keyword "in" $> Keyword In
    , keyword "instance" $> Keyword Instance
    , keyword "invariant" $> Keyword Invariant
    , keyword "invisible" $> Keyword Invisible
    , keyword "jump" $> Keyword Jump
    , keyword "little" $> Keyword Little
    , keyword "memsize" $> Keyword Memsize
    , keyword "never" $> Keyword Never
    , keyword "pointersize" $> Keyword Pointersize
    , keyword "pragma" $> Keyword Pragma
    , keyword "reads" $> Keyword Reads
    , keyword "register" $> Keyword Register
    , keyword "return" $> Keyword Return
    , keyword "returns" $> Keyword Returns
    , keyword "section" $> Keyword Section
    , keyword "semi" $> Keyword Semi
    , keyword "span" $> Keyword Span
    , keyword "stackdata" $> Keyword Stackdata
    , keyword "struct" $> Keyword Struct
    , keyword "switch" $> Keyword Switch
    , keyword "target" $> Keyword Target
    , keyword "targets" $> Keyword Targets
    , keyword "to" $> Keyword To
    , keyword "typedef" $> Keyword Typedef
    , keyword "unicode" $> Keyword Unicode
    , keyword "unwinds" $> Keyword Unwinds
    , keyword "wordsize" $> Keyword Wordsize
    , keyword "writes" $> Keyword Writes
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
      string "bits" *>
      choice
        [ BitsType <$> L.decimal
        , Ident . T.pack . ("bits" ++) <$> identifierRest
        , pure $ Keyword Bits
        ]
    , identifier
    ]

tokenize :: Lexer [Annot Token SourcePos]
tokenize = sc *> manyTill token eof

{-# LANGUAGE Safe #-}

module CMM.Lexer.Token where

import safe Data.String ( IsString )
import safe Data.Text ( Text )
import safe Prettyprinter
    ( Pretty(pretty), hsep, dquotes, squotes )

import safe CMM.AST.Annot ( takeAnnot, Annot )
import safe Text.Megaparsec
    ( SourcePos,
      TraversableStream(reachOffset),
      VisualStream (showTokens, tokensLength),
      PosState(PosState) )
import safe Data.List.NonEmpty ( toList )

-- | Tokens for the reserved keywords
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
  | New
  | Ptr
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

instance Pretty Reserved where
  pretty = \case
    Aborts -> abortsName
    Align -> alignName
    Aligned -> alignedName
    Also -> alsoName
    As -> asName
    Auto -> autoName
    Big -> bigName
    Bits -> bitsName
    Byteorder -> byteorderName
    Case -> caseName
    Class -> className
    Const -> constName
    Continuation -> continuationName
    Cut -> cutName
    Cuts -> cutsName
    Else -> elseName
    Equal -> equalName
    Export -> exportName
    Foreign -> foreignName
    Goto -> gotoName
    If -> ifName
    Import -> importName
    In -> inName
    Instance -> instanceName
    Invariant -> invariantName
    Invisible -> invisibleName
    Jump -> jumpName
    Little -> littleName
    Memsize -> memsizeName
    Never -> neverName
    New -> newName
    Ptr -> ptrName
    Pointersize -> pointersizeName
    Pragma -> pragmaName
    Reads -> readsName
    Register -> registerName
    Return -> returnName
    Returns -> returnsName
    Section -> sectionName
    Semi -> semiName
    Span -> spanName
    Stackdata -> stackdataName
    Struct -> structName
    Switch -> switchName
    Target -> targetName
    Targets -> targetsName
    To -> toName
    Typedef -> typedefName
    Unicode -> unicodeName
    Unwinds -> unwindsName
    Writes -> writesName
    Wordsize -> wordsizeName

-- | All the tokens of the CHMMM language
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
  | Eof
  deriving (Functor, Eq, Ord, Show)


instance Pretty (Token a) where
  pretty = \case
    Keyword kw -> pretty kw
    Ident name -> pretty name
    StrLit txt -> dquotes $ pretty txt
    BitsType int -> bitsName <> pretty int
    CharLit char -> squotes $ pretty char
    FloatLit float -> pretty float
    IntLit int -> pretty int
    Arr -> "->"
    DArr -> "=>"
    Colon -> ":"
    DColon -> "::"
    Semicolon -> ";"
    LBrace -> "{"
    RBrace -> "}"
    LParen -> "("
    RParen -> ")"
    LBracket -> "["
    RBracket -> "]"
    Lt -> "<"
    Gt -> ">"
    Leq -> "<="
    Geq -> ">="
    ShL -> "<<"
    ShR -> ">>"
    Eq -> "=="
    Neq -> "!="
    Comma -> ","
    Backtick -> "`"
    Percent -> "%"
    DPercent -> "%%"
    Tilde -> "~"
    Minus -> "-"
    Plus -> "+"
    Slash -> "/"
    Star -> "*"
    Ampersand -> "&"
    Pipe -> "|"
    EqSign -> "="
    DotDot -> ".."
    Caret -> "^"
    Eof -> ""

instance Ord a => VisualStream [Annot Token a] where
  showTokens _ = show . hsep . fmap pretty . toList
  tokensLength proxy = length . showTokens proxy

instance TraversableStream [Annot Token SourcePos] where
  reachOffset int = \case
    state@(PosState ts n _ pos s) -> go ts n
      where
        go [] _ = (Nothing, state)
        go tokens'@(token':tokens'') n'
          | n'' <= int = go tokens'' n''
          | otherwise = (Just $ s <> string, PosState tokens' n' (takeAnnot token') pos "")
          where
            n'' = len + n'
            string = show $ pretty token'
            len = length string



abortsName :: IsString a => a
abortsName = "aborts"

alignName :: IsString a => a
alignName = "align"

alignedName :: IsString a => a
alignedName = "aligned"

alsoName :: IsString a => a
alsoName = "also"

asName :: IsString a => a
asName = "as"

autoName :: IsString a => a
autoName = "auto"

boolName :: IsString a => a
boolName = "bool"

voidName :: IsString a => a
voidName = "void"

labelName :: IsString a => a
labelName = "label"

bigName :: IsString a => a
bigName = "big"

bitsName :: IsString a => a
bitsName = "bits"

byteorderName :: IsString a => a
byteorderName = "byteorder"

caseName :: IsString a => a
caseName = "case"

className :: IsString a => a
className = "class"

constName :: IsString a => a
constName = "const"

continuationName :: IsString a => a
continuationName = "continuation"

cutName :: IsString a => a
cutName = "cut"

cutsName :: IsString a => a
cutsName = "cuts"

elseName :: IsString a => a
elseName = "else"

equalName :: IsString a => a
equalName = "equal"

exportName :: IsString a => a
exportName = "export"

foreignName :: IsString a => a
foreignName = "foreign"

gotoName :: IsString a => a
gotoName = "goto"

ifName :: IsString a => a
ifName = "if"

importName :: IsString a => a
importName = "import"

inName :: IsString a => a
inName = "in"

instanceName :: IsString a => a
instanceName = "instance"

invariantName :: IsString a => a
invariantName = "invariant"

invisibleName :: IsString a => a
invisibleName = "invisible"

jumpName :: IsString a => a
jumpName = "jump"

littleName :: IsString a => a
littleName = "little"

memsizeName :: IsString a => a
memsizeName = "memsize"

neverName :: IsString a => a
neverName = "never"

newName :: IsString a => a
newName = "new"

ptrName :: IsString a => a
ptrName = "ptr"

pointersizeName :: IsString a => a
pointersizeName = "pointersize"

pragmaName :: IsString a => a
pragmaName = "pragma"

readsName :: IsString a => a
readsName = "reads"

registerName :: IsString a => a
registerName = "register"

returnName :: IsString a => a
returnName = "return"

returnsName :: IsString a => a
returnsName = "returns"

sectionName :: IsString a => a
sectionName = "section"

semiName :: IsString a => a
semiName = "semi"

spanName :: IsString a => a
spanName = "span"

stackdataName :: IsString a => a
stackdataName = "stackdata"

structName :: IsString a => a
structName = "struct"

switchName :: IsString a => a
switchName = "switch"

targetName :: IsString a => a
targetName = "target"

targetsName :: IsString a => a
targetsName = "targets"

toName :: IsString a => a
toName = "to"

typedefName :: IsString a => a
typedefName = "typedef"

unicodeName :: IsString a => a
unicodeName = "unicode"

unwindsName :: IsString a => a
unwindsName = "unwinds"

wordsizeName :: IsString a => a
wordsizeName = "wordsize"

writesName :: IsString a => a
writesName = "writes"

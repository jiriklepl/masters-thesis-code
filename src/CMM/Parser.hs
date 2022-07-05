{-# LANGUAGE Safe #-}

module CMM.Parser where

import safe Control.Applicative
  ( Alternative((<|>))
  , Applicative(liftA2)
  , (<**>)
  , liftA2
  , liftA3
  , optional
  )
import safe Data.Functor (($>))
import safe Data.Maybe (fromMaybe, isJust)
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Void (Void)
import safe Text.Megaparsec
  ( MonadParsec(lookAhead, token, try)
  , Parsec
  , SourcePos
  , between
  , choice
  , many
  , sepBy1
  , sepEndBy
  , sepEndBy1
  )

import safe qualified CMM.AST as AST
import safe CMM.AST.Annot (Annot, Annotation(Annot), withAnnot)
import safe CMM.Control.Applicative ((<*<), (>*>), liftA4, liftA6)
import safe qualified CMM.Lexer.Token as T

type Parser = Parsec Void [Annot T.Token SourcePos]

type SourceParser a = Parser (Annot a SourcePos)

type ULocParser a = Parser (a SourcePos)

optionalL :: Parser [a] -> Parser [a]
optionalL = (fromMaybe [] <$>) . optional

keyword :: T.Reserved -> Parser T.Reserved
keyword name =
  flip token Set.empty $ \case
    Annot (T.Keyword name') _ ->
      if name == name'
        then Just name
        else Nothing
    _ -> Nothing

keywords :: [T.Reserved] -> Parser T.Reserved
keywords = foldl1 (*>) . (keyword <$>)

symbol :: T.Token SourcePos -> Parser ()
symbol tok =
  flip token Set.empty $ \case
    Annot tok' _ ->
      if tok == tok'
        then Just ()
        else Nothing

identifier :: Parser (AST.Name SourcePos)
identifier =
  flip token Set.empty $ \case
    Annot (T.Ident n) _ -> Just $ AST.Name n
    _ -> Nothing

stringLiteral :: Parser AST.StrLit
stringLiteral =
  flip token Set.empty $ \case
    Annot (T.StrLit t) _ -> Just $ AST.StrLit t
    _ -> Nothing

bitsType :: Parser Int
bitsType =
  flip token Set.empty $ \case
    Annot (T.BitsType i) _ -> Just i
    _ -> Nothing

charLit :: Parser Char
charLit =
  flip token Set.empty $ \case
    Annot (T.CharLit c) _ -> Just c
    _ -> Nothing

floatLit :: Parser Float
floatLit =
  flip token Set.empty $ \case
    Annot (T.FloatLit f) _ -> Just f
    _ -> Nothing

intLit :: Parser (Int, Bool)
intLit =
  flip token Set.empty $ \case
    Annot (T.IntLit i) _ -> Just i
    _ -> Nothing

getPos :: Parser SourcePos
getPos =
  lookAhead $
  flip token Set.empty $ \case
    Annot _ pos -> Just pos

withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 withAnnot getPos

identifiers :: Parser [Annot AST.Name SourcePos]
identifiers = commaList $ withSourcePos identifier

braces :: Parser a -> Parser a
braces = between (symbol T.LBrace) (symbol T.RBrace)

parens :: Parser a -> Parser a
parens = between (symbol T.LParen) (symbol T.RParen)

brackets :: Parser a -> Parser a
brackets = between (symbol T.LBracket) (symbol T.RBracket)

angles :: Parser a -> Parser a
angles = between (symbol T.Lt) (symbol T.Gt)

commaList :: Parser a -> Parser [a]
commaList = (`sepEndBy1` comma)

program :: SourceParser AST.Unit
program = unit <* symbol T.Eof

comma :: Parser ()
comma = symbol T.Comma

colon :: Parser ()
colon = symbol T.Colon

semicolon :: Parser ()
semicolon = symbol T.Semicolon

-- | Parses the whole 'Unit'
unit :: SourceParser AST.Unit
unit = withSourcePos $ AST.Unit <$> many topLevel

topLevel :: SourceParser AST.TopLevel
topLevel =
  withSourcePos $
  choice
    [ sectionTopLevel
    , classTopLevel
    , instanceTopLevel
    , structTopLevel
    , AST.TopDecl <$> decl
    , AST.TopProcedure <$> try procedure
    ]

sectionTopLevel :: ULocParser AST.TopLevel
sectionTopLevel =
  keyword T.Section *> liftA2 AST.TopSection stringLiteral (braces $ many section)

classTopLevel :: ULocParser AST.TopLevel
classTopLevel = keyword T.Class *> (AST.TopClass <$> class')

class' :: SourceParser AST.Class
class' =
  withSourcePos $
  choice
    [ try $
      liftA3 AST.Class (sepBy1 paraName comma <* symbol T.DArr) paraName classBody
    , liftA2 (AST.Class []) paraName classBody
    ]
  where
    classBody =
      braces $
      many (withSourcePos $ AST.ProcedureDecl <$> procedureHeader <* semicolon)

instanceTopLevel :: ULocParser AST.TopLevel
instanceTopLevel = keyword T.Instance *> (AST.TopInstance <$> instance')

instance' :: SourceParser AST.Instance
instance' =
  withSourcePos $
  choice
    [ try $
      liftA3
        AST.Instance
        (sepBy1 paraName comma <* symbol T.DArr)
        paraName
        (braces $ many procedureInstance)
    , liftA2 (AST.Instance []) paraName (braces $ many procedureInstance)
    ]

structTopLevel :: ULocParser AST.TopLevel
structTopLevel = keyword T.Struct *> (AST.TopStruct <$> struct)

struct :: SourceParser AST.Struct
struct = withSourcePos $ liftA2 AST.Struct paraName (braces $ many datum)

class ParaName' param where
  paraName :: SourceParser (AST.ParaName param)

instance ParaName' AST.Type where
  paraName = withSourcePos $ liftA2 AST.ParaName identifier (many type')

instance ParaName' AST.Name where
  paraName =
    withSourcePos $ liftA2 AST.ParaName identifier (many $ withSourcePos identifier)

section :: SourceParser AST.Section
section =
  withSourcePos $
  choice
    [ AST.SecDecl <$> decl
    , secSpan
    , AST.SecProcedure <$> try procedure
    , AST.SecDatum <$> datum
    ]

decl :: SourceParser AST.Decl
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

importDecl :: ULocParser AST.Decl
importDecl = keyword T.Import *> (AST.ImportDecl <$> commaList import') <* semicolon

exportDecl :: ULocParser AST.Decl
exportDecl = keyword T.Export *> (AST.ExportDecl <$> commaList export) <* semicolon

constDecl :: ULocParser AST.Decl
constDecl =
  keyword T.Const *>
  liftA2
    (uncurry AST.ConstDecl)
    (try (liftA2 (,) (optional type') identifier) <|> (Nothing, ) <$> identifier)
    (symbol T.EqSign *> expr) <*
  semicolon

typedefDecl :: ULocParser AST.Decl
typedefDecl =
  keyword T.Typedef *> liftA2 AST.TypedefDecl type' identifiers <* semicolon

pragmaDecl :: ULocParser AST.Decl
pragmaDecl = keyword T.Pragma *> liftA2 AST.PragmaDecl identifier (braces pragma)

targetDecl :: ULocParser AST.Decl
targetDecl =
  keyword T.Target *> (AST.TargetDecl <$> many targetDirective) <* semicolon

targetDirective :: SourceParser AST.TargetDirective
targetDirective =
  withSourcePos $
  choice
    [ memSizeDirective
    , byteOrderDirective
    , pointerSizeDirective
    , wordSizeDirective
    ]

registerDecl :: ULocParser AST.Decl
registerDecl = liftA2 AST.RegDecl invariant registers <* semicolon

memSizeDirective :: ULocParser AST.TargetDirective
memSizeDirective = keyword T.Memsize *> (AST.MemSize . fst <$> intLit)

byteOrderDirective :: ULocParser AST.TargetDirective
byteOrderDirective = keyword T.Byteorder *> (AST.ByteOrder <$> endian)

endian :: Parser AST.Endian
endian = choice [keyword T.Little $> AST.Little, keyword T.Big $> AST.Big]

pointerSizeDirective :: ULocParser AST.TargetDirective
pointerSizeDirective = keyword T.Pointersize *> (AST.PointerSize . fst <$> intLit)

wordSizeDirective :: ULocParser AST.TargetDirective
wordSizeDirective = keyword T.Wordsize *> (AST.WordSize . fst <$> intLit)

procedureHeader :: SourceParser AST.ProcedureHeader
procedureHeader =
  withSourcePos $
  liftA4
    AST.ProcedureHeader
    (optional convention)
    identifier
    formals
    (optional $ symbol T.Arr *> sepEndBy semiFormal comma)

procedureInstanceHeader :: SourceParser AST.ProcedureHeader
procedureInstanceHeader =
  withSourcePos $
  liftA4
    AST.ProcedureHeader
    (optional convention)
    identifier
    formalsInstance
    (optional $ symbol T.Arr *> sepEndBy semiFormalInstance comma)

procedure :: SourceParser AST.Procedure
procedure = withSourcePos $ liftA2 AST.Procedure procedureHeader body

procedureInstance :: SourceParser AST.Procedure
procedureInstance = withSourcePos $ liftA2 AST.Procedure procedureInstanceHeader body

formal :: SourceParser AST.Formal
formal = withSourcePos $ liftA4 AST.Formal mKind invariant type' identifier

formalInstance :: SourceParser AST.Formal
formalInstance = withSourcePos $ liftA4 AST.Formal mKind invariant justAutoType identifier

semiFormal :: SourceParser AST.SemiFormal
semiFormal = withSourcePos $ liftA2 AST.SemiFormal mKind type'

semiFormalInstance :: SourceParser AST.SemiFormal
semiFormalInstance = withSourcePos $ liftA2 AST.SemiFormal mKind justAutoType

formals :: Parser [Annot AST.Formal SourcePos]
formals = parens $ formal `sepEndBy` comma

formalsInstance :: Parser [Annot AST.Formal SourcePos]
formalsInstance = parens $ formalInstance `sepEndBy` comma

invariant :: Parser Bool
invariant = keyword T.Invariant $> True <|> pure False

actual :: SourceParser AST.Actual
actual = withSourcePos $ liftA2 AST.Actual mKind expr

actuals :: Parser [Annot AST.Actual SourcePos]
actuals = parens $ actual `sepEndBy` comma

convention :: Parser AST.Conv
convention = keyword T.Foreign *> (AST.Foreign <$> stringLiteral)

import' :: SourceParser AST.Import
import' =
  withSourcePos $
  liftA2 AST.Import (optional (stringLiteral <* keyword T.As)) identifier

export :: SourceParser AST.Export
export =
  withSourcePos $
  liftA2 AST.Export identifier (optional (keyword T.As *> stringLiteral))

body :: SourceParser AST.Body
body = withSourcePos . braces $ AST.Body <$> many bodyItem

bodyItem :: SourceParser AST.BodyItem
bodyItem =
  withSourcePos $
  AST.BodyDecl <$> try decl <|> AST.BodyStackDecl <$> stackDecl <|> AST.BodyStmt <$> stmt

secSpan :: ULocParser AST.Section
secSpan = keyword T.Span *> liftA3 AST.SecSpan expr expr (braces $ many section)

datum :: SourceParser AST.Datum
datum = withSourcePos $ choice [alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser AST.Datum
alignDatum = keyword T.Align *> (AST.DatumAlign . fst <$> intLit) <* semicolon

labelDatum :: ULocParser AST.Datum
labelDatum = AST.DatumLabel <$> identifier <* colon

justDatum :: ULocParser AST.Datum
justDatum = liftA4 AST.Datum (fmap isJust . optional $ keyword T.New) type' (optional size) (optional init') <* semicolon

init' :: SourceParser AST.Init
init' = withSourcePos $ choice [stringInit, string16Init, initList]

initList :: ULocParser AST.Init
initList = braces $ AST.ExprInit <$> commaList expr

stringInit :: ULocParser AST.Init
stringInit = AST.StrInit <$> stringLiteral

string16Init :: ULocParser AST.Init
string16Init = keyword T.Unicode *> (AST.Str16Init <$> parens stringLiteral)

size :: SourceParser AST.Size
size = withSourcePos . brackets $ AST.Size <$> optional expr

registers :: SourceParser AST.Registers
registers =
  withSourcePos $
  liftA3
    AST.Registers
    mKind
    type'
    (commaList
       (liftA2
          (,)
          (withSourcePos identifier)
          (optional $ symbol T.EqSign *> stringLiteral)))

type' :: SourceParser AST.Type
type' = withSourcePos $ choice [parensType, ptrType, autoType, bitsType', nameType]

parensType :: ULocParser AST.Type
parensType = AST.TPar <$> parens paraType

autoType :: ULocParser AST.Type
autoType = AST.TAuto <$> (keyword T.Auto *> optional (parens identifier))

justAutoType :: SourceParser AST.Type
justAutoType = withSourcePos $ AST.TAuto Nothing <$ keyword T.Auto

ptrType :: ULocParser AST.Type
ptrType = keyword T.Ptr *> (AST.TPtr <$> withSourcePos parensType)

bitsType' :: ULocParser AST.Type
bitsType' = AST.TBits <$> bitsType

nameType :: ULocParser AST.Type
nameType = AST.TName <$> identifier

paraType :: SourceParser AST.ParaType
paraType = withSourcePos $ liftA2 AST.ParaType type' (many type')

mKind :: Parser (Maybe AST.Kind)
mKind = optional $ AST.Kind <$> stringLiteral

pragma :: Parser a
pragma = error "Pragmas not specified" -- FIXME: pragmas not yet specified and with no explanation of functionality

stackDecl :: SourceParser AST.StackDecl
stackDecl =
  withSourcePos $ keyword T.Stackdata *> braces (AST.StackDecl <$> many datum)

stmt :: SourceParser AST.Stmt
stmt =
  withSourcePos $
  choice
    [ emptyStmt
    , ifStmt
    , switchStmt
    , spanStmt
    , jumpStmt
    , returnStmt
    , gotoStmt
    , contStmt
    , cutToStmt
    , try assignStmt
    , try primOpStmt
    , try labelStmt
    , callStmt
    ]

emptyStmt :: ULocParser AST.Stmt
emptyStmt = semicolon $> AST.EmptyStmt

ifStmt :: ULocParser AST.Stmt
ifStmt =
  keyword T.If *> liftA3 AST.IfStmt expr body (optional $ keyword T.Else *> body)

switchStmt :: ULocParser AST.Stmt
switchStmt = keyword T.Switch *> liftA2 AST.SwitchStmt expr (braces (many arm))

spanStmt :: ULocParser AST.Stmt
spanStmt = keyword T.Span *> liftA3 AST.SpanStmt expr expr body

assignStmt :: ULocParser AST.Stmt
assignStmt =
  liftA2 AST.AssignStmt (commaList lvalue <* symbol T.EqSign) (commaList expr) <*
  semicolon

primOpStmt :: ULocParser AST.Stmt
primOpStmt =
  liftA4
    AST.PrimOpStmt
    (identifier <* symbol T.EqSign)
    (symbol T.DPercent *> identifier)
    (optionalL actuals)
    (many flow) <*
  semicolon

callStmt :: ULocParser AST.Stmt
callStmt =
  liftA6
    AST.CallStmt
    (optionalL kindedNames <* symbol T.EqSign)
    (optional convention)
    expr
    actuals
    (optional targets)
    (many . withSourcePos $ AST.FlowAnnot <$> flow <|> AST.AliasAnnot <$> alias) <*
  semicolon

jumpStmt :: ULocParser AST.Stmt
jumpStmt =
  liftA4
    AST.JumpStmt
    (optional convention <* keyword T.Jump)
    expr
    (optionalL actuals)
    (optional targets) <*
  semicolon

returnStmt :: ULocParser AST.Stmt
returnStmt =
  liftA3
    AST.ReturnStmt
    (optional convention <* keyword T.Return)
    (optional . angles $
     liftA2 (,) (restrictedExpr <* symbol T.Slash) restrictedExpr)
    (optionalL actuals) <*
  semicolon

lvalue :: SourceParser AST.LValue
lvalue = withSourcePos $ try lvRef <|> lvName

lvRef :: ULocParser AST.LValue
lvRef =
  liftA3 AST.LVRef (optional type') (symbol T.LBracket *> expr) (optional asserts) <*
  symbol T.RBracket

lvName :: ULocParser AST.LValue
lvName = AST.LVName <$> identifier

asserts :: Parser (Annot AST.Asserts SourcePos)
asserts = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser AST.Asserts
alignAssert =
  liftA2
    AST.AlignAssert
    (keyword T.Aligned *> (fst <$> intLit))
    (optionalL $ keyword T.In *> (withSourcePos identifier `sepBy1` comma))

inAssert :: ULocParser AST.Asserts
inAssert =
  liftA2
    AST.InAssert
    (keyword T.In *> (withSourcePos identifier `sepBy1` comma))
    (optional (keyword T.Aligned *> (fst <$> intLit)))

labelStmt :: ULocParser AST.Stmt
labelStmt = AST.LabelStmt <$> identifier <* colon

contStmt :: ULocParser AST.Stmt
contStmt =
  keyword T.Continuation *>
  liftA2 AST.ContStmt identifier (parens $ optionalL kindedNames) <*
  colon

gotoStmt :: ULocParser AST.Stmt
gotoStmt =
  keyword T.Goto *> liftA2 AST.GotoStmt expr (optional targets) <* semicolon

cutToStmt :: ULocParser AST.Stmt
cutToStmt =
  keywords [T.Cut, T.To] *> liftA3 AST.CutToStmt expr actuals (many flow) <*
  semicolon

kindedNames :: Parser [Annot AST.KindName SourcePos]
kindedNames = commaList . withSourcePos $ liftA2 AST.KindName mKind identifier

arm :: SourceParser AST.Arm
arm =
  withSourcePos $ keyword T.Case *> liftA2 AST.Arm (commaList range <* colon) body

range :: SourceParser AST.Range
range = withSourcePos $ liftA2 AST.Range expr (optional $ symbol T.DotDot *> expr)

flow :: SourceParser AST.Flow
flow = withSourcePos $ alsoFlow <|> neverReturns

alsoFlow :: ULocParser AST.Flow
alsoFlow =
  keyword T.Also *>
  choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

alsoCutsTo :: ULocParser AST.Flow
alsoCutsTo = keywords [T.Cuts, T.To] *> (AST.AlsoCutsTo <$> identifiers)

alsoUnwindsTo :: ULocParser AST.Flow
alsoUnwindsTo = keywords [T.Unwinds, T.To] *> (AST.AlsoUnwindsTo <$> identifiers)

alsoReturnsTo :: ULocParser AST.Flow
alsoReturnsTo = keywords [T.Returns, T.To] *> (AST.AlsoReturnsTo <$> identifiers)

alsoAborts :: ULocParser AST.Flow
alsoAborts = keyword T.Aborts *> optional comma $> AST.AlsoAborts

neverReturns :: ULocParser AST.Flow
neverReturns = keywords [T.Never, T.Returns] *> optional comma $> AST.NeverReturns

alias :: SourceParser AST.Alias
alias = withSourcePos $ readsAlias <|> writesAlias

readsAlias :: ULocParser AST.Alias
readsAlias = keyword T.Reads *> (AST.Reads <$> identifiers)

writesAlias :: ULocParser AST.Alias
writesAlias = keyword T.Writes *> (AST.Writes <$> identifiers)

targets :: SourceParser AST.Targets
targets = withSourcePos $ keyword T.Targets *> (AST.Targets <$> identifiers)

expr :: SourceParser AST.Expr
expr = infixExpr

simpleExpr :: SourceParser AST.Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

restrictedExpr :: SourceParser AST.Expr
restrictedExpr = choice [litExpr, parExpr, nameExpr]

litExpr :: SourceParser AST.Expr
litExpr =
  withSourcePos $
  liftA2
    AST.LitExpr
    (withSourcePos $ choice [charExpr, floatExpr, intExpr])
    (optional $ symbol T.DColon *> type')

intExpr :: ULocParser AST.Lit
intExpr = AST.LitInt . fst <$> intLit

floatExpr :: ULocParser AST.Lit
floatExpr = AST.LitFloat <$> floatLit

charExpr :: ULocParser AST.Lit
charExpr = AST.LitChar <$> charLit

nameExpr :: SourceParser AST.Expr
nameExpr = withSourcePos $ AST.LVExpr <$> withSourcePos lvName

refExpr :: SourceParser AST.Expr
refExpr = withSourcePos $ AST.LVExpr <$> withSourcePos lvRef

parExpr :: SourceParser AST.Expr
parExpr = withSourcePos $ AST.ParExpr <$> parens expr

-- SYMBOLIC OPERATORS --
-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4.1)
infixExpr :: SourceParser AST.Expr
infixExpr = opImplN ("`" :: Text) cmpExpr

cmpExpr :: SourceParser AST.Expr
cmpExpr =
  opImplN
    [ (AST.GeOp, T.Geq :: T.Token SourcePos)
    , (AST.GtOp, T.Gt)
    , (AST.LeOp, T.Leq)
    , (AST.LtOp, T.Lt)
    , (AST.NeqOp, T.Neq)
    , (AST.EqOp, T.Eq)
    ]
    orExpr

orExpr :: SourceParser AST.Expr
orExpr = opImplL (AST.OrOp, T.Pipe :: T.Token SourcePos) xorExpr

xorExpr :: SourceParser AST.Expr
xorExpr = opImplL (AST.XorOp, T.Caret :: T.Token SourcePos) andExpr

andExpr :: SourceParser AST.Expr
andExpr = opImplL (AST.AndOp, T.Ampersand :: T.Token SourcePos) shExpr

shExpr :: SourceParser AST.Expr
shExpr = opImplL [(AST.ShLOp, T.ShL :: T.Token SourcePos), (AST.ShROp, T.ShR)] addExpr

addExpr :: SourceParser AST.Expr
addExpr =
  opImplL [(AST.AddOp, T.Plus :: T.Token SourcePos), (AST.SubOp, T.Minus)] mulExpr

mulExpr :: SourceParser AST.Expr
mulExpr =
  opImplL
    [(AST.DivOp, T.Slash :: T.Token SourcePos), (AST.MulOp, T.Star), (AST.ModOp, T.Percent)]
    negExpr

-- SYMBOLIC OPERATORS -- END
negExpr :: SourceParser AST.Expr
negExpr =
  withSourcePos (symbol T.Minus *> (AST.NegExpr <$> negExpr)) <|>
  withSourcePos (symbol T.Tilde *> (AST.ComExpr <$> negExpr)) <|>
  memberExpr

memberExpr :: SourceParser AST.Expr
memberExpr = do
  expr' <- simpleExpr
  choice
    [ symbol T.Arr *>
      withSourcePos (AST.MemberExpr expr' <$> withSourcePos identifier)
    , return expr'
    ]

prefixExpr :: SourceParser AST.Expr
prefixExpr =
  withSourcePos $
  symbol T.Percent *> liftA2 AST.PrefixExpr identifier (optionalL actuals)

class OpImpl a where
  opImplL :: a -> SourceParser AST.Expr -> SourceParser AST.Expr
  opImplL x next = next <**> opRestImplL x next
  opImplN :: a -> SourceParser AST.Expr -> SourceParser AST.Expr
  opImplN x next = next <**> opRestImplN x next
  opRestImplL ::
       a
    -> SourceParser AST.Expr
    -> Parser (Annot AST.Expr SourcePos -> Annot AST.Expr SourcePos)
  opRestImplL x next =
    withAnnot <$> getPos <*< opRestInner x next >*> opRestImplL x next <|>
    pure id
  opRestImplN ::
       a
    -> SourceParser AST.Expr
    -> Parser (Annot AST.Expr SourcePos -> Annot AST.Expr SourcePos)
  opRestImplN x next = withAnnot <$> getPos <*< opRestInner x next <|> pure id
  opRestInner ::
       a -> SourceParser AST.Expr -> Parser (Annot AST.Expr SourcePos -> AST.Expr SourcePos)

instance OpImpl (AST.Op, T.Token SourcePos) where
  opRestInner (op, str) next = flip (AST.BinOpExpr op) <$> (symbol str *> next)

instance OpImpl x => OpImpl [x] where
  opRestInner xs next = foldl1 (<|>) (flip opRestInner next <$> xs)

instance OpImpl Text where
  opRestInner "`" next =
    symbol T.Backtick *> (flip . AST.InfixExpr <$> identifier) <* symbol T.Backtick <*>
    next
  opRestInner _ _ = error "Parser not implemented for this operator"

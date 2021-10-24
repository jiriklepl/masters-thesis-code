{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module CMM.Parser where

import safe Control.Applicative hiding (many)
import safe Data.Foldable
import safe Data.Functor
import safe Data.Maybe
import safe qualified Data.Set as Set
import safe Data.Text (Text)
import safe Data.Void
import safe Prelude
import safe Text.Megaparsec hiding (State)

import safe CMM.AST
import safe CMM.AST.Utils

import safe qualified CMM.Lexer as L

type Parser = Parsec Void [Annot L.Token SourcePos]

type SourceParser a = Parser (Annot a SourcePos)

type ULocParser a = Parser (a SourcePos)

optionalL :: Parser [a] -> Parser [a]
optionalL = (fromMaybe [] <$>) . optional

liftA4 ::
     Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

liftA5 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
liftA5 f a b c d e = liftA4 f a b c d <*> e

liftA6 ::
     Applicative f
  => (a -> b -> c -> d -> e -> g -> h)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
  -> f g
  -> f h
liftA6 f a b c d e g = liftA5 f a b c d e <*> g

infixl 4 <*<

(<*<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) = liftA2 (.)

infixl 4 >*>

(>*>) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>*>) = liftA2 (flip (.))

keyword :: L.Reserved -> Parser L.Reserved
keyword name = flip token Set.empty $ \case
  Annot (L.Keyword name') _  -> if name == name' then Just name else Nothing
  _ -> Nothing

keywords :: [L.Reserved] -> Parser L.Reserved
keywords = foldl1 (*>) . (keyword <$>)

symbol :: L.Token SourcePos -> Parser ()
symbol tok = flip token Set.empty $ \case
  Annot tok' _ -> if tok == tok' then Just () else Nothing

identifier :: Parser (Name SourcePos)
identifier  = flip token Set.empty $ \case
  Annot (L.Ident n) _  -> Just $ Name n
  _ -> Nothing

stringLiteral :: Parser StrLit
stringLiteral  = flip token Set.empty $ \case
  Annot (L.StrLit t) _  -> Just $ StrLit t
  _ -> Nothing

bitsType :: Parser Int
bitsType  = flip token Set.empty $ \case
  Annot (L.BitsType i) _  -> Just i
  _ -> Nothing

charLit :: Parser Char
charLit  = flip token Set.empty $ \case
  Annot (L.CharLit c) _  -> Just c
  _ -> Nothing

floatLit :: Parser Float
floatLit  = flip token Set.empty $ \case
  Annot (L.FloatLit f) _  -> Just f
  _ -> Nothing

intLit :: Parser (Int, Bool)
intLit  = flip token Set.empty $ \case
  Annot (L.IntLit i) _  -> Just i
  _ -> Nothing

getPos :: Parser SourcePos
getPos = lookAhead $ flip token Set.empty $ \case
  Annot _ pos -> Just pos

withSourcePos :: ULocParser a -> SourceParser a
withSourcePos = liftA2 withAnnot getPos

identifiers :: Parser [Annot Name SourcePos]
identifiers = commaList $ withSourcePos identifier

braces :: Parser a -> Parser a
braces = between (symbol L.LBrace) (symbol L.RBrace)

parens :: Parser a -> Parser a
parens = between (symbol L.LParen) (symbol L.RParen)

brackets :: Parser a -> Parser a
brackets = between (symbol L.LBracket) (symbol L.RBracket)

angles :: Parser a -> Parser a
angles =  between (symbol L.Lt) (symbol L.Gt)

commaList :: Parser a -> Parser [a]
commaList = (`sepEndBy1` comma)

program :: SourceParser Unit
program = unit

comma :: Parser ()
comma = symbol L.Comma

colon :: Parser ()
colon = symbol L.Colon

semicolon :: Parser ()
semicolon = symbol L.Semicolon

-- | Parses the whole 'Unit'
unit :: SourceParser Unit
unit = withSourcePos $ Unit <$> many topLevel

topLevel :: SourceParser TopLevel
topLevel =
  withSourcePos $
  choice [sectionTopLevel, TopDecl <$> decl, TopProcedure <$> try procedure]

sectionTopLevel :: ULocParser TopLevel
sectionTopLevel =
  keyword L.Section *> liftA2 TopSection stringLiteral (braces $ many section)

section :: SourceParser Section
section =
  withSourcePos $
  choice
    [ SecDecl <$> decl
    , secSpan
    , SecProcedure <$> try procedure
    , SecDatum <$> datum
    ]

decl :: SourceParser Decl
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

importDecl :: ULocParser Decl
importDecl = keyword L.Import *> (ImportDecl <$> commaList import') <* semicolon

exportDecl :: ULocParser Decl
exportDecl = keyword L.Export *> (ExportDecl <$> commaList export) <* semicolon

constDecl :: ULocParser Decl
constDecl =
  keyword L.Const *>
  liftA2
    (uncurry ConstDecl)
    (try (liftA2 (,) (optional typeToken) identifier) <|>
     (Nothing, ) <$> identifier)
    (symbol L.EqSign *> expr) <*
  semicolon

typedefDecl :: ULocParser Decl
typedefDecl =
  keyword L.Typedef *> liftA2 TypedefDecl typeToken identifiers <* semicolon

pragmaDecl :: ULocParser Decl
pragmaDecl = keyword L.Pragma *> liftA2 PragmaDecl identifier (braces pragma)

targetDecl :: ULocParser Decl
targetDecl =
  keyword L.Target *> (TargetDecl <$> many targetDirective) <* semicolon

targetDirective :: SourceParser TargetDirective
targetDirective =
  withSourcePos $
  choice
    [ memSizeDirective
    , byteOrderDirective
    , pointerSizeDirective
    , wordSizeDirective
    ]

registerDecl :: ULocParser Decl
registerDecl = liftA2 RegDecl invariant registers <* semicolon

memSizeDirective :: ULocParser TargetDirective
memSizeDirective = keyword L.Memsize *> (MemSize . fst <$> intLit)

byteOrderDirective :: ULocParser TargetDirective
byteOrderDirective = keyword L.Byteorder *> (ByteOrder <$> endian)

endian :: Parser Endian
endian = choice [keyword L.Little $> Little, keyword L.Big $> Big]

pointerSizeDirective :: ULocParser TargetDirective
pointerSizeDirective =
  keyword L.Pointersize *> (PointerSize . fst <$> intLit)

wordSizeDirective :: ULocParser TargetDirective
wordSizeDirective = keyword L.Wordsize *> (WordSize . fst <$> intLit)

procedure :: SourceParser Procedure
procedure =
  withSourcePos $ liftA4 Procedure (optional convention) identifier formals body

formal :: SourceParser Formal
formal = withSourcePos $ liftA4 Formal mKind invariant typeToken identifier

formals :: Parser [Annot Formal SourcePos]
formals = parens $ formal `sepEndBy` comma

invariant :: Parser Bool
invariant = keyword L.Invariant $> True <|> pure False

actual :: SourceParser Actual
actual = withSourcePos $ liftA2 Actual mKind expr

actuals :: Parser [Annot Actual SourcePos]
actuals = parens $ actual `sepEndBy` comma

convention :: Parser Conv
convention = keyword L.Foreign *> (Foreign <$> stringLiteral)

import' :: SourceParser Import
import' =
  withSourcePos $
  liftA2 Import (optional (stringLiteral <* keyword L.As)) identifier

export :: SourceParser Export
export =
  withSourcePos $
  liftA2 Export identifier (optional (keyword L.As *> stringLiteral))

body :: SourceParser Body
body = withSourcePos . braces $ Body <$> many bodyItem

bodyItem :: SourceParser BodyItem
bodyItem =
  withSourcePos $
  BodyDecl <$> try decl <|> BodyStackDecl <$> stackDecl <|> BodyStmt <$> stmt

secSpan :: ULocParser Section
secSpan = keyword L.Span *> liftA3 SecSpan expr expr (braces $ many section)

datum :: SourceParser Datum
datum = withSourcePos $ choice [alignDatum, try labelDatum, justDatum]

alignDatum :: ULocParser Datum
alignDatum = keyword L.Align *> (DatumAlign . fst <$> intLit) <* semicolon

labelDatum :: ULocParser Datum
labelDatum = DatumLabel <$> identifier <* colon

justDatum :: ULocParser Datum
justDatum = liftA3 Datum typeToken (optional size) (optional init') <* semicolon

init' :: SourceParser Init
init' = withSourcePos $ choice [stringInit, string16Init, initList]

initList :: ULocParser Init
initList = braces $ ExprInit <$> commaList expr

stringInit :: ULocParser Init
stringInit = StrInit <$> stringLiteral

string16Init :: ULocParser Init
string16Init = keyword L.Unicode *> (Str16Init <$> parens stringLiteral)

size :: SourceParser Size
size = withSourcePos . brackets $ Size <$> optional expr

registers :: SourceParser Registers
registers =
  withSourcePos $
  liftA3
    Registers
    mKind
    typeToken
    (commaList
       (liftA2
          (,)
          (withSourcePos identifier)
          (optional $ symbol L.EqSign *> stringLiteral)))

typeToken :: SourceParser Type
typeToken = withSourcePos $ bitsType' <|> nameType

bitsType' :: ULocParser Type
bitsType' = TBits <$> bitsType

nameType :: ULocParser Type
nameType = TName <$> identifier

mKind :: Parser (Maybe Kind)
mKind = optional $ Kind <$> stringLiteral

pragma :: Parser a
pragma = undefined -- FIXME: pragmas not yet specified and with no explanation of functionality

stackDecl :: SourceParser StackDecl
stackDecl =
  withSourcePos $ keyword L.Stackdata *> braces (StackDecl <$> many datum)

stmt :: SourceParser Stmt
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

emptyStmt :: ULocParser Stmt
emptyStmt = semicolon $> EmptyStmt

ifStmt :: ULocParser Stmt
ifStmt =
  keyword L.If *> liftA3 IfStmt expr body (optional $ keyword L.Else *> body)

switchStmt :: ULocParser Stmt
switchStmt = keyword L.Switch *> liftA2 SwitchStmt expr (braces (many arm))

spanStmt :: ULocParser Stmt
spanStmt = keyword L.Span *> liftA3 SpanStmt expr expr body

assignStmt :: ULocParser Stmt
assignStmt =
  liftA2 AssignStmt (commaList lvalue <* symbol L.EqSign) (commaList expr) <* semicolon

primOpStmt :: ULocParser Stmt
primOpStmt =
  liftA4
    PrimOpStmt
    (identifier <* symbol L.EqSign)
    (symbol L.DPercent *> identifier)
    (optionalL actuals)
    (many flow) <*
  semicolon

callStmt :: ULocParser Stmt
callStmt =
  liftA6
    CallStmt
    (optionalL kindedNames <* symbol L.EqSign)
    (optional convention)
    expr
    actuals
    (optional targets)
    (many . withSourcePos $ FlowAnnot <$> flow <|> AliasAnnot <$> alias) <*
  semicolon

jumpStmt :: ULocParser Stmt
jumpStmt =
  liftA4
    JumpStmt
    (optional convention <* keyword L.Jump)
    expr
    (optionalL actuals)
    (optional targets) <*
  semicolon

returnStmt :: ULocParser Stmt
returnStmt =
  liftA3
    ReturnStmt
    (optional convention <* keyword L.Return)
    (optional . angles $
     liftA2 (,) (restrictedExpr <* symbol L.Slash) restrictedExpr)
    (optionalL actuals) <*
  semicolon

lvalue :: SourceParser LValue
lvalue = withSourcePos $ try lvRef <|> lvName

lvRef :: ULocParser LValue
lvRef =
  liftA3 LVRef typeToken (symbol L.LBracket *> expr) (optional asserts) <* symbol L.RBracket

lvName :: ULocParser LValue
lvName = LVName <$> identifier

asserts :: Parser (Annot Asserts SourcePos)
asserts = withSourcePos $ alignAssert <|> inAssert

alignAssert :: ULocParser Asserts
alignAssert =
  liftA2
    AlignAssert
    (keyword L.Aligned *> (fst <$> intLit))
    (optionalL $ keyword L.In *> (withSourcePos identifier `sepBy1` comma))

inAssert :: ULocParser Asserts
inAssert =
  liftA2
    InAssert
    (keyword L.In *> (withSourcePos identifier `sepBy1` comma))
    (optional (keyword L.Aligned *> (fst <$> intLit)))

labelStmt :: ULocParser Stmt
labelStmt = LabelStmt <$> identifier <* colon

contStmt :: ULocParser Stmt
contStmt =
  keyword L.Continuation *>
  liftA2 ContStmt identifier (parens $ optionalL kindedNames) <*
  colon

gotoStmt :: ULocParser Stmt
gotoStmt =
  keyword L.Goto *> liftA2 GotoStmt expr (optional targets) <* semicolon

cutToStmt :: ULocParser Stmt
cutToStmt =
  keywords [L.Cut, L.To] *> liftA3 CutToStmt expr actuals (many flow) <*
  semicolon

kindedNames :: Parser [Annot KindName SourcePos]
kindedNames = commaList . withSourcePos $ liftA2 KindName mKind identifier

arm :: SourceParser Arm
arm =
  withSourcePos $ keyword L.Case *> liftA2 Arm (commaList range <* colon) body

range :: SourceParser Range
range = withSourcePos $ liftA2 Range expr (optional $ symbol L.DotDot *> expr)

flow :: SourceParser Flow
flow = withSourcePos $ alsoFlow <|> neverReturns

alsoFlow :: ULocParser Flow
alsoFlow =
  keyword L.Also *>
  choice [alsoCutsTo, alsoUnwindsTo, alsoReturnsTo, alsoAborts]

alsoCutsTo :: ULocParser Flow
alsoCutsTo = keywords [L.Cuts, L.To] *> (AlsoCutsTo <$> identifiers)

alsoUnwindsTo :: ULocParser Flow
alsoUnwindsTo = keywords [L.Unwinds, L.To] *> (AlsoUnwindsTo <$> identifiers)

alsoReturnsTo :: ULocParser Flow
alsoReturnsTo = keywords [L.Returns, L.To] *> (AlsoReturnsTo <$> identifiers)

alsoAborts :: ULocParser Flow
alsoAborts = keyword L.Aborts *> optional comma $> AlsoAborts

neverReturns :: ULocParser Flow
neverReturns = keywords [L.Never, L.Returns] *> optional comma $> NeverReturns

alias :: SourceParser Alias
alias = withSourcePos $ readsAlias <|> writesAlias

readsAlias :: ULocParser Alias
readsAlias = keyword L.Reads *> (Reads <$> identifiers)

writesAlias :: ULocParser Alias
writesAlias = keyword L.Writes *> (Writes <$> identifiers)

targets :: SourceParser Targets
targets = withSourcePos $ keyword L.Targets *> (Targets <$> identifiers)

expr :: SourceParser Expr
expr = infixExpr

simpleExpr :: SourceParser Expr
simpleExpr = choice [litExpr, parExpr, prefixExpr, try refExpr, nameExpr]

restrictedExpr :: SourceParser Expr
restrictedExpr = choice [litExpr, parExpr, nameExpr]

litExpr :: SourceParser Expr
litExpr =
  withSourcePos $
  liftA2
    LitExpr
    (withSourcePos $ choice [charExpr, floatExpr, intExpr])
    (optional $ symbol L.DColon *> typeToken)

intExpr :: ULocParser Lit
intExpr = LitInt . fst <$> intLit

floatExpr :: ULocParser Lit
floatExpr = LitFloat <$> floatLit

charExpr :: ULocParser Lit
charExpr = LitChar <$> charLit

nameExpr :: SourceParser Expr
nameExpr = withSourcePos $ LVExpr <$> withSourcePos lvName

refExpr :: SourceParser Expr
refExpr = withSourcePos $ LVExpr <$> withSourcePos lvRef

parExpr :: SourceParser Expr
parExpr = withSourcePos $ ParExpr <$> parens expr

-- SYMBOLIC OPERATORS --
-- Source: https://www.cs.tufts.edu/~nr/c--/extern/man2.pdf (7.4.1)
infixExpr :: SourceParser Expr
infixExpr = opImplN ("`" :: Text) cmpExpr

cmpExpr :: SourceParser Expr
cmpExpr =
  opImplN
    [ (GeOp, L.Geq :: L.Token SourcePos)
    , (GtOp, L.Gt)
    , (LeOp, L.Leq)
    , (LtOp, L.Lt)
    , (NeqOp, L.Neq)
    , (EqOp, L.Eq)
    ]
    orExpr

orExpr :: SourceParser Expr
orExpr = opImplL (OrOp, L.Pipe :: L.Token SourcePos) xorExpr

xorExpr :: SourceParser Expr
xorExpr = opImplL (XorOp, L.Caret :: L.Token SourcePos) andExpr

andExpr :: SourceParser Expr
andExpr = opImplL (XorOp, L.Ampersand :: L.Token SourcePos) shExpr

shExpr :: SourceParser Expr
shExpr = opImplL [(ShLOp, L.ShL :: L.Token SourcePos), (ShROp, L.ShR)] addExpr

addExpr :: SourceParser Expr
addExpr = opImplL [(AddOp, L.Plus :: L.Token SourcePos), (SubOp, L.Minus)] mulExpr

mulExpr :: SourceParser Expr
mulExpr = opImplL [(DivOp, L.Slash :: L.Token SourcePos), (MulOp, L.Star), (ModOp, L.Percent)] negExpr

-- SYMBOLIC OPERATORS -- END

negExpr :: SourceParser Expr
negExpr =
  withSourcePos (symbol L.Minus *> (NegExpr <$> negExpr)) <|>
  withSourcePos (symbol L.Tilde *> (ComExpr <$> negExpr)) <|>
  simpleExpr

prefixExpr :: SourceParser Expr
prefixExpr =
  withSourcePos $ symbol L.Percent *> liftA2 PrefixExpr identifier (optionalL actuals)

class OpImpl a where
  opImplL :: a -> SourceParser Expr -> SourceParser Expr
  opImplL x next = next <**> opRestImplL x next
  opImplN :: a -> SourceParser Expr -> SourceParser Expr
  opImplN x next = next <**> opRestImplN x next
  opRestImplL ::
       a
    -> SourceParser Expr
    -> Parser (Annot Expr SourcePos -> Annot Expr SourcePos)
  opRestImplL x next =
    withAnnot <$> getPos <*< opRestInner x next >*> opRestImplL x next <|>
    pure id
  opRestImplN ::
       a
    -> SourceParser Expr
    -> Parser (Annot Expr SourcePos -> Annot Expr SourcePos)
  opRestImplN x next =
    withAnnot <$> getPos <*< opRestInner x next <|> pure id
  opRestInner ::
       a -> SourceParser Expr -> Parser (Annot Expr SourcePos -> Expr SourcePos)

instance OpImpl (Op, L.Token SourcePos) where
  opRestInner (op, str) next = flip (BinOpExpr op) <$> (symbol str *> next)

instance OpImpl x => OpImpl [x] where
  opRestInner xs next = foldl1 (<|>) (flip opRestInner next <$> xs)

instance OpImpl Text where
  opRestInner "`" next =
    symbol L.Backtick *> (flip . InfixExpr <$> identifier) <* symbol L.Backtick <*> next
  opRestInner _ _ = error "Parser not implemented for this operator"

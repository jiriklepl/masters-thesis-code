{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CMM.Pretty where

import safe Data.String (IsString)

import safe Prettyprinter
  ( Doc
  , Pretty(pretty)
  , braces
  , comma
  , enclose
  , hsep
  , indent
  , line
  , punctuate
  , space
  , vsep
  )

commaSep :: [Doc ann] -> Doc ann
commaSep xs = hsep $ punctuate comma xs

commaPretty :: Pretty a => [a] -> Doc ann
commaPretty = commaSep . fmap pretty

inBraces :: Doc ann -> Doc ann
inBraces = braces . enclose line line . indent 2

bracesBlock :: Pretty a => [a] -> Doc ann
bracesBlock xs = inBraces . vsep $ pretty <$> xs

maybeSpacedL :: Pretty a => Maybe a -> Doc ann
maybeSpacedL = maybe mempty ((space <>) . pretty)

maybeSpacedR :: Pretty a => Maybe a -> Doc ann
maybeSpacedR = maybe mempty ((<> space) . pretty)

darrow :: IsString a => a
darrow = "=>"

arrow :: IsString a => a
arrow = "->"

bquote :: IsString a => a
bquote = "`"

bquotes :: Doc ann -> Doc ann
bquotes = enclose bquote bquote

ddot :: IsString a => a
ddot = ".."

dcolon :: IsString a => a
dcolon = "::"

lambda :: IsString a => a
lambda = "λ"

deltaBig :: IsString a => a
deltaBig = "Δ"

question :: IsString a => a
question = "?"

bang :: IsString a => a
bang = "!"

emptySet :: IsString a => a
emptySet = "∅"

dollar :: IsString a => a
dollar = "$"

star :: IsString a => a
star = "*"

arrowNice :: IsString a => a
arrowNice = "->"

darrowNice :: IsString a => a
darrowNice = "=>"

isIn :: IsString a => a
isIn = "∈"

instSymbol :: IsString a => a
instSymbol = "⊑"

genSymbol :: IsString a => a
genSymbol = "⊒"

typingSymbol :: IsString a => a
typingSymbol = "T"

constingSymbol :: IsString a => a
constingSymbol = "C"

kindingSymbol :: IsString a => a
kindingSymbol = "K"

regingSymbol :: IsString a => a
regingSymbol = "R"

ifTrue :: Monoid a => Bool -> a -> a
ifTrue bool x =
  if bool
    then x
    else mempty

prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

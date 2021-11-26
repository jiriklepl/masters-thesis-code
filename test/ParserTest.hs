{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Main
  ( main
  ) where

import safe Data.Text (Text)
import safe qualified Data.Text as T
import Debug.Trace (trace)
import safe Prettyprinter (Doc(), Pretty(), pretty)
import safe System.Exit (exitFailure, exitSuccess)
import safe Test.HUnit
import safe Text.Megaparsec (ParseErrorBundle(), Parsec(), many, runParser)

import safe CMM.AST
import safe CMM.AST.Annot
import safe CMM.Lexer
import safe CMM.Parser
import safe CMM.Pretty ()
import safe QuasiQuotes (text)

data SimpleAnnotation node annot =
  SimpleAnnot node annot
  deriving (Show, Functor)

deriving instance Eq n => Eq (SimpleAnnotation n ())

type SimpleAnnot = SimpleAnnotation

withSimpleAnnot :: b -> a -> SimpleAnnot a b
withSimpleAnnot = flip SimpleAnnot

withDummyAnnot :: a -> SimpleAnnot a ()
withDummyAnnot = withSimpleAnnot ()

class PrettyEmptyUnit n where
  prettyEmptyUnit :: n -> Doc ann

instance {-# OVERLAPPABLE #-} Pretty n => PrettyEmptyUnit n where
  prettyEmptyUnit = pretty

instance {-# OVERLAPPING #-} PrettyEmptyUnit () where
  prettyEmptyUnit () = mempty

instance (PrettyEmptyUnit n) => Pretty (SimpleAnnot n a) where
  pretty (SimpleAnnot n _) = prettyEmptyUnit n

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

checkReparse ::
     (Eq (n ()), Functor n, Pretty (n a)) => Parser (n b) -> n a -> Bool
checkReparse parser ast =
  either (const False) ((== stripAnnots ast) . stripAnnots) .
  either undefined (parse parser) . parse tokenize . T.pack . show $ -- TODO: clean this up (along with the other undefined)
  pretty ast

testTemplate ::
     (Show a, Pretty a) => String -> Text -> Parser a -> (a -> Bool) -> Test
testTemplate testName input parser validator =
  testName ~: assertion $
  either ((`trace` undefined) . show) (parse parser) $ parse tokenize input
  where
    assertion (Left result) =
      assertFailure $
      "Failed to parse the input: " ++
      T.unpack input ++ "------\n" ++ show result
    assertion (Right result) =
      assertBool
        ("Failed to validate the output: " ++ show (pretty result))
        (validator result)

manTests :: [Test]
manTests =
  [ testTemplate
      "manual, Figure 1"
      [text|
/* Ordinary recursion */
export sp1;
sp1( bits32 n ) {
bits32 s, p;
if n == 1 {
return( 1, 1 );
} else {
s, p = sp1( n-1 );
return( s+n, p*n );
}
}
/* Tail recursion */
export sp2;
sp2( bits32 n ) {
jump sp2_help( n, 1, 1 );
}
sp2_help( bits32 n, bits32 s, bits32 p ) {
if n==1 {
return( s, p );
} else {
jump sp2_help( n-1, s+n, p*n );
}
}
/* Loops */
export sp3;
sp3( bits32 n ) {
bits32 s, p;
s = 1; p = 1;
loop:
if n==1 {
return( s, p );
} else {
s = s+n;
p = p*n;
n = n-1;
goto loop;
} }
    |]
      (many topLevel)
      (all $ checkReparse topLevel)
  , testTemplate
      "manual, 3.3.2"
      [text|
x _foo.name_abit_12.long
foo Sys.Indicators
_912 .9Aname
aname12 $1
@name
    |]
      (many identifier)
      (all $ checkReparse identifier)
  , testTemplate
      "manual, 3.3.3, integer literals"
      [text|
5 01234 23::bits8 077::bits16 0x00 255U::bits8 -128::bits8
    |]
      (many litExpr)
      (all $ checkReparse litExpr)
  , testTemplate
      "manual, 3.3.3, bit vectors"
      [text|
0x81::bits8 0201::bits8 129U::bits8 -127::bits8
    |]
      (many litExpr)
      (all $ checkReparse litExpr)
  , testTemplate
      "manual, 3.3.4, float literals"
      [text|
3.1415 3e-5 1e+2 23.3e-4 2.71828e0::bits64
    |]
      (many litExpr)
      (all $ checkReparse litExpr)
  , testTemplate
      "manual, 3.3.5, char literals"
      [text|
'a' 'a'::bits16 '\0' '\x0' '\010' '\r'
    |]
      (many litExpr)
      (all $ checkReparse litExpr)
  , testTemplate
      "manual, 3.3.6, string literals"
      [text|
"hello" "world\0" "(%d) (%s) \n"
    |]
      (many (withDummyAnnot <$> stringLiteral))
      (all . checkReparse $ withDummyAnnot <$> stringLiteral)
  , testTemplate
      "manual, 3.4, comments"
      [text|
/* This is a multi-line
comment */
// */ this is a one-line comment
/*// this comment
ends at the end of this line, not at the end of the line above */
    |]
      (withDummyAnnot <$> mempty)
      (checkReparse . pure $ withDummyAnnot ())
  , testTemplate
      "manual, TopLevel example"
      [text|
import printf, sqrt; /* C procedures used in this C-- program */
export f3; /* To be used outside this C-- program */
import "jump" as ext_jump;
const pi = 3.1415; /* type bits k - native word size */
const mega = kilo * kilo; /* type bits k - native word size */
const kilo = 1024; /* type bits k - native word size */
const nl = '\n'; /* type bits8 */
typedef bits32 code;
typedef bits32 word;
typedef bits32 data;
bits64 hp;
"address" bits64 hptr, hplim;
"float" bits80 epsilon;
bits32 rm = "IEEE 754 rounding mode";
section "data" {
foo: bits32[] {1::bits32,2::bits32,3::bits32,ff}; // ff is a forward reference
bits32[] {1::bits32,2::bits32};
ff: bits32[] {2,3}; bits32[]{ff,foo};
str: bits8[] "Hello world\0";
bits16[10];
}
section "data" {
baz: align 8;
quux: bits32 {0};
}
section "text" {
hello: bits8[] "hello world\n\0";
/* hello is of the native pointer type */
}
section "data" {
align 4;
one_: bits32 {1};
align 8;
pi_: bits32 {3.1415};
}
section "data" {
align 1;
align 16;
align 8;
label: bits32[] {1,2,3};
}
section "data" {
const PROC = 3;
bits32[] {p_end, PROC};
p (bits32 i) {
loop:
i = i-1;
if (i >= 0) { goto loop ; }
return;
}
p_end:
}
target
memsize 8
byteorder little
pointersize 32
wordsize 32 ;
goo(bits32 y) {
bits32 x;
x = y + 1;
jump bar(x);
}
f2 (bits32 x) {
bits32 y;
stackdata { p : bits32;
q : bits32[40];
}
/* Here, p and q are the addresses of the relevant chunks of data.
* Their type is the native pointer type of the machine.
*/
return (q - p);
}
export ceefun;
foreign "C" ceefun() {
bits32 x;
x = bar(x);
foreign "C" return (x);
}
bar(bits32 a) {
return (a + 1);
}
swap (bits32 x) {
bits32 p,q;
p,q = x, x+1::bits32;
bits32[p], bits32[q] = bits32[q], bits32[p];
return;
}
f0(bits32 x) {
bits32 y;
y = 0;
if (y >= bits32[foo+8]) {
y = y + 1;
return (y);
} else {
x = x - 1;
if x != 0 {
y = y + 2;
}
return (y);
}
}
f6 (bits32 x, bits32 y) {
import f;
switch (x + 23) {
case 1,2,3 : { y = y + 1;}
case 5 : { y = x + 1; x = y;}
case 0,4,6,7 : { y = f();
if y == 0 { x = 1;}
}
}
return (x, y);
}
f3 () {
stackdata {
bar: bits8[64];
}
f3_label:
bits64[bar] = 18::bits64;
bits64[bar+4*8] = bits64[bar];
if (%zx32(bits8[bar+4*8]) == 18) { return; }
goto f3_label;
}
f7(bits32 x, bits32 y) {
jump f7(y, x); /* Loop forever */
}
f4() {
bits32 x, y;
x, y = f5(5);
return (x,y);
}
f5(bits32 x) {
return (x, x+1);
}
fn (bits32 x /* 0 */, bits32 y /* 1 */) {
bits32 m /* 2 */, n /* 3 */;
return (y,x);
}
    |]
      (many topLevel)
      (all $ checkReparse topLevel)
  ]

main :: IO ()
main = do
  results <-
    runTestTT $
    test
      (manTests ++
       [ testTemplate
           "test1"
           "x + y * z"
           expr
           (\case
              Annot (BinOpExpr AddOp _ (Annot (BinOpExpr MulOp _ _) _)) _ ->
                True
              _ -> False)
       , "tests2" ~: assertEqual "Failed" (3 :: Integer) (3 :: Integer)
       ])
  if errors results + failures results == 0
    then exitSuccess
    else exitFailure

{-# LANGUAGE Safe #-}

module CMM.Pipeline where

import Text.Megaparsec hiding (Token)
import CMM.Lexer
import Control.Monad
import Data.Either
import Data.Function
import Data.Bifunctor
import CMM.AST.Annot
import Data.Text
import Data.String
import CMM.Parser
import CMM.AST

tokenizer :: String
  -> Text
  -> Either String [Annot Token SourcePos]
tokenizer source = first errorBundlePretty . runParser tokenize source

parser :: String
  -> [Annot Token SourcePos]
  -> Either String (Annot Unit SourcePos)
parser source = first errorBundlePretty . runParser unit source

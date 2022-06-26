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
import CMM.Inference.Preprocess.Settings
import CMM.Inference.Preprocess.State
import CMM.Inference.Settings
import CMM.Inference.State
import CMM.Monomorphize.Settings
import CMM.Monomorphize.State
import CMM.Inference.Preprocess.TypeHole
import CMM.Lexer.Token

tokenizer :: String
  -> Text
  -> Either String [Annot Token SourcePos]
tokenizer source = first errorBundlePretty . runParser tokenize source

parser :: String
  -> [Annot Token SourcePos]
  -> Either String (Annot Unit SourcePos)
parser source = first errorBundlePretty . runParser unit source

-- preprocessor :: PreprocessorSettings -> Annot Unit SourcePos
--   -> Either String (Annot Unit (SourcePos, TypeHole), PreprocessorState)
-- preprocessor = _

-- inferencer :: InferencerSettings -> Annot Unit SourcePos
--   -> Either String (Annot Unit (SourcePos, TypeHole), InferencerState)
-- inferencer = _

-- monomorphizer :: MonomorphizerSettings -> Annot Unit SourcePos
--   -> Either String (Annot Unit (SourcePos, MonomorphizeState SourcePos), (InferencerState, MonomorphizeState a))
-- monomorphizer = _

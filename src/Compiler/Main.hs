{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State as State
import qualified Data.Map as Map

import Debug.Trace -- TODO: remove this

-- import Data.Text as T
import Data.Text.IO as TS
import Data.Tuple
import Text.Megaparsec hiding (parse)

import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Prettyprinter

import CMM.AST
import CMM.AST.Blockifier
import qualified CMM.AST.Blockifier.State as B
import CMM.AST.Flattener
import CMM.FlowAnalysis
import CMM.Inference.Preprocess
import CMM.Inference.Preprocess.State
import CMM.Inference.Type
import CMM.Lexer
import CMM.Parser
import CMM.Translator
import qualified CMM.Translator.State as Tr

main :: IO ()
main = do
  contents <- TS.getContents
  let ast =
        either undefined id .
        parse procedure . either undefined id . parse tokenize $
        contents
  let flattened = flatten ast
  let (mined, miner) =
        runState
          (preprocess ast :: (MonadState InferPreprocessor m) =>
                               m (Annot Procedure (SourcePos, TypeHandle)))
          initInferPreprocessor
  (blockified, blockifier) <-
    runStateT
      (blockifyProcedure flattened <* analyzeFlow flattened)
      B.initBlockifier
  trace (show $ pretty blockified) $ return ()
  let translated =
        ppllvm $
        flip
          evalState
          Tr.initTranslState
            { Tr._controlFlow = B._controlFlow blockifier
            , Tr._blockData = B._blockData blockifier
            , Tr._blocksTable =
                Map.fromList . (swap <$>) . Map.toList $
                B._blocksTable blockifier
            } $
        buildModuleT "llvm" $
        runIRBuilderT emptyIRBuilder $ translate blockified
  T.putStr translated
  print mined
  print (_facts miner)
  return ()

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

{-# LANGUAGE OverloadedStrings #-}

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

import CMM.AST.Blockifier
import qualified CMM.AST.Blockifier.State as B
import CMM.AST.Flattener
import CMM.FlowAnalysis
import CMM.Lexer
import CMM.Parser
import CMM.Translator
import qualified CMM.Translator.State as Tr

main :: IO ()
main = do
  contents <- TS.getContents
  let p =
        flatten .
        either undefined id .
        parse procedure . either undefined id . parse tokenize $
        contents
  (blockified, blockifier) <-
    runStateT (blockifyProcedure p <* analyzeFlow p) B.initBlockifier
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
  return ()

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

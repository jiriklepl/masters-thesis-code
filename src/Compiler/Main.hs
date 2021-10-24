{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State as State
import qualified Data.Map as Map
-- import Data.Text as T
import Data.Text.IO as TS
import Data.Tuple
import Text.Megaparsec hiding (parse)

import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import CMM.AST.Flattener
import CMM.Lexer
import CMM.Translator
import CMM.Parser
import CMM.AST.LRAnalysis
import CMM.Pretty ()
import qualified CMM.TranslState as Tr
import qualified CMM.AST.Blockifier as B

main :: IO ()
main = do
    contents <- TS.getContents
    (blockified, blockifier) <- blockifyProcedure . flatten . either undefined id . parse procedure . either undefined id . parse tokenize $ contents
    let translated =  ppllvm $ flip evalState Tr.initTranslState
            { Tr._controlFlow = B._controlFlow blockifier
            , Tr._blockData = B._blockData blockifier
            , Tr._blocksTable = Map.fromList . (swap <$>) . Map.toList $ B._blocksTable blockifier
            } $ buildModuleT "llvm" $ runIRBuilderT emptyIRBuilder $ translate blockified
    T.putStr translated
    return ()

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

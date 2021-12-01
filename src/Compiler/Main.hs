{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State as State
import qualified Data.Map as Map

-- import Data.Text as T
import Data.Text.IO as TS
import Data.Tuple
import Text.Megaparsec hiding (parse)

import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Prettyprinter

import CMM.AST
import CMM.AST.Annot
import CMM.AST.Blockifier
import qualified CMM.AST.Blockifier.State as B
import CMM.AST.Flattener
import CMM.AST.Variables
import CMM.FlowAnalysis
import CMM.Inference.Preprocess as Infer
import CMM.Inference.Preprocess.State as Infer
import qualified CMM.Inference.Preprocess.State
import CMM.Inference.Type as Infer
import CMM.Inference.State as Infer
import CMM.Inference as Infer
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
  (mined, miner) <- runStateT (preprocess ast) initInferPreprocessor
  (blockified, blockifier) <-
    runStateT
      (blockifyProcedure flattened <* analyzeFlow flattened)
      B.initBlockifier
  T.putStr . T.pack . show $ pretty blockified
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
  let annot = takeAnnot ast
  vars <-
    globalVariables
      (Unit
         [ withAnnot annot $
           TopSection
             (CMM.AST.StrLit "data")
             [withAnnot annot $ SecProcedure ast]
         ])
  print vars
  print
    (freeTypeVars
       (SimpleType
          (TupleType
             [ SimpleType BoolType
             , SimpleType (VarType (TypeVar 20))
             , SimpleType (VarType (TypeVar 30))
             ])))
  print (_facts miner)
  runStateT (Infer.infer (_facts miner) >>= Infer.infer) (Infer.initInferencer (CMM.Inference.Preprocess.State._handleCounter miner)) >>= print

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

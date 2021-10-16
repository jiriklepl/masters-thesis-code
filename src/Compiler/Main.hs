{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State as State
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Text as T
import Data.Text.IO as TS
import Data.Tuple
import Data.Void
import Data.Functor
import Text.Megaparsec hiding (parse)

import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Type as AST

import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Language.AST
import Language.AST.Flattener
import Language.Lexer
import Language.Translator
import Language.Parser
import Language.AST.LRAnalysis
import Language.Pretty ()
import Prettyprinter (pretty)

-- main :: IO ()
-- main = T.putStr . ppllvm . buildModule "exampleModule" $ mdo
--   foo <- function "add" [(i32, "foo"), (i32, "bar")] i32 $ \[a, b] -> mdo
--     entry <- block `named` "entry"
--     c <- add a b
--     br entry2
--     result <- call foo [(a, []), (b, [])]
--     entry2 <- block `named` "entry2"
--     entry3 <- block `named` "entry2"
--     ret result
--   return foo
main :: IO ()
main = do
    contents <- TS.getContents
    (blockified, blockifier) <- blockifyProcedure . flatten . either undefined id . parse procedure . either undefined id . parse tokenize $ contents
    let translated =  flip evalState initTranslState
            { translControlFlow = controlFlow blockifier
            , translBlockData = blockData blockifier
            , translBlocksTable = Map.fromList . (swap <$>) . Map.toList $ blocksTable blockifier
            } $ execModuleBuilderT emptyModuleBuilder $ runIRBuilderT emptyIRBuilder $ translate blockified
    print $ show translated
    return ()

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

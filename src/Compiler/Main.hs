{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.State as State
import Data.Text as T
import Data.Text.IO as T
import Data.Void
import Text.Megaparsec hiding (parse)

import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Type as AST

-- import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

import Language.AST
import Language.AST.Flattener
import Language.Lexer
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
main = T.getContents >>= either print (\r -> blockifyProcedure (flatten r) >>= print . snd) . parse procedure . either undefined id . parse tokenize

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

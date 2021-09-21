{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.State as State
import Data.Text as T
import Data.Void
import Data.Text.IO as T
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

import Language.Parser
import Language.AST
import Language.Pretty()
import Prettyprinter(pretty)

-- main :: IO ()
-- main = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
--   function "add" [(i32, "foo"), (i32, "bar")] i32 $ \[a, b] -> mdo
--     entry <- block `named` "entry"; do
--       c <- add a b
--       ret c

main :: IO ()
main = T.getContents >>= either print (print . pretty) . parse

parse :: Text
  -> Either
     (ParseErrorBundle Text Void) (Annot Unit SourcePos)
parse = runParser program "stdin"

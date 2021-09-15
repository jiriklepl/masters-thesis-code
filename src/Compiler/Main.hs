{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.State as State
import Data.Text as T
import Data.Text.IO as T
import Text.Megaparsec

import Language.Parser

import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Type as AST
-- import Data.Text.Lazy.IO as T
import LLVM.Pretty -- from the llvm-hs-pretty package

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

-- main :: IO ()
-- main = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
--   function "add" [(i32, "foo"), (i32, "bar")] i32 $ \[a, b] -> mdo
--     entry <- block `named` "entry"; do
--       c <- add a b
--       ret c
main :: IO ()
main = do
  contents <- T.getContents
  print $ runParser program "stdin" contents

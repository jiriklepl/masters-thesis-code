{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Text.Megaparsec
import Data.Text as T
import Control.Monad.State as State

import Parser

import Data.Text.Lazy.IO as T

import LLVM.Pretty  -- from the llvm-hs-pretty package
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

main :: IO ()
main = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo
  function "add" [(i32, "foo"), (i32, "bar")] i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c

-- main :: IO ()
-- main = do
--     contents <- T.getContents
--     print $ runParser unit "stdin" contents

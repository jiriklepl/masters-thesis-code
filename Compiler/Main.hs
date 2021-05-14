module Main where

import Text.Megaparsec
import Data.Text as T
import Data.Text.IO as T
import Control.Monad.State as State

import Parser

main :: IO ()
main = do
    contents <- T.getContents
    print . flip State.runState initState $ runParserT unit "stdin" contents

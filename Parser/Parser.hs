module Parser (someFunc) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T
import qualified Control.Monad.State as State
import Language

data ParserState = ParserState
    { dummy1 :: [String]
    , dummy2 :: [String]
    }

type StateMonad = State ParserState
type Parser a = ParsecT Void T.Text StateMonad (a Int)

fib a = 10 :: Int

-- | Parses the whole 'Unit'
unit :: Parser Unit
unit = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"

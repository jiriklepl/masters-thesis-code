{-# LANGUAGE Safe #-}

module QuasiQuotes where

import Language.Haskell.TH
import Language.Haskell.TH.Quote


text :: QuasiQuoter
text = QuasiQuoter {
    quoteExp= return . LitE . StringL . normaliseNewlines,
    quotePat= const $ fail "pattern not implemented",
    quoteType= const $ fail "type not implemented",
    quoteDec= const $ fail "declaration not implemented"}

normaliseNewlines :: String -> String
normaliseNewlines []             = []
normaliseNewlines ('\r':'\n':cs) = '\n':normaliseNewlines cs
normaliseNewlines ('\r':cs)      = '\n':normaliseNewlines cs
normaliseNewlines (c:cs)         = c:normaliseNewlines cs

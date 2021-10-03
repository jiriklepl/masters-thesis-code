{-# LANGUAGE Safe #-}

module QuasiQuotes where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

text :: QuasiQuoter
text =
  QuasiQuoter
    { quoteExp = return . LitE . StringL . normalizeNewlines
    , quotePat = const $ fail "pattern not implemented"
    , quoteType = const $ fail "type not implemented"
    , quoteDec = const $ fail "declaration not implemented"
    }

normalizeNewlines :: String -> String
normalizeNewlines [] = []
normalizeNewlines ('\r':'\n':cs) = '\n' : normalizeNewlines cs
normalizeNewlines ('\r':cs) = '\n' : normalizeNewlines cs
normalizeNewlines (c:cs) = c : normalizeNewlines cs

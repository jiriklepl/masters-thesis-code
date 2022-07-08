{-# LANGUAGE Safe #-}

module QuasiQuotes where

import safe Language.Haskell.TH (Exp(LitE), Lit(StringL))
import safe Language.Haskell.TH.Quote
  ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType)
  )

text :: QuasiQuoter
text =
  QuasiQuoter
    { quoteExp = return . LitE . StringL . normalizeNewlines
    , quotePat = const $ fail "pattern not implemented"
    , quoteType = const $ fail "type not implemented"
    , quoteDec = const $ fail "declaration not implemented"
    }

normalizeNewlines :: String -> String
normalizeNewlines =
  \case
    [] -> []
    '\r':'\n':cs -> '\n' : normalizeNewlines cs
    '\r':cs -> '\n' : normalizeNewlines cs
    c:cs -> c : normalizeNewlines cs

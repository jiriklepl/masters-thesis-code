{-# LANGUAGE Safe #-}

module CMM.Data.Either where

import safe Data.Bifunctor (bimap, first)
import safe Data.Functor (($>))

oneRight :: [Either a b] -> Either [a] b
oneRight =
  \case
    [] -> Left []
    Left a:others -> first (a :) $ oneRight others
    Right b:others -> allLeft others $> b
  where
    allLeft =
      \case
        Left a:rest -> bimap (a :) (a :) $ allLeft rest
        Right _:rest -> allLeft rest >>= Left
        [] -> Right []

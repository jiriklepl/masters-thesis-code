{-# LANGUAGE Safe #-}

module CMM.Data.Either where

import safe Prelude

import safe Data.Bifunctor (bimap, first)
import safe Data.Functor (($>))

oneRight :: [Either a b] -> Either [a] b
oneRight [] = Left []
oneRight (Left a:others) = first (a :) $ oneRight others
oneRight (Right b:others) = allLeft others $> b
  where
    allLeft (Left a:rest) = bimap (a :) (a :) $ allLeft rest
    allLeft (Right _:rest) = allLeft rest >>= Left
    allLeft [] = Right []

{-# LANGUAGE Safe #-}

module Language.Utils where

import safe qualified Data.Text as T
import safe Data.Text (Text)
import safe Data.Maybe

addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons ':' text

getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= ':')

hasPrefix :: Text -> Bool
hasPrefix = isNothing . T.find (== ':')

{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Utils where

import safe Control.Monad
import safe Data.Maybe
import safe qualified Data.Text as T
import safe Data.Text (Text)

addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons ':' text

getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= ':')

hasPrefix :: Text -> Bool
hasPrefix = isJust . T.find (== ':')

while :: Monad m => m Bool -> m a -> m ()
while cond action = cond >>= flip when (action *> while cond action)

doWhile :: Monad m => m Bool -> m ()
doWhile action = action >>= flip when (doWhile action)

repeatUntil :: Monad m => m Bool -> m ()
repeatUntil action = action >>= flip unless (repeatUntil action)

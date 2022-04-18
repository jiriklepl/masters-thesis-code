{-# LANGUAGE Safe #-}

module CMM.Utils where

import safe Prelude

import safe Control.Monad (unless, when)
import safe Data.Maybe (isJust)
import safe Data.String (IsString)
import safe qualified Data.Text as T
import safe Data.Text (Text)

addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons ':' text

getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= ':')

hasPrefix :: Text -> Bool
hasPrefix = isJust . T.find (== ':')

-- | Implementation of the standard while loop
while :: Monad m => m Bool -> m a -> m ()
while cond action = cond >>= flip when (action *> while cond action)

-- | Implementation of the standard do-while loop (condition appended to the do-block)
doWhile :: Monad m => m Bool -> m ()
doWhile action = action >>= flip when (doWhile action)

-- | Implementation of the standard repeat-until loop (condition appended to the repeat-block)
repeatUntil :: Monad m => m Bool -> m ()
repeatUntil action = action >>= flip unless (repeatUntil action)

backQuote :: (IsString a, Semigroup a) => a -> a
backQuote string = "`" <> string <> "`"

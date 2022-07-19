{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

{-|
Module      : CMM.Utils
Description : Various general utilities
Maintainer  : jiriklepl@seznam.cz

This module contains various utilities used in other modules
-}

module CMM.Utils where

import safe Control.Monad (unless, when)
import safe Data.Bifunctor (Bifunctor(second))
import safe Data.Maybe (isJust)
import safe Data.String (IsString)
import safe qualified Data.Text as T
import safe Data.Text (Text)

#ifdef USE_GHC_STACK
import safe qualified GHC.Stack
type HasCallStack = GHC.Stack.HasCallStack
#else
class HasCallStack

instance HasCallStack
#endif

-- | used to separate the prefix from the rest of the name
prefixSeparator :: Char
prefixSeparator = '$'

-- | Adds a layer prefix to a name
addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons prefixSeparator text

-- | Retrieves the layer prefix prepended to the given name
getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= prefixSeparator)

-- | for a prefixed name, returns a pair with the prefix
--   as the first element and the rest of the name as the second
extractPrefix :: Text -> (Text, Text)
extractPrefix = second T.tail . T.span (/= prefixSeparator)

-- | splits the input name by the `prefixSeparator`
splitName :: Text -> [Text]
splitName name
  | T.null rest = [first]
  | otherwise = first : splitName (T.tail rest)
  where
    (first, rest) = T.span (/= prefixSeparator) name

prefixesElem :: Text -> Text -> Bool
prefixesElem prefix = elem prefix . init . splitName

-- | Checks whether the given name has a layer prefix
hasPrefix :: Text -> Bool
hasPrefix = isJust . T.find (== prefixSeparator)

fmapTrivial :: Functor f => f a -> f b
fmapTrivial = fmap $ error "The provided functor is not trivial"

-- | Implementation of the standard while loop
while :: Monad m => m Bool -> m a -> m ()
while cond action = cond >>= flip when (action *> while cond action)

-- | Implementation of the standard do-while loop (condition appended to the do-block)
doWhile :: Monad m => m Bool -> m ()
doWhile action = action >>= flip when (doWhile action)

-- | Implementation of the standard repeat-until loop (condition appended to the repeat-block)
repeatUntil :: Monad m => m Bool -> m ()
repeatUntil action = action >>= flip unless (repeatUntil action)

-- | wraps the input in backQuotes
backQuote :: (IsString a, Semigroup a) => a -> a
backQuote string = "`" <> string <> "`"

-- | transforms the input to text and wraps it in backQuotes
backQuoteShow :: Show a => a -> String
backQuoteShow = backQuote . show

-- | Signifies a logic error
logicError :: HasCallStack => a
logicError = error "logicError"

-- | Signifies that the given feature is not yet implemented
notYetImplemented :: HasCallStack => a
notYetImplemented = error "not yet implemented"

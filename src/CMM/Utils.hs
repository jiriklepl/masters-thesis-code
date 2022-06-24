{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module CMM.Utils where

import safe Control.Applicative (Applicative((*>)))
import safe Control.Monad (Monad((>>=)), unless, when)
import safe Data.Bool (Bool, otherwise)
import safe Data.Eq (Eq((/=), (==)))
import safe Data.Function ((.), flip)
import safe Data.Maybe (isJust)
import safe Data.Semigroup (Semigroup((<>)))
import safe Data.String (IsString, String)
import safe qualified Data.Text as T
import safe Data.Text (Text)
import safe Data.Char ( Char )
import safe Data.Bifunctor ( Bifunctor(second) )
import safe Data.List ( elem, init )
import safe Text.Show (Show(show))

#ifdef USE_GHC_STACK
import safe qualified GHC.Stack
#endif

prefixSeparator :: Char
prefixSeparator = ':'

-- | Adds a layer prefix to a name
addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons prefixSeparator text

-- | Retrieves the layer prefix prepended to the given name
getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= prefixSeparator)

extractPrefix :: Text -> (Text, Text)
extractPrefix = second T.tail . T.span (/= prefixSeparator)

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

backQuoteShow :: Show a => a -> String
backQuoteShow = backQuote . show

#ifdef USE_GHC_STACK
type HasCallStack = GHC.Stack.HasCallStack
#else
class HasCallStack
instance HasCallStack
#endif

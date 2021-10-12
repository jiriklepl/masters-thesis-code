{-# LANGUAGE Safe #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Utils where

import safe qualified Data.Text as T
import safe Data.Text (Text)
import safe Data.Maybe
import safe Control.Monad

import safe Peano

addPrefix :: Text -> Text -> Text
addPrefix prefix text = prefix <> T.cons ':' text

getPrefix :: Text -> Text
getPrefix = T.takeWhile (/= ':')

hasPrefix :: Text -> Bool
hasPrefix = isNothing . T.find (== ':')

while :: Monad m => m Bool -> m a -> m ()
while cond action = cond >>= flip when (action *> while cond action)

doWhile :: Monad m => m Bool -> m ()
doWhile action = action >>= flip when (doWhile action)

repeatUntil :: Monad m => m Bool -> m ()
repeatUntil action = action >>= flip unless (repeatUntil action)

-- | Implements a generic getter indexed by `Peano` numbers for various types
class Nat n => Getter n a b | n a -> b where
    -- | A generic getter indexed by `Peano` numbers
    get_nth :: n -> a -> b

instance Getter P0 (a, b) a where
    get_nth _ = fst

instance Getter P0 (a, b, c) a where
    get_nth _ (a, _, _) = a

instance Getter P0 (a, b, c, d) a where
    get_nth _ (a, _, _, _) = a

instance Getter P0 (a, b, c, d, e) a where
    get_nth _ (a, _, _, _, _) = a

instance Getter P0 (a, b, c, d, e, f) a where
    get_nth _ (a, _, _, _, _, _) = a

instance Getter P0 (a, b, c, d, e, f, g) a where
    get_nth _ (a, _, _, _, _, _, _) = a

instance Getter P0 (a, b, c, d, e, f, g, h) a where
    get_nth _ (a, _, _, _, _, _, _, _) = a

instance Getter P1 (a, b) b where
    get_nth _ = snd

instance Getter P1 (a, b, c) b where
    get_nth _ (_, b, _) = b

instance Getter P1 (a, b, c, d) b where
    get_nth _ (_, b, _, _) = b

instance Getter P1 (a, b, c, d, e) b where
    get_nth _ (_, b, _, _, _) = b

instance Getter P1 (a, b, c, d, e, f) b where
    get_nth _ (_, b, _, _, _, _) = b

instance Getter P1 (a, b, c, d, e, f, g) b where
    get_nth _ (_, b, _, _, _, _, _) = b

instance Getter P1 (a, b, c, d, e, f, g, h) b where
    get_nth _ (_, b, _, _, _, _, _, _) = b

instance Getter P2 (a, b, c) c where
    get_nth _ (_, _, c) = c

instance Getter P2 (a, b, c, d) c where
    get_nth _ (_, _, c, _) = c

instance Getter P2 (a, b, c, d, e) c where
    get_nth _ (_, _, c, _, _) = c

instance Getter P2 (a, b, c, d, e, f) c where
    get_nth _ (_, _, c, _, _, _) = c

instance Getter P2 (a, b, c, d, e, f, g) c where
    get_nth _ (_, _, c, _, _, _, _) = c

instance Getter P2 (a, b, c, d, e, f, g, h) c where
    get_nth _ (_, _, c, _, _, _, _, _) = c

instance Getter P3 (a, b, c, d) d where
    get_nth _ (_, _, _, d) = d

instance Getter P3 (a, b, c, d, e) d where
    get_nth _ (_, _, _, d, _) = d

instance Getter P3 (a, b, c, d, e, f) d where
    get_nth _ (_, _, _, d, _, _) = d

instance Getter P3 (a, b, c, d, e, f, g) d where
    get_nth _ (_, _, _, d, _, _, _) = d

instance Getter P3 (a, b, c, d, e, f, g, h) d where
    get_nth _ (_, _, _, d, _, _, _, _) = d

instance Getter P4 (a, b, c, d, e) e where
    get_nth _ (_, _, _, _, e) = e

instance Getter P4 (a, b, c, d, e, f) e where
    get_nth _ (_, _, _, _, e, _) = e

instance Getter P4 (a, b, c, d, e, f, g) e where
    get_nth _ (_, _, _, _, e, _, _) = e

instance Getter P4 (a, b, c, d, e, f, g, h) e where
    get_nth _ (_, _, _, _, e, _, _, _) = e

instance Getter P5 (a, b, c, d, e, f) f where
    get_nth _ (_, _, _, _, _, f) = f

instance Getter P5 (a, b, c, d, e, f, g) f where
    get_nth _ (_, _, _, _, _, f, _) = f

instance Getter P5 (a, b, c, d, e, f, g, h) f where
    get_nth _ (_, _, _, _, _, f, _, _) = f

instance Getter P6 (a, b, c, d, e, f, g) g where
    get_nth _ (_, _, _, _, _, _, g) = g

instance Getter P6 (a, b, c, d, e, f, g, h) g where
    get_nth _ (_, _, _, _, _, _, g, _) = g

instance Getter P7 (a, b, c, d, e, f, g, h) h where
    get_nth _ (_, _, _, _, _, _, _, h) = h

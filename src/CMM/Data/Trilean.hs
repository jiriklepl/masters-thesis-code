{-# LANGUAGE Safe #-}

module CMM.Data.Trilean where

import safe Data.Bool (Bool)
import safe qualified Data.Bool as B
import safe Data.Data (Data)
import safe Data.Eq (Eq)
import safe Data.Ord (Ord)
import safe Text.Read (Read)
import safe Text.Show (Show)

import safe CMM.Data.Bounded (Bounded(maxBound, minBound))
import safe CMM.Data.Nullable (Fallbackable((??)), Nullable(nullVal))

-- | Data type used for three-valued logic (with added 'Unknown' value)
data Trilean
  = False
  -- ^ The value signifies falsehood
  | Unknown
  -- ^ The value signifies indeterminate truth/falsehood
  | True
  -- ^ The value signifies truth
  deriving (Eq, Ord, Show, Read, Data)

instance Bounded Trilean where
  minBound = False
  maxBound = True

instance Nullable Trilean where
  nullVal = Unknown

instance Fallbackable Trilean where
  Unknown ?? r = r
  l ?? _ = l

-- | AND operation on two 'Trilean' values
(&&) :: Trilean -> Trilean -> Trilean
False && _ = False
_ && False = False
True && True = True
_ && _ = Unknown

-- | OR operation on two 'Trilean' values
(||) :: Trilean -> Trilean -> Trilean
True || _ = True
_ || True = True
False || False = False
_ || _ = Unknown

-- | NOT operation on a 'Trilean' value
not :: Trilean -> Trilean
not =
  \case
    False -> True
    Unknown -> Unknown
    True -> False

-- | 'True'
otherwise :: Trilean
otherwise = True

-- | Case analysis for the 'Trilean' type.
-- @'trilean' x y w p@ evaluates to @x@ when @p@ is 'False', @y@ when @p@ is 'Unknown', and finally evaluates to @z@ when @p@ is 'True'
trilean :: a -> a -> a -> Trilean -> a
trilean a b c =
  \case
    False -> a
    Unknown -> b
    True -> c

-- | transforms a 'Bool' value to an equivalent 'Trilean' value
fromBool :: Bool -> Trilean
fromBool =
  \case
    B.True -> True
    B.False -> False

-- | transforms a 'Trilean' value to an equivalent 'Bool' value with a fallback used on 'Unknown'-valued argument
toBool :: Bool -> Trilean -> Bool
toBool fallback = trilean B.False fallback B.True

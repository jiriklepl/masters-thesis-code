{-# LANGUAGE Safe #-}

module CMM.Data.Way where

-- | An object representing a 1D direction
data Way
  = Backward
  | Both
  | Forward
  deriving (Eq, Show, Read)

instance Semigroup Way where
  Both <> _ = Both
  _ <> Both = Both
  Backward <> Forward = Both
  Forward <> Backward = Both
  Backward <> Backward = Backward
  Forward <> Forward = Forward

-- | Reverses the `Way` object
otherWay :: Way -> Way
otherWay =
  \case
    Backward -> Forward
    Both -> Both
    Forward -> Backward

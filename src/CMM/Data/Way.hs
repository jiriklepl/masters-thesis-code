{-# LANGUAGE Safe #-}

module CMM.Data.Way where

import safe Data.Eq ( Eq )
import safe Data.Semigroup ( Semigroup((<>)) )
import safe Text.Show ( Show )
import safe Text.Read ( Read )

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

otherWay :: Way -> Way
otherWay = \case
  Backward -> Forward
  Both -> Both
  Forward -> Backward

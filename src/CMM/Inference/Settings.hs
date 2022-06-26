{-# LANGUAGE Safe #-}

module CMM.Inference.Settings where

import safe Data.Eq ( Eq )
import safe Data.Ord ( Ord )
import safe Text.Show ( Show )
import safe Data.Data ( Data )

data InferencerSettings = InferencerSettings
  deriving (Eq, Ord, Show, Data)

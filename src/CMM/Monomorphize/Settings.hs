{-# LANGUAGE Safe #-}

module CMM.Monomorphize.Settings where

import safe Data.Eq ( Eq )
import safe Data.Ord ( Ord )
import safe Text.Show ( Show )
import safe Data.Data ( Data )

data MonomorphizerSettings = MonomorphizerSettings
  deriving (Eq, Ord, Show, Data)

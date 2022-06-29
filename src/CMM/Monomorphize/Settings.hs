{-# LANGUAGE Safe #-}

module CMM.Monomorphize.Settings where

import safe Data.Data ( Data )

data MonomorphizerSettings = MonomorphizerSettings
  deriving (Eq, Ord, Show, Data)

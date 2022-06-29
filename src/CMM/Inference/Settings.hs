{-# LANGUAGE Safe #-}

module CMM.Inference.Settings where

import safe Data.Data ( Data )

data InferencerSettings = InferencerSettings
  deriving (Eq, Ord, Show, Data)

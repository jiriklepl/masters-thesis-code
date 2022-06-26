{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Settings where

import safe Data.Eq ( Eq )
import safe Data.Ord ( Ord )
import safe Data.Data ( Data )
import safe Text.Show ( Show )

data PreprocessorSettings = PreprocessorSettings
  deriving (Eq, Ord, Show, Data)

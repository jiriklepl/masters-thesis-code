{-# LANGUAGE Safe #-}

module CMM.Inference.Preprocess.Settings where

import safe Data.Data ( Data )

data PreprocessorSettings = PreprocessorSettings
  deriving (Eq, Ord, Show, Data)

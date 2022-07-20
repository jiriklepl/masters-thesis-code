{-# LANGUAGE Safe #-}

module Main
  ( main
  ) where

import safe CMM.Pipeline (runPipeline)

main :: IO ()
main = runPipeline

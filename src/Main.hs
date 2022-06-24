{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Control.Lens.Getter ((^.), use, view)
import Control.Monad.State as State (Monad(return), runState)
import Data.Either (Either(Left, Right), either)
import Data.Function (($), (.), id)
import Data.List (head, reverse)
import Data.Monoid (Monoid(mappend, mempty))
import GHC.Err (undefined)

import qualified Data.Text.IO as TS

-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
import safe System.IO (IO, print, putStrLn)

-- import Data.Tuple
import Text.Megaparsec hiding (parse)

-- import Data.Text.Lazy as T
-- import Data.Text.Lazy.IO as T
-- import LLVM.Pretty -- from the llvm-hs-pretty package
-- import LLVM.IRBuilder.Module
-- import LLVM.IRBuilder.Monad
import Prettyprinter

import CMM.AST.Annot
import CMM.AST.Blockifier
import qualified CMM.AST.Blockifier.State as B
import CMM.AST.Flattener
import CMM.AST.Variables
import CMM.Inference as Infer
import CMM.Inference.Preprocess as Infer
import CMM.Inference.Preprocess.State as Infer
import qualified CMM.Inference.Preprocess.State
import CMM.Inference.State as InferState

import safe CMM.Inference.HandleCounter
  ( HasHandleCounter(handleCounter)
  , setHandleCounter
  )

-- import CMM.Inference.Type as Infer
-- import CMM.Inference.TypeKind as Infer
import CMM.Lexer
import CMM.Monomorphize (Monomorphize(monomorphize))
import qualified CMM.Monomorphize.State as Infer
import CMM.Parser
import CMM.Pretty ()
import Data.Bifunctor
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple
import GHC.Show
import CMM.Err.State

-- import CMM.Translator
-- import qualified CMM.Translator.State as Tr
-- import Data.Foldable (traverse_)
main :: IO ()
main = do
  contents <- TS.getContents
  let tokens' = either undefined id $ parse tokenize contents
  let ast = either undefined id $ parse unit tokens'
  -- let flattened = flatten ast
  -- print $ pretty flattened
  let (mined, miner) = runState (preprocess ast) initPreprocessor
  -- let (_, _) = runState (blockify flattened) B.initBlockifier
  -- let translated =
  --       ppllvm $
  --       flip
  --         evalState
  --         Tr.initTranslState
  --           { Tr._controlFlow = B._controlFlow blockifier
  --           , Tr._blockData = B._blockData blockifier
  --           , Tr._blocksTable =
  --               Map.fromList . (swap <$>) . Map.toList $
  --               B._blocksTable blockifier
  --           } $
  --       buildModuleT "llvm" $
  --       runIRBuilderT emptyIRBuilder $ translate blockified
  -- T.putStr translated
  let _ = globalVariables $ unAnnot ast
      oldCounter = view handleCounter miner
      fs = reverse . head $ view CMM.Inference.Preprocess.State.facts miner
  print . sep $ pretty <$> fs
  let (fs', inferencer) =
        (`runState` InferState.initInferencer) $ do
          setHandleCounter oldCounter
          mineAST mined
          reduce fs
  let (msg, (inferencer', monomorphizer)) =
        (`runState` (inferencer, Infer.initMonomorphizeState)) $ do
          monomorphize mempty mined <&> \case
            Left what -> show what
            Right mined' -> show $ pretty mined'
  putStrLn msg
  print .
    vsep .
    fmap (uncurry mappend . bimap pretty (list . fmap pretty . Set.toList)) .
    Map.toList . Infer.getPolyGenerate $
    monomorphizer ^. Infer.polyMemory
  print "ERRORS:"
  print . vsep . fmap pretty . view errors $ inferencer ^. errorState
  print "CLASS_SCHEMES:"
  print . vsep . fmap (uncurry mappend . bimap pretty pretty) . Map.toList $
    inferencer ^. classSchemes
  print "CLASS_FACTS:"
  print .
    vsep .
    fmap (uncurry mappend . bimap pretty (vsep . fmap pretty)) . Map.toList $
    inferencer ^. classFacts
  print "SCHEMES:"
  print . vsep . fmap (uncurry mappend . bimap pretty pretty) . Map.toList $
    inferencer ^. schemes
  void $ return inferencer

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

{-# LANGUAGE Safe #-}

module Main where

import Control.Monad.State as State
    ( Monad(return, (>>=)),
      void,
      StateT(runStateT),
      MonadIO(liftIO),
      execStateT )
import Control.Lens.Getter (view)
import System.IO ( print, IO )
import Data.Either ( Either(..), either )
import Data.Monoid ( Monoid(mempty) )
import Data.List ( head )
import Data.Function ( ($), (.), id )
import GHC.Err ( undefined )

-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
import Data.Text.IO as TS

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
-- import CMM.Inference.Type as Infer
-- import CMM.Inference.TypeKind as Infer
import CMM.Lexer
import CMM.Monomorphize (Monomorphize(monomorphize))
import CMM.Monomorphize.Monomorphized as Infer
import CMM.Parser
import CMM.Pretty ()

-- import CMM.Translator
-- import qualified CMM.Translator.State as Tr
-- import Data.Foldable (traverse_)
main :: IO ()
main = do
  contents <- TS.getContents
  let tokens' = either undefined id $ parse tokenize contents
  let ast = either undefined id $ parse unit tokens'
  let flattened = flatten ast
  (mined, miner) <- runStateT (preprocess ast) initInferPreprocessor
  (_, _) <- runStateT (blockify flattened) B.initBlockifier
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
  _ <- globalVariables $ unAnnot ast
  -- print $ CMM.Inference.Preprocess.State._facts miner
  inferencer <-
    (`execStateT` InferState.initInferencer
                    (CMM.Inference.Preprocess.State._handleCounter miner)) $ do
      let fs = head $ CMM.Inference.Preprocess.State._facts miner
      mineAST mined
      -- liftIO $ print fs
      void $ reduce fs
      monomorphize mempty mined >>= \case
        Left what -> liftIO $ print what
        Right mined' -> liftIO . print . pretty $ view Infer.node mined'
  -- liftIO $ print inferencer
  void $ return inferencer

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

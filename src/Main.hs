
module Main where

import Control.Lens.Getter ((^.), view)
import Control.Monad.State as State (Monad(return), runState, evalState, (>>=), MonadState (get), sequence, MonadTrans (lift), liftM, StateT (runStateT))

import qualified Data.Text.IO as TS

-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T
-- import qualified Data.Map as Map
-- import Control.Lens
-- import Data.Text as T

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

import CMM.Inference.HandleCounter
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
import qualified CMM.Inference.State.Impl as InferState
import CMM.Inference.Preprocess.Settings
import CMM.Inference.Settings
import CMM.Monomorphize.Settings (MonomorphizerSettings(MonomorphizerSettings))
import CMM.Inference.State.Impl
import qualified CMM.Translator.State as Tr
import qualified Data.Text.Lazy.IO as T
import Data.Maybe
import LLVM.Pretty
import LLVM.IRBuilder
import CMM.Translator
import LLVM.IRBuilder.Internal.SnocList (SnocList(SnocList))
import LLVM.AST
import CMM.AST

-- import CMM.Translator
-- import qualified CMM.Translator.State as Tr
-- import Data.Foldable (traverse_)
main :: IO ()
main = do
  contents <- TS.getContents
  let tokens' = either (error . show) id $ parse tokenize contents
  let ast = either (error . show) id $ parse unit tokens'
  let flattened = flatten ast
  -- print $ pretty flattened
  let (blockified, blockifier) = blockify flattened `runState` B.initBlockifier
  let (mined, miner) = runState (preprocess blockified) $ initPreprocessor PreprocessorSettings {}
  let _ = globalVariables $ unAnnot ast
      oldCounter = view handleCounter miner
      fs = reverse . head $ view CMM.Inference.Preprocess.State.facts miner
  -- print . sep $ pretty <$> fs
  print . pretty $ view errorState blockifier
  print . pretty $ blockified
  let (fs', inferencer) =
        (`runState` InferState.initInferencer InferencerSettings {}) $ do
          setHandleCounter oldCounter
          mineAST mined
          reduce fs
  print . pretty $ view errorState inferencer
  let (msg, (inferencer', monomorphizer)) =
        (`runState` (inferencer, Infer.initMonomorphizeState MonomorphizerSettings)) $ do
          monomorphize mempty mined <&> \case
            Left what -> error $ show $ pretty what
            Right mined' -> fromJust mined'
  -- let moduleBuilder = runFreshIRBuilderT $ translate msg
  --     translator = execFreshModuleBuilderT moduleBuilder
  --     translated =
  --       evalState translator $
  --         Tr.initTranslState
  --           { Tr._controlFlow = B._controlFlow blockifier
  --           , Tr._blockData = B._blockData blockifier
  --           , Tr._blocksTable =
  --               Map.fromList . (swap <$>) . Map.toList $
  --               B._blocksTable blockifier
  --           }

  --     module' = defaultModule { moduleName = "", moduleDefinitions = translated }
  --     prettyprinted = evalModuleBuilder emptyModuleBuilder{builderDefs=SnocList translated} $ ppllvm module'

  putStr . show $ pretty msg
  print "ERRORS:"
  print . pretty $ inferencer' ^. errorState
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
  return ()

parse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse parser = runParser parser "stdin"

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : CMM.Pipeline
Description : The compiler pipeline
Maintainer  : jiriklepl@seznam.cz

This module contains the compiler pipeline (high-level logic)
-}
module CMM.Pipeline where

import safe Control.Lens (uses, view)
import safe Control.Monad (when)
import safe Control.Monad.State (runState)
import safe Data.Bifunctor (Bifunctor(first), second)
import safe Data.Data (Data, Typeable)
import safe Data.Text (Text)
import safe qualified Data.Text.IO as T
import safe LLVM.AST (Module(moduleDefinitions), defaultModule, moduleName)
import LLVM.IRBuilder
  ( ModuleBuilderState(builderDefs)
  , emptyModuleBuilder
  , evalModuleBuilder
  , execFreshModuleBuilderT
  , runFreshIRBuilderT
  )
import safe LLVM.IRBuilder.Internal.SnocList (SnocList(SnocList))
import safe LLVM.Pretty (ppllvm)
import safe Options.Applicative (execParser)
import safe System.Exit (die)
import safe System.IO (IOMode(ReadMode), hPrint, stderr, stdin, withFile)
import safe Text.Megaparsec (SourcePos, errorBundlePretty, runParser)

import safe Prettyprinter (Pretty(pretty), (<+>))

import safe CMM.AST (Unit)
import safe CMM.AST.Annot (Annot)
import safe CMM.AST.BlockAnnot (BlockAnnot)
import safe CMM.AST.Blockifier (Blockify(blockify))
import safe CMM.AST.Blockifier.State
  ( BlockifierState
  , BlockifyAssumps
  , initBlockifier
  )
import safe CMM.AST.Flattener (flatten)
import safe CMM.Data.Nullable (Nullable(nullVal))
import safe CMM.Err.IsError (IsError)
import safe CMM.Err.State
  ( ErrorState
  , HasErrorState(errorState)
  , countErrors
  , registerError
  )
import safe CMM.FillElabs (FillElabs(fillHoles))
import safe CMM.Inference (reduce)
import safe CMM.Inference.Fact (Fact, Facts)
import safe CMM.Inference.HandleCounter (handleCounter)
import safe CMM.Inference.Preprocess (Preprocess(preprocess))
import safe CMM.Inference.Preprocess.Elaboration (Elaboration, HasElaboration)
import safe CMM.Inference.Preprocess.State
  ( PreprocessorState
  , facts
  , initPreprocessor
  )
import safe CMM.Inference.State (InferencerState)
import safe CMM.Inference.State.Impl (initInferencer)
import safe CMM.Lexer (tokenize)
import safe CMM.Lexer.Token (Token)
import safe CMM.Mangle (Mangle(mangle))
import safe CMM.Monomorphize (Monomorphize(monomorphize))
import safe CMM.Monomorphize.Error
  ( MonomorphizeError(InstantiatesToNothing)
  , voidWrapped
  )
import safe CMM.Monomorphize.State (MonomorphizeState, initMonomorphizeState)
import safe CMM.Options
  ( Options(handleStart, input, monoSrc, output, prettify, quiet)
  , blockifySrc
  , flattenSrc
  , noTransl
  , opts
  , preprocessSrc
  )
import safe CMM.Parser (program)
import safe CMM.Parser.GetPos (GetPos)
import safe CMM.Pretty (prettyShow)
import safe CMM.Translator (TranslAnnotAssumps, translate)
import safe CMM.Translator.State (TranslState, initTranslState)

newtype PipelineError =
  InferenceIncomplete Facts
  deriving (Show, Eq, IsError, Data)

instance Pretty PipelineError where
  pretty (InferenceIncomplete fs) =
    "Type inference could not resolve the following constraints:" <+> pretty fs

-- | runs the compiler pipeline
runPipeline :: IO ()
runPipeline = execParser opts >>= runWithOptions

-- | runs the compiler pipeline with the given options
runWithOptions :: Options -> IO ()
runWithOptions options = do
  case input options of
    "-" -> runOnInput stdin
    name -> withFile name ReadMode runOnInput
  where
    fileName =
      case input options of
        "-" -> "stdin" -- name the input stdin
        name -> name
    runOnInput input = do
      (prepared, errState) <- prepareOnInput input
      when (errState /= nullVal) . hPrint stderr $
        pretty errState -- prints all warnings and infos
      if quiet options
        then return () -- prints nothing
        else case output options of
               "-" -> putStrLn prepared -- prints to stdout
               name -> writeFile name (prepared <> "\n") -- prints to the file `name`
    prepareOnInput input = do
      contents <- T.hGetContents input -- gets the content of the file
      case runAll fileName options contents of
        Left prettyError -> die prettyError -- prints errors and dies
        Right result -> return result

-- | runs the compiler pipeline as an `Either String` monad
runAll :: String -> Options -> Text -> Either String (String, ErrorState)
runAll fileName options contents = do
  tokens <- tokenizer fileName contents
  ast <- parser fileName tokens
  runParsed options ast

-- | runs the compiler pipeline on a parsed program
runParsed ::
     Options -> Annot Unit SourcePos -> Either String (String, ErrorState)
runParsed options ast
  | prettify options = return (prettyShow ast, nullVal)
  | monoSrc options
    -- we output the unflattened monomorphized source
   = do
    (ast'', _, errState) <- runInferMono options ast
    return (prettyShow ast'', errState)
  | flattenSrc options = return (prettyShow ast', nullVal)
  | otherwise = runFlattened options ast'
  where
    ast' = flattener ast

runFlattened ::
     Options -> Annot Unit SourcePos -> Either String (String, ErrorState)
runFlattened options ast = do
  (ast', bState, errState) <- blockifier ast
  if blockifySrc options
    then return (show ast', errState)
    else second (errState <>) <$> runBlockified options bState ast'

-- | runs the compiler pipeline on a parsed and blockified program
runBlockified ::
     Options
  -> BlockifierState
  -> Annot Unit (SourcePos, BlockAnnot)
  -> Either String (String, ErrorState)
runBlockified options bState ast = do
  (ast', ~(Right iState), errState) <- runInferMono options ast
  if preprocessSrc options
    then return (show ast', errState)
    else second (errState <>) <$> runMonomorphized options bState iState ast'

-- | runs the last, translation, phases of the compiler
runMonomorphized ::
     Options
  -> BlockifierState
  -> InferencerState
  -> Annot Unit ((SourcePos, BlockAnnot), Elaboration)
  -> Either String (String, ErrorState)
runMonomorphized options bState iState ast =
  if noTransl options
    then return (prettyShow ast, nullVal) -- prettyprint flattened and monomorphized source
    else do
      (ast', _, errState') <- translator options iState bState ast
      -- prettyprint the llvm representation of the source
      return (ast', errState')

-- | runs the inference phase and the monomorphizer phase
runInferMono ::
     (GetPos a, GetPos (a, Elaboration), Data a)
  => Options
  -> Annot Unit a
  -> Either String ( Annot Unit (a, Elaboration)
                   , Either PreprocessorState InferencerState
                   , ErrorState)
runInferMono options ast = do
  ((prepAST, facts'), pState, errState) <- preprocessor options ast
  if preprocessSrc options
    then return (prepAST, Left pState, errState)
    else do
      ((), iState, errState') <-
        inferencer options {handleStart = view handleCounter pState} facts'
      (monoAST, (iState', _)) <- monomorphizer options iState prepAST
      (ast', iState'', errState'') <- postprocessor iState' monoAST
      return (ast', Right iState'', errState <> errState' <> errState'')

-- | runs the tokenization
tokenizer :: String -> Text -> Either String [Annot Token SourcePos]
tokenizer source = first errorBundlePretty . runParser tokenize source

-- | runs the parsing phase
parser ::
     String -> [Annot Token SourcePos] -> Either String (Annot Unit SourcePos)
parser source = first errorBundlePretty . runParser program source

-- | runs the flattener
flattener :: (Data (n a), Typeable a) => n a -> n a
flattener = flatten

-- | wraps the result in an `Either String` monad, choosing `Left` if the `ErrorState` contains any errors
wrapStandardLayout ::
     HasErrorState s ErrorState => (a, s) -> Either String (a, s, ErrorState)
wrapStandardLayout (result, state')
  | countErrors errState /= 0 = Left $ prettyShow errState
  | otherwise = Right (result, state', errState)
  where
    errState = view errorState state'

-- | runs the blockifier
blockifier ::
     (Blockify n a b, BlockifyAssumps a b)
  => n a
  -> Either String (n b, BlockifierState, ErrorState)
blockifier flattened =
  wrapStandardLayout $ blockify flattened `runState` initBlockifier

-- | runs the inference preprocessor
preprocessor ::
     (Preprocess n a (a, Elaboration), GetPos a)
  => Options
  -> Annot n a
  -> Either String ( (Annot n (a, Elaboration), [Fact])
                   , PreprocessorState
                   , ErrorState)
preprocessor settings ast =
  wrapStandardLayout $ action `runState` initPreprocessor settings
  where
    action = do
      ast' <- preprocess ast
      uses facts $ (ast', ) . reverse . head

-- | runs the inferencer
inferencer ::
     Options -> Facts -> Either String ((), InferencerState, ErrorState)
inferencer settings fs =
  wrapStandardLayout $ action `runState` initInferencer settings
  where
    action = do
      fs' <- snd <$> reduce fs
      if null fs'
        then return ()
        else registerError $ InferenceIncomplete fs'

-- | runs the monomorphizer
monomorphizer ::
     (GetPos a, HasElaboration a)
  => Options
  -> InferencerState
  -> Annot Unit a
  -> Either String (Annot Unit a, (InferencerState, MonomorphizeState a))
monomorphizer settings iState ast =
  case result of
    Left err -> Left $ prettyShow err
    Right Nothing -> Left . prettyShow . InstantiatesToNothing $ voidWrapped ast
    Right (Just unit') -> Right (unit', states)
  where
    (result, states) =
      monomorphize mempty ast `runState`
      (iState, initMonomorphizeState settings)

-- | runs the mangler and hole-filler
postprocessor ::
     (Data a, HasElaboration a, GetPos a)
  => InferencerState
  -> Annot Unit a
  -> Either String (Annot Unit a, InferencerState, ErrorState)
postprocessor iState ast = wrapStandardLayout $ action `runState` iState
  where
    action = fillHoles ast >>= mangle

-- | runs the translator
translator ::
     TranslAnnotAssumps a
  => Options
  -> InferencerState
  -> BlockifierState
  -> Annot Unit a
  -> Either String (String, TranslState, ErrorState)
translator _ iState bState ast = do
  let irBuilder = runFreshIRBuilderT $ translate ast
      moduleBuilder = execFreshModuleBuilderT irBuilder
      (translated, tState) =
        runState moduleBuilder $ initTranslState iState bState
      module' = defaultModule {moduleName = "", moduleDefinitions = translated}
      ast' =
        evalModuleBuilder emptyModuleBuilder {builderDefs = SnocList translated} $
        ppllvm module'
  wrapStandardLayout (prettyShow ast', tState)

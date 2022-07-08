{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Pipeline where

import safe Text.Megaparsec
    ( SourcePos, runParser, errorBundlePretty )
import safe CMM.Lexer ( tokenize )
import safe Data.Bifunctor ( Bifunctor(first) )
import safe Data.Text ( Text )
import safe CMM.AST.Annot ( Annot )
import safe CMM.Parser ( program )
import safe CMM.AST ( Unit )
import safe CMM.Inference.Preprocess.State
    ( PreprocessorState, initPreprocessor, facts )
import safe CMM.Inference.State ( InferencerState )
import safe CMM.Monomorphize.State
    ( MonomorphizeState, initMonomorphizeState )
import safe CMM.Inference.Preprocess.TypeHole
    ( HasTypeHole, TypeHole )
import safe CMM.Lexer.Token ( Token )
import safe Data.Data ( Data, Typeable )
import safe CMM.AST.Flattener ( flatten )
import safe CMM.AST.Blockifier ( Blockify (blockify), BlockifyAssumps )
import safe CMM.AST.Blockifier.State
    ( initBlockifier, BlockifierState )
import safe Control.Monad.State ( runState )
import safe CMM.Err.State ( ErrorState, HasErrorState (errorState), registerError, countErrors )
import safe Control.Lens ( uses, view )
import safe CMM.Data.Nullable ( Nullable(nullVal) )
import safe Prettyprinter ( Pretty(pretty), (<+>) )
import safe CMM.Inference.Preprocess ( Preprocess(preprocess) )
import safe CMM.Inference.Fact ( Facts, Fact )
import safe CMM.Inference ( mineAST, reduce )
import safe CMM.Inference.State.Impl ( initInferencer )
import safe CMM.Monomorphize ( Monomorphize(monomorphize) )
import safe CMM.Parser.GetPos ( GetPos )
import safe CMM.Monomorphize.Error
    ( MonomorphizeError(InstantiatesToNothing), voidWrapped )
import safe CMM.FillHoles ( FillHoles(fillHoles) )
import safe CMM.Mangle ( Mangle(mangle) )
import safe CMM.Options ( Options (output, prettify, monoSrc, input, quiet, handleStart), opts, noTransl )
import safe Options.Applicative ( execParser )
import safe qualified Data.Text.IO as T
import safe CMM.Err.IsError ( IsError )
import safe CMM.AST.BlockAnnot ( BlockAnnot )
import safe System.IO (stdin, IOMode (ReadMode), withFile, stderr, hPrint)
import safe System.Exit ( die )
import safe CMM.Inference.HandleCounter ( handleCounter )
import safe LLVM.AST (moduleName, Module (moduleDefinitions), defaultModule)
import LLVM.IRBuilder
    ( ModuleBuilderState(builderDefs),
      emptyModuleBuilder,
      evalModuleBuilder,
      execFreshModuleBuilderT,
      runFreshIRBuilderT )
import safe CMM.Translator (translate, TranslAnnotAssumps)
import safe LLVM.IRBuilder.Internal.SnocList ( SnocList(SnocList) )
import safe LLVM.Pretty ( ppllvm )
import safe Control.Monad ( when )
import safe CMM.Translator.State ( TranslState, initTranslState )

newtype PipelineError
  = InferenceIncomplete Facts
  deriving (Show, Eq, IsError, Data)

instance Pretty PipelineError where
  pretty (InferenceIncomplete fs) = "Type inference could not resolve the following constraints:" <+> pretty fs

-- | runs the compiler pipeline
runPipeline :: IO ()
runPipeline =
  execParser opts >>= runWithOptions

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
      when (errState /= nullVal) .
        hPrint stderr $ pretty errState -- prints all warnings and infos
      if quiet options
        then return () -- prints nothing
        else case output options of
          "-" ->  putStrLn prepared -- prints to stdout
          name -> writeFile name (prepared <> "\n") -- prints to the file `name`
    prepareOnInput input = do
      contents <- T.hGetContents input -- gets the content of the file
      case runAll fileName options contents of
        Left prettyError -> die prettyError -- prints errors and dies
        Right result -> return result

-- runs the compiler pipeline as an `Either String` monad
runAll :: String -> Options -> Text -> Either String (String, ErrorState)
runAll fileName options contents = do
  tokens <- tokenizer fileName contents
  ast <- parser fileName tokens
  if prettify options
    then return . (,nullVal) . show $ pretty ast -- we just output the prettyprinted source
    else if monoSrc options
      then do
        -- we output the unflattened monomorphized source
        (ast'', _, errState) <- runInferMono options ast
        return . (,errState)  . show . pretty $ ast''
      else do
        (ast', bState, errState) <- runFlatBlock ast
        (ast'', iState, errState') <- runInferMono options ast'
        if noTransl options
          then return . (,errState <> errState')  . show . pretty $ ast'' -- prettyprint flattened and monomorphized source
          else do
            (prettyprinted, _, errState'') <- translator options iState bState ast''
            -- prettyprint the llvm representation of the source
            return (prettyprinted ,errState <> errState' <> errState'')

-- | runs the flattener phase and the blockifier phase
runFlatBlock :: (Blockify n a (a, BlockAnnot), GetPos a, Data (n a), Data a) => n a -> Either String (n (a, BlockAnnot), BlockifierState, ErrorState)
runFlatBlock ast =
  blockifier $ flattener ast

-- | runs the inference phase and the monomorphizer phase
runInferMono :: (GetPos a, GetPos (a, TypeHole), Data a) => Options -> Annot Unit a -> Either String (Annot Unit (a, TypeHole), InferencerState, ErrorState)
runInferMono options ast = do
  ((prepAST, facts'), pState, errState) <- preprocessor options ast
  ((),iState, errState') <- inferencer options{handleStart=view handleCounter pState} prepAST facts'
  (monoAST, (iState', _)) <- monomorphizer options iState prepAST
  (ast', iState'', errState'') <- postprocessor iState' monoAST
  return (ast', iState'', errState <> errState' <> errState'')

-- | runs the tokenization
tokenizer :: String
  -> Text
  -> Either String [Annot Token SourcePos]
tokenizer source = first errorBundlePretty . runParser tokenize source

-- | runs the parsing phase
parser :: String
  -> [Annot Token SourcePos]
  -> Either String (Annot Unit SourcePos)
parser source = first errorBundlePretty . runParser program source

-- | runs the flattener
flattener :: (Data (n a), Typeable a) => n a -> n a
flattener = flatten

-- | wraps the result in an `Either String` monad, choosing `Left` if the `ErrorState` contains any errors
wrapStandardLayout :: HasErrorState s ErrorState => (a, s) -> Either String (a, s, ErrorState)
wrapStandardLayout (result, state')
  | countErrors errState /= 0 = Left . show $ pretty errState
  | otherwise = Right (result, state', errState)
  where
      errState = view errorState state'

-- | runs the blockifier
blockifier :: (Blockify n a b, BlockifyAssumps a b) => n a
  -> Either String (n b, BlockifierState, ErrorState)
blockifier flattened = wrapStandardLayout $
    blockify flattened `runState` initBlockifier

-- | runs the inference preprocessor
preprocessor :: (Preprocess n a (a, TypeHole), GetPos a) =>
  Options
  -> Annot n a
  -> Either
      String ((Annot n (a, TypeHole), [Fact]), PreprocessorState, ErrorState)
preprocessor settings ast = wrapStandardLayout $
  action `runState` initPreprocessor settings
  where
    action = do
      ast' <- preprocess ast
      uses facts $ (ast',) . reverse . head

-- | runs the inferencer
inferencer :: HasTypeHole a => Options -> Annot Unit a -> Facts
  -> Either String ((), InferencerState, ErrorState)
inferencer settings ast fs = wrapStandardLayout $
  action `runState` initInferencer settings
  where
    action = do
      mineAST ast
      fs' <- snd <$> reduce fs
      if null fs'
        then return ()
        else registerError $ InferenceIncomplete fs'

-- | runs the monomorphizer
monomorphizer :: (GetPos a, HasTypeHole a) => Options -> InferencerState -> Annot Unit a
  -> Either String (Annot Unit a, (InferencerState, MonomorphizeState a))
monomorphizer settings iState ast = case result of
      Left err -> Left . show $ pretty err
      Right Nothing -> Left . show . pretty . InstantiatesToNothing $ voidWrapped ast
      Right (Just unit') -> Right (unit', states)
  where
    (result,states) = monomorphize mempty ast `runState` (iState, initMonomorphizeState settings)

-- | runs the mangler and hole-filler
postprocessor :: (Data a, HasTypeHole a, GetPos a) => InferencerState -> Annot Unit a -> Either String (Annot Unit a, InferencerState, ErrorState)
postprocessor iState ast = wrapStandardLayout $
    action `runState` iState
  where
    action = fillHoles ast >>= mangle

-- | runs the translator
translator :: TranslAnnotAssumps a =>
  Options
  -> InferencerState
  -> BlockifierState
  -> Annot Unit a
  -> Either
      String (String, TranslState, ErrorState)
translator _ iState bState ast = do
  let irBuilder = runFreshIRBuilderT $ translate ast
      moduleBuilder = execFreshModuleBuilderT irBuilder
      (translated, tState) =
            runState moduleBuilder $ initTranslState iState bState
      module' = defaultModule { moduleName = "", moduleDefinitions = translated }
      prettyprinted = evalModuleBuilder emptyModuleBuilder{builderDefs=SnocList translated} $ ppllvm module'
  wrapStandardLayout (show $ pretty  prettyprinted, tState)

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Pipeline where

import safe Text.Megaparsec
    ( SourcePos, runParser, errorBundlePretty )
import safe CMM.Lexer ( tokenize )
import safe Data.Bifunctor ( Bifunctor(first, second) )
import safe Data.Text ( Text )
import safe CMM.AST.Annot ( Annot )
import safe CMM.Parser ( unit )
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
import safe CMM.Err.State ( ErrorState, HasErrorState (errorState), registerError )
import safe Control.Lens ( uses, view )
import safe CMM.Data.Nullable ( Nullable(nullVal) )
import safe Prettyprinter ( Pretty(pretty), (<+>) )
import safe CMM.Inference.Preprocess ( Preprocess(preprocess) )
import safe CMM.Inference.Fact ( Facts, Fact )
import safe CMM.Inference ( mineAST, reduce )
import safe CMM.Inference.State.Impl ( initInferencer )
import safe CMM.Monomorphize ( Monomorphize(monomorphize) )
import safe CMM.Parser.HasPos ( HasPos )
import safe CMM.Monomorphize.Error
    ( mapWrapped, MonomorphizeError(InstantiatesToNothing) )
import safe CMM.FillHoles ( FillHoles(fillHoles) )
import safe CMM.Mangle ( Mangle(mangle) )
import safe CMM.Options ( Options (output, prettify, monosrc, input, quiet, handleStart), opts )
import safe Options.Applicative ( execParser )
import qualified Data.Text.IO as T
import safe CMM.Err.IsError ( IsError )
import safe CMM.AST.BlockAnnot ( BlockAnnot )
import System.IO (stdin, IOMode (ReadMode), withFile)
import safe System.Exit ( die )
import CMM.Inference.HandleCounter
import CMM.Inference.BuiltIn()

newtype PipelineError
  = InferenceIncomplete Facts
  deriving (Show, Eq, IsError, Data)

instance Pretty PipelineError where
  pretty (InferenceIncomplete fs) = "Type inference could not resolve the following constraints:" <+> pretty fs

runPipeline :: IO ()
runPipeline = do
  execParser opts >>= runWithOptions

runWithOptions :: Options -> IO ()
runWithOptions options = do
  case input options of
    "-" -> runOnInput stdin
    name -> withFile name ReadMode runOnInput
  where
    fileName =
      case input options of
        "-" -> "stdin"
        name -> name
    runOnInput input = do
      prepared <- prepareOnInput input
      if quiet options
        then return ()
        else case output options of
          "-" ->  putStr prepared
          name -> writeFile name prepared
    prepareOnInput input = do
      contents <- T.hGetContents input
      case runAll fileName options contents of
        Left prettyError -> die prettyError
        Right result -> return result

runAll :: String -> Options -> Text -> Either String String
runAll fileName options contents = do
  tokens <- tokenizer fileName contents
  ast <- parser fileName tokens
  if prettify options
    then return . show $ pretty ast
    else if monosrc options
      then second (show . pretty) $ runInferMono options ast
      else do
      ast' <- runFlatBlock ast
      second (show . pretty) $ runInferMono options ast'

runFlatBlock :: (Blockify n a (a, BlockAnnot), HasPos a, Data (n a), Data a) => n a -> Either String (n (a, BlockAnnot))
runFlatBlock ast =
  fmap fst . blockifier $ flattener ast

runInferMono :: (HasPos a, HasPos (a, TypeHole), Data a) => Options -> Annot Unit a -> Either String (Annot Unit (a, TypeHole))
runInferMono options ast = do
  ((prepAST, facts), pState) <- preprocessor options ast
  ((),iState) <- inferencer options{handleStart=readHandleCounter pState} prepAST facts
  (monoAST, (iState', _)) <- monomorphizer options iState prepAST
  fst <$> postprocessor iState' monoAST

tokenizer :: String
  -> Text
  -> Either String [Annot Token SourcePos]
tokenizer source = first errorBundlePretty . runParser tokenize source

parser :: String
  -> [Annot Token SourcePos]
  -> Either String (Annot Unit SourcePos)
parser source = first errorBundlePretty . runParser unit source

flattener :: (Data (n a), Typeable a) => n a -> n a
flattener = flatten

wrapStandardLayout :: HasErrorState s ErrorState => (a, s) -> Either String (a, s)
wrapStandardLayout result@(_,state') = if errState == nullVal
  then Right result
  else Left . show $ pretty errState
  where
    errState = view errorState state'

blockifier :: (Blockify n a b, BlockifyAssumps a b) => n a
  -> Either String (n b, BlockifierState)
blockifier flattened = wrapStandardLayout $
    blockify flattened `runState` initBlockifier

preprocessor :: (Preprocess n a (a, TypeHole), HasPos a) =>
  Options
  -> Annot n a
  -> Either
      String ((Annot n (a, TypeHole), [Fact]), PreprocessorState)
preprocessor settings ast = wrapStandardLayout $
  action `runState` initPreprocessor settings
  where
    action = do
      ast' <- preprocess ast
      uses facts $ (ast',) . reverse . head

inferencer :: HasTypeHole a => Options -> Annot Unit a -> Facts
  -> Either String ((), InferencerState)
inferencer settings ast fs = wrapStandardLayout $
  action `runState` initInferencer settings
  where
    action = do
      mineAST ast
      fs' <- snd <$> reduce fs
      if null fs'
        then return ()
        else registerError $ InferenceIncomplete fs'

monomorphizer :: (HasPos a, HasTypeHole a) => Options -> InferencerState -> Annot Unit a
  -> Either String (Annot Unit a, (InferencerState, MonomorphizeState a))
monomorphizer settings iState ast = case result of
      Left err -> Left . show $ pretty err
      Right Nothing -> Left . show . pretty . InstantiatesToNothing $ mapWrapped ast
      Right (Just unit') -> Right (unit', states)
  where
    (result,states) = monomorphize mempty ast `runState` (iState, initMonomorphizeState settings)

postprocessor :: (Data a, HasTypeHole a, HasPos a) => InferencerState -> Annot Unit a -> Either String (Annot Unit a, InferencerState)
postprocessor iState ast = wrapStandardLayout $
    action `runState` iState
  where
    action = fillHoles ast >>= mangle

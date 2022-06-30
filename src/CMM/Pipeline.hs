{-# LANGUAGE Safe #-}

module CMM.Pipeline where

import safe Text.Megaparsec
    ( SourcePos, runParser, errorBundlePretty )
import safe CMM.Lexer ( tokenize )
import safe Data.Bifunctor ( Bifunctor(first) )
import safe Data.Text ( Text )
import safe CMM.AST.Annot ( Annot )
import safe CMM.Parser ( unit )
import safe CMM.AST ( Unit )
import safe CMM.Inference.Preprocess.Settings
    ( PreprocessorSettings )
import safe CMM.Inference.Preprocess.State
    ( PreprocessorState, initPreprocessor, facts )
import safe CMM.Inference.Settings ( InferencerSettings )
import safe CMM.Inference.State ( InferencerState )
import safe CMM.Monomorphize.Settings ( MonomorphizerSettings )
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
import safe CMM.Err.State ( ErrorState, HasErrorState (errorState) )
import safe Control.Lens ( uses, view )
import safe CMM.Data.Nullable ( Nullable(nullVal) )
import safe Prettyprinter ( Pretty(pretty) )
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

tokenizer :: String
  -> Text
  -> Either String [Annot Token SourcePos]

parser :: String
  -> [Annot Token SourcePos]
  -> Either String (Annot Unit SourcePos)

flattener :: (Data (n a), Typeable a) => n a -> n a

wrapStandardLayout :: HasErrorState s ErrorState => (a, s) -> Either String (a, s)

blockifier :: (Blockify n a b, BlockifyAssumps a b) => n a
  -> Either String (n b, BlockifierState)

preprocessor :: (Preprocess n a (a, TypeHole), HasPos a) =>
  PreprocessorSettings
  -> Annot n a
  -> Either
      String ((Annot n (a, TypeHole), [Fact]), PreprocessorState)

inferencer :: HasTypeHole a => InferencerSettings -> Annot Unit a -> Facts
  -> Either String (Facts, InferencerState)

monomorphizer :: (HasPos a, HasTypeHole a) => MonomorphizerSettings -> InferencerState -> Annot Unit a
  -> Either String (Annot Unit a, (InferencerState, MonomorphizeState a))

postprocessor :: (Data a, HasTypeHole a, HasPos a) => InferencerState -> Annot Unit a -> Either String (Annot Unit a, InferencerState)

tokenizer source = first errorBundlePretty . runParser tokenize source

parser source = first errorBundlePretty . runParser unit source

flattener = flatten

wrapStandardLayout result@(_,state') = if errState == nullVal
  then Right result
  else Left . show $ pretty errState
  where
    errState = view errorState state'

blockifier flattened = wrapStandardLayout $
    blockify flattened `runState` initBlockifier

preprocessor settings ast = wrapStandardLayout $
  action `runState` initPreprocessor settings
  where
    action = do
      ast' <- preprocess ast
      uses facts $ (ast',) . reverse . head

inferencer settings ast fs = wrapStandardLayout $
  action `runState` initInferencer settings
  where
    action = do
      mineAST ast
      snd <$> reduce fs

monomorphizer settings iState ast = case result of
      Left err -> Left . show $ pretty err
      Right Nothing -> Left . show . pretty . InstantiatesToNothing $ mapWrapped ast
      Right (Just unit') -> Right (unit', states)
  where
    (result,states) = monomorphize mempty ast `runState` (iState, initMonomorphizeState settings)

postprocessor iState ast = wrapStandardLayout $
    action `runState` iState
  where
    action = fillHoles ast >>= mangle

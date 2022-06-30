{-# LANGUAGE Safe #-}

module CMM.Pipeline where

import Text.Megaparsec hiding (Token)
import CMM.Lexer
import Control.Monad
import Data.Bifunctor
import Data.Text hiding (head, reverse, null)

import CMM.AST.Annot
import CMM.Parser
import CMM.AST
import CMM.Inference.Preprocess.Settings
import CMM.Inference.Preprocess.State
import CMM.Inference.Settings
import CMM.Inference.State
import CMM.Monomorphize.Settings
import CMM.Monomorphize.State
import CMM.Inference.Preprocess.TypeHole
import CMM.Lexer.Token
import Data.Data
import CMM.AST.Flattener
import CMM.AST.Blockifier
import CMM.AST.Blockifier.State
import Control.Monad.State
import CMM.AST.BlockAnnot
import CMM.Err.State
import Control.Lens
import CMM.Data.Nullable
import Prettyprinter
import CMM.Inference.Preprocess
import CMM.Inference.Fact
import CMM.Inference
import CMM.Inference.State.Impl
import CMM.Monomorphize
import CMM.Parser.HasPos
import CMM.Monomorphize.Error
import CMM.FillHoles
import CMM.Mangle

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

preprocessor :: PreprocessorSettings -> Annot Unit SourcePos
  -> Either String ((Annot Unit (SourcePos, TypeHole), Facts), PreprocessorState)

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

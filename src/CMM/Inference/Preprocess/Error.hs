{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveAnyClass #-}

module CMM.Inference.Preprocess.Error where

import safe Data.Text (Text)
import safe Data.Data ( Data )

import safe Prettyprinter ( Pretty(pretty), (<+>), dquotes, dot )

import safe CMM.Err.IsError (IsError)
import safe CMM.Inference.Preprocess.Elaboration ( Elaboration )
import CMM.AST.Wrap (ASTWrapper)

-- | The errors used by the `Preprocessor`
data PreprocessError
  = UndefinedForeign Text
  | IllegalElaboration Elaboration
  | LVNotFound Text
  | NotImplemented (ASTWrapper ())
  deriving (Eq, Show, IsError, Data)

instance Pretty PreprocessError where
  pretty = \case
    UndefinedForeign text -> "Foreign" <+> dquotes (pretty text) <+> "is not recognized by the language, the only foreign recognized currently is" <+> dquotes "C" <> dot
    IllegalElaboration hole -> "Illegal elaboration" <+> pretty hole <+>"encountered, report bug in inference preprocessing"
    NotImplemented node -> "The following feature has not yet been implemented:" <+> pretty node
    LVNotFound name -> "Variable" <+> pretty name <+> "cannot be found"

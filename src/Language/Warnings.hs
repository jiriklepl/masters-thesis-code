{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Warnings where

import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

mkWarning :: SourcePos -> Text -> Text
mkWarning sourcePos message = T.pack (sourcePosPretty sourcePos) <> " warning:\n\t" <> message

mkError :: SourcePos -> Text -> Text
mkError sourcePos message = T.pack (sourcePosPretty sourcePos) <> " error:\n\t" <> message


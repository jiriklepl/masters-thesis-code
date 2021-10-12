{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Warnings where

import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

-- | Creates a warning text from a `SourcePos` object, which gets printed out as the header for the warning, and from the message itself
mkWarning :: SourcePos -> Text -> Text
mkWarning sourcePos message = T.pack (sourcePosPretty sourcePos) <> " warning:\n\t" <> message

-- | Creates an error text from a `SourcePos` object, which gets printed out as the header for the error, and from the message itself
mkError :: SourcePos -> Text -> Text
mkError sourcePos message = T.pack (sourcePosPretty sourcePos) <> " error:\n\t" <> message


{-# LANGUAGE Safe #-}

module CMM.Warnings where

import safe Control.Monad.IO.Class (MonadIO(..))
import safe Data.Text (Text)
import safe qualified Data.Text as T
import safe qualified Data.Text.IO as T
import safe Prettyprinter (Pretty(pretty))
import safe Text.Megaparsec.Pos (sourcePosPretty)

import safe CMM.AST.Utils ()
import safe CMM.Parser.HasPos (HasPos(..), SourcePos)

-- | Creates a warning text from a `SourcePos` object, which gets printed out as the header for the warning, and from the message itself
mkWarning :: SourcePos -> Text -> Text
mkWarning sourcePos message =
  T.pack (sourcePosPretty sourcePos) <> " warning:\n\t" <> message

-- | Creates an error text from a `SourcePos` object, which gets printed out as the header for the error, and from the message itself
mkError :: SourcePos -> Text -> Text
mkError sourcePos message =
  T.pack (sourcePosPretty sourcePos) <> " error:\n\t" <> message

makeMessage ::
     (HasPos n, Pretty n, MonadIO m)
  => (SourcePos -> Text -> Text)
  -> n
  -> Text
  -> m ()
makeMessage constructor node message = do
  let pos = getPos node
  liftIO . T.putStrLn $
    constructor pos $ (T.pack . show $ pretty node) <> "\n\t" <> message

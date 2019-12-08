{-# LANGUAGE NoImplicitPrelude #-}
module Common where
import           Protolude
import           Text.Megaparsec

type Parser = Parsec Void Text

executeParser :: Parser a -> Text -> IO a
executeParser parser input = case parse (parser <* eof) "" input of
  Left  boo -> putStr (errorBundlePretty boo) >> exitFailure
  Right joy -> pure joy

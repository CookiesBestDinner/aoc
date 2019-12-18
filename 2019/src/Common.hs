{-# LANGUAGE NoImplicitPrelude #-}
module Common
  ( Parser
  , executeParser
  )
where
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void Text

executeParser :: Parser a -> Text -> IO a
executeParser parser input = case parse (parser <* space <* eof) "" input of
  Left  boo -> putStr (errorBundlePretty boo) >> exitFailure
  Right joy -> pure joy

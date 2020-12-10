{-# LANGUAGE FlexibleContexts #-}
module Parsing where

import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void Text

number :: Integral i => Parser i
number = signed (pure ()) decimal

numbers :: Integral i => Parser [i]
numbers = number `sepEndBy1` space1

restOfLine :: ConvertText Text s => Parser s
restOfLine = toS <$> takeWhileP Nothing (/= '\n') <* ("\n" $> () <|> eof)

parse' :: Parser a -> Text -> IO a
parse' parser text = case parse parser "" text of
  Right yay -> pure yay
  Left  boo -> putStr (errorBundlePretty boo) >> exitFailure

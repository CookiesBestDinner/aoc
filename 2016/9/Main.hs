{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Protolude               hiding ( many )

import           Text.Megaparsec
import qualified Data.Text                     as T
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

pSomething :: Parser Integer
pSomething = decimal

pThings :: Parser [Text]
pThings = many (pSection <|> (T.singleton <$> anySingle)) <* eof

pSection :: Parser Text
pSection = do
  "("
  size <- decimal
  "x"
  repeats <- decimal
  ")"
  repeatThis <- count size anySingle
  pure $ T.concat $ replicate repeats (T.pack repeatThis)

main = do
  input <- T.filter (/= '\n') <$> getContents
  things <- case parse pThings "" input of
              Right stuff -> pure stuff
              (Left bundle) -> do
                putStr (errorBundlePretty bundle)
                exitFailure
  let file = T.concat things
  print things
  print $ T.length file

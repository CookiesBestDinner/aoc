{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day9 where

import           Protolude                         hiding ( many )
import           Prelude                                  ( error )

import           Text.Megaparsec
import qualified Data.Text                     as T
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

pThings :: Parser [Text]
pThings = many (pSection <|> (T.singleton <$> anySingle)) <* eof

pThingsCount :: Parser Integer
pThingsCount = sum <$> many (pSectionCount <|> (1 <$ anySingle)) <* eof

pSectionCount :: Parser Integer
pSectionCount = do
  "("
  size <- decimal
  "x"
  repeats <- decimal
  ")"
  repeatThis <- count size anySingle
  let lenOfThat = case parse pThingsCount "" (T.pack repeatThis) of
        Right stuff   -> stuff
        (Left bundle) -> error (errorBundlePretty bundle)
  -- pure $ T.concat $ repeats (T.pack repeatThis)
  pure $ repeats * lenOfThat

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
  input  <- T.filter (/= '\n') <$> getContents
  things <- case parse pThings "" input of
    Right stuff   -> pure stuff
    (Left bundle) -> do
      putStr (errorBundlePretty bundle)
      exitFailure
  let file = T.concat things
  print things
  print $ T.length file
  count <- case parse pThingsCount "" input of
    Right stuff   -> pure stuff
    (Left bundle) -> do
      putStr (errorBundlePretty bundle)
      exitFailure
  print count

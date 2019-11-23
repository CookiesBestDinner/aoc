{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day09.Main where

import           Protolude                  hiding (many)
import           Text.Megaparsec

data Result = Result {groupScore :: Int, garbageCount :: Int} deriving (Eq, Show)

instance Semigroup Result where
  (Result gs1 gc1) <> (Result gs2 gc2) = Result (gs1 + gs2) (gc1 + gc2)
instance Monoid Result where
  mempty = Result 0 0


-- garbage <garbageContent>
-- garbageContent ! skips next char
-- group {thing,thing,...}
-- thing group <|> garbage

type Parser = Parsec Void Text

pThing :: Int -> Parser Result
pThing depth = pGroup depth <|> pGarbage

pGroup :: Int -> Parser Result
pGroup depth = do
  "{"
  subs <- (pThing (depth + 1)) `sepBy` ","
  "}"
  return $ Result {groupScore = depth + 1, garbageCount = 0} <> mconcat subs

pGarbage :: Parser Result
pGarbage = do
  "<"
  gc <- pGarbageContent
  ">"
  return gc

pGarbageContent :: Parser Result
pGarbageContent =
  do
    let skip = "!" >> anySingle
        nonTerm = anySingleBut '>'
    charCount <- many $ (skip $> 0) <|> (nonTerm $> 1)
    return $ Result {garbageCount = sum charCount, groupScore = 0}

main :: IO ()
main = do
  input <- getContents
  case solve input of
    Right solution -> do
      print solution
    Left boo -> do
        putStrLn $ errorBundlePretty boo
        exitFailure

solve :: Text -> Either (ParseErrorBundle Text Void) Result
solve input = do
  score <- parse (pThing 0) "" input
  return score

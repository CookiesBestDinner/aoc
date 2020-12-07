module Day07 where

import qualified Data.Map.Strict            as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

type Bag = [Char]

bagParser :: Parser Bag
bagParser = do
  let word = toS <$> takeWhile1P Nothing (not . isSpace)
  a <- word
  " "
  b <- word
  " bags" <|> " bag"
  pure (a <> " " <> b)

contaieeParser :: Parser (Int, Bag)
contaieeParser = do
  n <- decimal
  " "
  bag <- bagParser
  pure (n, bag)

nodeParser :: Parser (Bag, [(Int, Bag)])
nodeParser = do
  a <- bagParser
  " contain "
  cs <- contaieeParser `sepBy1` ", " <|> "no other bags" $> []
  "."
  pure (a, cs)

reverseEdges :: [(Bag, [Bag])] -> Map Bag [Bag]
reverseEdges bags = foldl' insert mempty insertionPairs
 where
  insert acc (k, v) = Map.insertWith (<>) k [v] acc
  insertionPairs = [ (val, key) | (key, vals) <- bags, val <- vals ]

canCarry :: Map Bag [Bag] -> Bag -> [Bag]
canCarry bags bag = bag : (canCarry bags =<< more)
  where more = Map.findWithDefault [] bag bags

mustCarry :: Map Bag [(Int, Bag)] -> Bag -> [Bag]
mustCarry bags bag = bag : need
 where
  more = Map.findWithDefault [] bag bags
  need = mconcat $ mconcat [ replicate n (mustCarry bags b) | (n, b) <- more ]

main :: Text -> IO ()
main input = do
  bags <- parse' (nodeParser `sepEndBy1` eol <* eof) input
  let rev = reverseEdges $ second (map snd) <$> bags
  "shiny gold" & canCarry rev & ordNub & length & subtract 1 & print
  "shiny gold" & mustCarry (Map.fromList bags) & length & subtract 1 & print

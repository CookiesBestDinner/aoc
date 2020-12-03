module Day03 where

import           Data.List       (iterate')
import           Protolude       hiding (some)
import           Text.Megaparsec

import           Parsing

data Tile = Empty | Tree deriving (Eq, Show)

mapParser :: Parser [[Tile]]
mapParser = some ("." $> Empty <|> "#" $> Tree) `sepEndBy1` "\n" <* eof

treeCount :: [[Tile]] -> (Int, Int) -> Int
treeCount grid (dx, dy) = (countTrees . whileInTheWoods) (getTile <$> steps)
 where
  width = maybe 0 length (head grid)
  step (x, y) = (x + dx, y + dy)
  steps = iterate' step (0, 0)
  getTile (x, y) = (`atMay` y) >=> (`atMay` (x `mod` width)) $ grid
  countTrees      = length . filter (== Tree)
  whileInTheWoods = catMaybes . takeWhile isJust

main :: Text -> IO ()
main input = do
  grid <- parse' mapParser input
  print $ treeCount grid (3, 1)
  print $ product $ treeCount grid <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

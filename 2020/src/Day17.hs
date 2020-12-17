module Day17 where

import           Data.List (iterate', (!!))
import qualified Data.Set  as Set
import           Protolude

type Loc = [Int]
type Space = Set Loc

convert :: Int -> Text -> Space
convert dims txt = Set.fromList $ do
  (y, row) <- zip [0 ..] (lines txt)
  (x, '#') <- zip [0 ..] (toS row)
  pure $ take dims ([x, y] <> repeat 0)

around :: Loc -> [Loc]
around = traverse (\c -> [c - 1, c, c + 1])

neighbourhood :: Space -> Loc -> Int
neighbourhood space = length . filter (`Set.member` space) . around

willBeAlive :: Space -> Loc -> Bool
willBeAlive space here = neighbourhood space here & condition
 where
  condition | here `Set.member` space = (`elem` [3, 4])
            | otherwise               = (== 3)

step :: Space -> Space
step space = Set.filter (willBeAlive space) interesting
  where interesting = Set.fromList (Set.toList space >>= around)

main :: Text -> IO ()
main input = do
  let solve n = input & convert n & iterate' step & (!! 6) & Set.size & print
  solve 3
  solve 4

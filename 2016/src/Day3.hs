module Day3 where

import Data.List
import Data.List.Extra
import Data.Function

parseTriangle :: String -> (Integer, Integer, Integer)
parseTriangle s = (a, b, c)
  where
    tokens = words s
    [a, b, c] = read <$> tokens

isTriangle (a, b, c) = sum shortest > longest
  where
    sides = [a, b, c]
    longest = maximum sides
    shortest = sides \\ [longest]

main = do
  input <- getContents
  let triangles1 = map parseTriangle . lines $ input
  print $ triangles1 & filter isTriangle & length
  -- part 2
  let rows :: [[Integer]]
      rows = map read . words <$> lines input
      columnWise = transpose rows & concat
      groups2 = chunksOf 3 columnWise
      triangles2 = map (\[a, b, c] -> (a, b, c)) groups2
  print $ triangles2 & filter isTriangle & length

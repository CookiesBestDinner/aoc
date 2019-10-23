module Main where

import           Control.Monad
import           Data.Function

main = do
  let target = 150
  containers <- map read . lines <$> getContents
  let candidates = fill target containers
  print $ length candidates
  -- part 2
  let leastAmountUsed = candidates & map sum & minimum
      countByThatAmount =
        candidates & map sum & filter (== leastAmountUsed) & length
  print countByThatAmount

fill :: Int -> [Int] -> [[Int]]
fill 0 []        = [[0]]
fill _ []        = []
fill n (j : ars) = do
  (used, remaining) <- [(0, n), (1, n - j)]
  guard $ remaining >= 0
  continue <- fill remaining ars
  return (used : continue)

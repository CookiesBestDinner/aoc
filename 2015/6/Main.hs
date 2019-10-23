module Main where

import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Function
import           Data.List                                ( foldl' )
import           Prelude                           hiding ( either )
import           Data.Char

main :: IO ()
main = do
  let replaceComma = map (\ch -> if ch == ',' then ' ' else ch)
  updates <- map words . lines . replaceComma <$> getContents
  let res = foldl' applyUpdate Set.empty updates
  print $ Set.size res
  let res2 = foldl' applyUpdate2 Map.empty updates
  print $ sum $ Map.elems res2

applyUpdate :: Set.Set (Int, Int) -> [String] -> Set.Set (Int, Int)
applyUpdate acc update = case update of
  ("toggle"       : _) -> either `Set.difference` both
  ("turn" : "on"  : _) -> either
  ("turn" : "off" : _) -> acc `Set.difference` changeCoords
 where
  changeCoords = Set.fromList $ rectangleCoords update
  both         = changeCoords `Set.intersection` acc
  either       = changeCoords `Set.union` acc

applyUpdate2 :: Map.Map (Int, Int) Int -> [String] -> Map.Map (Int, Int) Int
applyUpdate2 acc update = Map.union updated acc
 where
  updated = Map.fromList $ do
    updateCoord <- rectangleCoords update
    let oldVal = Map.findWithDefault 0 updateCoord acc
        newVal = max 0 (oldVal + diff)
    return (updateCoord, newVal)
  diff = case update of
    ("toggle"       : _) -> 2
    ("turn" : "on"  : _) -> 1
    ("turn" : "off" : _) -> -1

rectangleCoords :: [String] -> [(Int, Int)]
rectangleCoords update = do
  x <- [minimum xs .. maximum xs]
  y <- [minimum ys .. maximum ys]
  return (x, y)
 where
  nums = read <$> filter (all isNumber) update
  xs   = [nums !! 0, nums !! 2]
  ys   = [nums !! 1, nums !! 3]

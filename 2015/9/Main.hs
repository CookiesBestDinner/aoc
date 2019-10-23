module Main where

import           Control.Arrow
import qualified Data.Map                      as Map
import           Data.List                                ( foldl'
                                                          , permutations
                                                          )

parseEdge :: String -> (String, String, Integer)
parseEdge ln =
  let tokens = words ln
      a      = tokens !! 0
      b      = tokens !! 2
      dist   = read $ tokens !! 4
  in  (a, b, dist)

tourDistance roadMap (a : b : rest) =
  let dist = (roadMap Map.! a) Map.! b
  in  dist + tourDistance roadMap (b : rest)
tourDistance _ _ = 0

main = do
  edges <- (lines >>> map parseEdge) <$> getContents
  let edgesTwoWays = edges >>= (\(a, b, dist) -> [(a, b, dist), (b, a, dist)])
      insertEdge acc (from, to, dist) =
        let oldVal = Map.findWithDefault [] from acc
        in  Map.insert from ((to, dist) : oldVal) acc
      roadMap   = Map.fromList <$> foldl' insertEdge Map.empty edgesTwoWays
      tours     = permutations (Map.keys roadMap)
      distances = map (tourDistance roadMap) tours
  -- Part 1
  print $ minimum distances
  -- Part 2 ... well, that was free.
  print $ maximum distances

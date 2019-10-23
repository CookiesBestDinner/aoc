module Main where

import           Data.List                                ( transpose
                                                          , scanl'
                                                          )
import           Control.Arrow
import           Data.Function

parseReindeer :: String -> (String, [Int])
parseReindeer ln =
  (name, cycle $ replicate travelTime speed ++ replicate restTime 0)
 where
  tokens     = words ln
  name       = tokens !! 0
  speed      = read $ tokens !! 3
  travelTime = read $ tokens !! 6
  restTime   = read $ tokens !! 13

scoreSlice :: [Int] -> [Int]
scoreSlice dists = [ if dist == winning then 1 else 0 | dist <- dists ]
  where winning = maximum dists

main = do
  let raceDuration = 2503
  reindeer <- map parseReindeer . lines <$> getContents
  let results = do
        (name, travel) <- reindeer
        let distance = sum $ take raceDuration travel
        [(distance, name)]

  print $ maximum results
  let
    racePerformances = transpose
      (map (snd >>> scanl' (+) 0 >>> drop 1 >>> take raceDuration) reindeer)
    pointsBySlice = map scoreSlice racePerformances
    pointsByDeer  = transpose pointsBySlice & map (take raceDuration) & map sum
  print $ pointsByDeer
  print $ maximum pointsByDeer

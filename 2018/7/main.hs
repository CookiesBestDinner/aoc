{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO

workerCount :: Int
baseTimePerJob :: Integer
workerCount = 5
baseTimePerJob = 60

main :: IO ()
main = do
  input <- TIO.getContents
  let
    dependPairs = case parseOnly pDependenies input of
      Right pairs -> pairs
      Left  e     -> error e
    dependMap = Map.fromListWith (++) [ (k, [v]) | (v, k) <- dependPairs ]
    allSteps =
      Set.toList $ Set.fromList $ concat [ [s1, s2] | (s1, s2) <- dependPairs ]
    order  = thingymajig allSteps dependMap
    frames = workFrames allSteps dependMap []
  print order
  print frames
  print $ length frames

workFrames
  :: String -> Map.Map Char String -> [(Char, Integer)] -> [[(Char, Integer)]]
workFrames [] _ [] = []
workFrames steps depMap wip =
  currentWip : workFrames remainingSteps newDepMap remainingWip
 where
  -- fill up workers for this timeframe
  availableWorkers = workerCount - length wip
  availableJobs =
    [ (step, costPerStep step) | step <- steps, not $ Map.member step depMap ]
  newJobs        = Prelude.take availableWorkers availableJobs
  currentWip     = wip ++ newJobs
  remainingSteps = steps \\ (map fst newJobs)
  -- clean up after what finshes at the end of this timeframe
  remainingWip   = [ (step, time - 1) | (step, time) <- currentWip, time > 1 ]
  completedSteps = [ step | (step, time) <- currentWip, time == 1 ]
  newDepMap'     = Map.map (\\ completedSteps) depMap
  newDepMap      = Map.filter (not . null) newDepMap'

costPerStep :: Char -> Integer
costPerStep ch = baseTimePerJob + (fromIntegral $ ord ch) - 64

thingymajig :: String -> Map.Map Char String -> String
thingymajig []    _      = []
thingymajig steps depMap = firstNodepend : thingymajig newSteps filteredEmpty
 where
  firstNodepend   = head [ step | step <- steps, not $ Map.member step depMap ]
  removedThisStep = Map.map (delete firstNodepend) depMap
  filteredEmpty   = Map.filter (not . null) removedThisStep
  newSteps        = delete firstNodepend steps

pDependenies :: Parser [(Char, Char)]
pDependenies =
  (`sepBy` endOfLine)
    $   (,)
    <$  string "Step "
    <*> satisfy (`elem` ['A' .. 'Z'])
    <*  string " must be finished before step "
    <*> satisfy (`elem` ['A' .. 'Z'])
    <*  string " can begin."

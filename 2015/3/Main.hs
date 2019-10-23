module Main where

import qualified Data.Set                      as Set
import           Data.Function
import Data.List.Extra (chunksOf, foldl')

data State = State { location :: (Integer, Integer), visited :: Set.Set (Integer, Integer)}

initState = State (0, 0) (Set.fromList [(0, 0)])

main = do
  directions <- filter (`elem` "<>^v") <$> getContents
  let endState = foldl' choochoo initState directions
  putStrLn "How many unique locations does santa visit given some instructions?"
  putStrLn "Part 1: Santa follows all the directions"
  print $ Set.size (visited endState)
  let roboInstructions = directions & chunksOf 2 & concatMap (take 1)
      santaInstructions = directions & drop 1 & chunksOf 2 & concatMap (take 1)
      roboLocs = foldl' choochoo initState roboInstructions
      santaLocs = foldl' choochoo initState santaInstructions
      combinedLocs = visited roboLocs `Set.union` visited santaLocs
  putStrLn "Part 2: Santa and RoboSanta take turns following the instructions"
  print $ Set.size combinedLocs

choochoo state direction = State newLoc (Set.insert newLoc (visited state))
 where
  (x, y) = location state
  newLoc = case direction of
    '>' -> (x + 1, y)
    '<' -> (x - 1, y)
    '^' -> (x, y + 1)
    'v' -> (x, y - 1)
    other -> error $ "Bad direction: " ++ show other

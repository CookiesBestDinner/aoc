module Main where

import           Data.List                                ( foldl'
                                                          , scanl'
                                                          )
import qualified Data.Set                      as Set

data Turn
  = TLeft
  | TRight
  deriving (Show)

data Instruction =
  Instruction Turn Int
  deriving (Show)

data Orientation
  = North
  | South
  | East
  | West
  deriving (Show)

data State =
  State
    { pos :: (Int, Int)
    , ori :: Orientation
    }
  deriving (Show)

applyDirection :: State -> Instruction -> State
applyDirection here (Instruction turn dist) = State { pos = newPos
                                                    , ori = newOri
                                                    }
 where
  newOri = getNewOrientation turn (ori here)
  newPos = getNewPos newOri dist (pos here)

getNewPos North dist (x, y) = (x, y + dist)
getNewPos South dist (x, y) = (x, y - dist)
getNewPos East  dist (x, y) = (x + dist, y)
getNewPos West  dist (x, y) = (x - dist, y)

getNewOrientation TLeft  North = West
getNewOrientation TLeft  West  = South
getNewOrientation TLeft  South = East
getNewOrientation TLeft  East  = North
getNewOrientation TRight North = East
getNewOrientation TRight East  = South
getNewOrientation TRight South = West
getNewOrientation TRight West  = North

lassoDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parse :: String -> [Instruction]
parse input = map pDirection tokens
 where
  noCommas = filter (/= ',') input
  tokens   = words noCommas
  pDirection ('L' : num) = Instruction TLeft $ read num
  pDirection ('R' : num) = Instruction TRight $ read num
  pDirection _           = error "uhm, what?"

fillLocations (a : b : rest) =
  [a] ++ locationsBetween a b ++ fillLocations (b : rest)
fillLocations [a] = [a]
fillLocations []  = []

locationsBetween (x1, y1) (x2, y2) = tail . init $ do
  x <- max [x1 .. x2] (reverse [x2 .. x1])
  y <- max [y1 .. y2] (reverse [y2 .. y1])
  [(x, y)]

firstDuplicate :: (Eq a, Ord a) => [a] -> a
firstDuplicate = go Set.empty
 where
  go seen (x : xs) =
    if x `Set.member` seen then x else go (Set.insert x seen) xs
  go _ [] = error "no duplicate"

main = do
  directions <- parse <$> getContents
  let begin = State (0, 0) North
      hq    = foldl' applyDirection begin directions
  print $ lassoDistance (pos begin) (pos hq)
  -- part 2
  let tour      = scanl' applyDirection begin directions
      positions = map pos tour
  print $ lassoDistance (0, 0) $ firstDuplicate $ fillLocations positions

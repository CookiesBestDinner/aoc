module Main where

import qualified Data.Map.Strict               as Map
import           Control.Arrow
import           Data.List                                ( iterate' )
import           Data.Function
import           Control.Monad
import           Data.Maybe

fromRows :: [String] -> Map.Map (Int, Int) Char
fromRows rows = Map.fromList $ do
  (y, row ) <- zip [0 ..] rows
  (x, cell) <- zip [0 ..] row
  [((x, y), cell)]

toRows :: Map.Map (Int, Int) Char -> [String]
toRows lights = do
  let maxX = Map.keys lights & map fst & maximum
      maxY = Map.keys lights & map snd & maximum
  y <- [0 .. maxY]
  let row = do
        x <- [0 .. maxX]
        [lights Map.! (x, y)]
  [row]

step :: Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
step lights = Map.fromList $ do
  (x, y) <- Map.keys lights
  let neighbours = filter (== '#') >>> length $ do
        x' <- [x - 1, x, x + 1]
        y' <- [y - 1, y, y + 1]
        guard $ (x', y') /= (x, y)
        maybeToList $ Map.lookup (x', y') lights
      lit = Map.lookup (x, y) lights == Just '#'
      nextState | lit && (neighbours `elem` [2, 3]) = '#'
                | not lit && neighbours == 3        = '#'
                | otherwise                         = '.'
  [((x, y), nextState)]

countLights :: Map.Map (Int, Int) Char -> Int
countLights lights = Map.elems lights & filter (== '#') & length

main = do
  initState <- fromRows . lines <$> getContents
  let hundredth = iterate' step initState !! 100
  print $ countLights hundredth
  -- part 2
  let
    maxY       = Map.keys initState & map snd & maximum
    maxX       = Map.keys initState & map fst & maximum
    litCorners = Map.fromList
      [((0, 0), '#'), ((maxX, 0), '#'), ((0, maxY), '#'), ((maxX, maxY), '#')]
    setCorners = Map.union litCorners
    initial    = setCorners initState
    newStep    = step >>> setCorners
    hundredth' = iterate' newStep initial !! 100
  print $ countLights hundredth'

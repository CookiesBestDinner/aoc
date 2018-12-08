{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let maze = case parseOnly pInput input of
                 Right x -> x
                 Left e -> error e
      part1 = stepsToEscape maze 0
  print maze
  print part1

stepsToEscape :: Map.Map Int Int -> Int -> Int
stepsToEscape maze loc
  | loc < 0 || loc >= Map.size maze = 0
  | otherwise = 1 + stepsToEscape newMaze newLoc
  where
    offset = maze Map.! loc
    increment = if offset >= 3
                   then -1
                   else 1
    newMaze = Map.adjust (+increment) loc maze
    newLoc = loc + maze Map.! loc


-- Map location value
pInput :: Parser (Map.Map Int Int)
pInput = do
  numbers <- (signed decimal `sepBy` endOfLine)
  return $ Map.fromList $ zip [0 ..] numbers

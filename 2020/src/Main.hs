module Main where

import           Criterion
import           Protolude
import           System.Environment
import           System.IO.Silently

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19

main :: IO ()
main = do
  args <- getArgs
  let execArgs = takeWhile (/= "--") args & dropWhile (== "bench")
      dayArgs  = drop 1 $ dropWhile (/= "--") args
  let input = case execArgs `atMay` 1 of
        Nothing       -> readFile $ mconcat $ "input/" : take 1 execArgs
        Just "-"      -> getContents
        Just filename -> readFile filename
  let run a | take 1 args == ["bench"] = benchmark (nfIO (silence a))
            | otherwise                = a
  run $ withArgs dayArgs $ case take 1 execArgs of
    ["01"] -> Day01.main =<< input
    ["02"] -> Day02.main =<< input
    ["03"] -> Day03.main =<< input
    ["04"] -> Day04.main =<< input
    ["05"] -> Day05.main =<< input
    ["06"] -> Day06.main =<< input
    ["07"] -> Day07.main =<< input
    ["08"] -> Day08.main =<< input
    ["09"] -> Day09.main =<< input
    ["10"] -> Day10.main =<< input
    ["11"] -> Day11.main =<< input
    ["12"] -> Day12.main =<< input
    ["13"] -> Day13.main =<< input
    ["14"] -> Day14.main =<< input
    ["15"] -> Day15.main =<< input
    ["16"] -> Day16.main =<< input
    ["17"] -> Day17.main =<< input
    ["18"] -> Day18.main =<< input
    ["19"] -> Day19.main =<< input
    _ -> putText "day not recognized." >> exitFailure

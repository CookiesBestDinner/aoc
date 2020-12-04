module Main where

import           Criterion
import           Protolude
import           System.Environment

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04

main :: IO ()
main = do
  args <- getArgs
  let execArgs = takeWhile (/= "--") args & dropWhile (== "bench")
      dayArgs  = drop 1 $ dropWhile (/= "--") args
  let input = case execArgs `atMay` 1 of
        Nothing       -> readFile $ mconcat $ "input/" : take 1 execArgs
        Just "-"      -> getContents
        Just filename -> readFile filename
  let run a | take 1 args == ["bench"] = benchmark (nfIO a)
            | otherwise                       = a
  run $ withArgs dayArgs $ case take 1 execArgs of
    ["01"] -> Day01.main =<< input
    ["02"] -> Day02.main =<< input
    ["03"] -> Day03.main =<< input
    ["04"] -> Day04.main =<< input

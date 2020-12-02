module Main where

import           Protolude
import           System.Environment

import qualified Day01
import qualified Day02

main :: IO ()
main = do
  args <- getArgs
  let execArgs = takeWhile (/= "--") args
      dayArgs  = drop 1 $ dropWhile (/= "--") args
  let input = case execArgs `atMay` 1 of
        Nothing       -> readFile $ mconcat $ "input/" : take 1 execArgs
        Just "-"      -> getContents
        Just filename -> readFile filename
  withArgs dayArgs $ case take 1 execArgs of
    ["01"] -> Day01.main =<< input
    ["02"] -> Day02.main =<< input

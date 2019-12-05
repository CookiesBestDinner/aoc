{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion
import           Protolude

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

-- import qualified Day06
-- import qualified Day07
-- import qualified Day08
-- import qualified Day09
-- import qualified Day10
-- import qualified Day11
-- import qualified Day12
-- import qualified Day13
-- import qualified Day14
-- import qualified Day15
-- import qualified Day16
-- import qualified Day17
-- import qualified Day18
-- import qualified Day19
-- import qualified Day20
-- import qualified Day21
-- import qualified Day22
-- import qualified Day23
-- import qualified Day24
-- import qualified Day25

main :: IO ()
main = do
  args   <- getArgs
  !input <- case args `atMay` 1 of
    Nothing       -> getContents
    Just filename -> readFile filename
  let benchMe = args `atMay` (length args - 1) == Just "bench"
  let run a | benchMe   = benchmark (nfIO a)
            | otherwise = a
  let action = case take 1 args of
        ["1"] -> Day01.main input
        ["2"] -> Day02.main input
        ["3"] -> Day03.main input
        ["4"] -> Day04.main input
        ["5"] -> Day05.main input
        -- ["6"] -> Day06.main input
        -- ["7"] -> Day07.main input
        -- ["8"] -> Day08.main input
        -- ["9"] -> Day09.main input
        -- ["10"] -> Day10.main input
        -- ["11"] -> Day11.main input
        -- ["12"] -> Day12.main input
        -- ["13"] -> Day13.main input
        -- ["14"] -> Day14.main input
        -- ["15"] -> Day15.main input
        -- ["16"] -> Day16.main input
        -- ["17"] -> Day17.main input
        -- ["18"] -> Day18.main input
        -- ["19"] -> Day19.main input
        -- ["20"] -> Day20.main input
        -- ["21"] -> Day21.main input
        -- ["22"] -> Day22.main input
        -- ["23"] -> Day23.main input
        -- ["24"] -> Day24.main input
        -- ["25"] -> Day25.main input
        _     -> putText "I'm not aware of that day." >> exitFailure
  run action

mainmain :: IO ()
mainmain = main

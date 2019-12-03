{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude   (error)
import           Protolude

import qualified Day01
import qualified Day02
import qualified Day03
-- import qualified Day04
-- import qualified Day05
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
  args <- getArgs
  case args of
    ["1"] -> Day01.main
    ["2"] -> Day02.main
    ["3"] -> Day03.main
    -- ["4"] -> Day04.main
    -- ["5"] -> Day05.main
    -- ["6"] -> Day06.main
    -- ["7"] -> Day07.main
    -- ["8"] -> Day08.main
    -- ["9"] -> Day09.main
    -- ["10"] -> Day10.main
    -- ["11"] -> Day11.main
    -- ["12"] -> Day12.main
    -- ["13"] -> Day13.main
    -- ["14"] -> Day14.main
    -- ["15"] -> Day15.main
    -- ["16"] -> Day16.main
    -- ["17"] -> Day17.main
    -- ["18"] -> Day18.main
    -- ["19"] -> Day19.main
    -- ["20"] -> Day20.main
    -- ["21"] -> Day21.main
    -- ["22"] -> Day22.main
    -- ["23"] -> Day23.main
    -- ["24"] -> Day24.main
    -- ["25"] -> Day25.main
    _     -> error "I'm not aware of that day."

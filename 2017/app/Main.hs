{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- so err making many executables seems slow .. so dispatch from a single main
-- instead to get multiple entrypoints
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day22
import           Prelude                                  ( error )
import           Protolude
import           System.Environment                       ( withArgs )

main :: IO ()
main = do
  putText "uhm. hi?"
  args <- getArgs
  withArgs (drop 1 args) $ case take 1 args of
    ["12"] -> Day12.main
    ["13"] -> Day13.main
    ["14"] -> Day14.main
    ["15"] -> Day15.main
    ["16"] -> Day16.main
    ["17"] -> Day17.main
    ["18"] -> Day18.main
    ["22"] -> Day22.main
    _      -> error "I'm not aware of that day."

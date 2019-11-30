{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- so err making many executables seems slow .. so dispatch from a single main
-- instead to get multiple entrypoints
import qualified Day12
import qualified Day13
import qualified Day14
import           Prelude   (error)
import           Protolude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["12"] -> Day12.main
    ["13"] -> Day13.main
    ["14"] -> Day14.main
    _      -> error "I'm not aware of that day."

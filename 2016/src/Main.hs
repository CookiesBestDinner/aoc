{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Day13
import qualified Day16
import qualified Day19
import           Protolude

main :: IO ()
main = getArgs >>= \case
  ["19"] -> Day19.main
  ["13"] -> Day13.main
  ["16"] -> Day16.main

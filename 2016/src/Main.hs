{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Day13
import qualified Day19
import           Protolude

main :: IO ()
main = getArgs >>= \case
  ["19"] -> Day19.main
  ["13"] -> Day13.main

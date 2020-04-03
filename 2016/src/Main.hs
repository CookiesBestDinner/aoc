{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Day19
import           Protolude

main :: IO ()
main = getArgs >>= \case
  ["19"] -> Day19.main

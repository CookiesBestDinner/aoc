{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Day12


import Protolude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["12"] ->  Day12.main
    _ -> undefined

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day08
  ( main
  )
where

import           Common

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Protolude
import           Data.List.Extra                          ( chunksOf
                                                          , foldl1'
                                                          )

pInput :: Parser [Int]
pInput =
  (Text.Megaparsec.many $ choice ["0" $> 0, "1" $> 1, "2" $> 2]) <* space

-- |
-- >>> readFile "input/day08" >>= main
-- 1452
-- ███  █  █ ███  ████ █  █
-- █  █ █  █ █  █ █    █  █
-- █  █ ████ █  █ ███  █  █
-- ███  █  █ ███  █    █  █
-- █    █  █ █    █    █  █
-- █    █  █ █    ████  ██
main :: Text -> IO ()
main indata = do
  let (width, height) = (25, 6)
  input <- executeParser pInput indata
  let layers = chunksOf (width * height) input
  [ (zcount, ones * twos)
    | layer <- layers
    , let zcount = layer & filter (== 0) & length
    , let ones   = layer & filter (== 1) & length
    , let twos   = layer & filter (== 2) & length
    ]
    & minimum
    & snd
    & print
  layers
    &   foldl1' addLayer
    <&> (\case
          1 -> '█'
          _ -> ' '
        )
    &   chunksOf width
    &   mapM_ putStrLn

addLayer :: [Int] -> [Int] -> [Int]
addLayer old new = [ if o == 2 then n else o | (o, n) <- zip old new ]

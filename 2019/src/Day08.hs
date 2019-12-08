{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day08
  ( main
  )
where

import           Common

import           Data.List                                ( (!!) )
import           Protolude                         hiding ( zero )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.List.Split
import qualified Prelude

pInput :: Parser [Int]
pInput = Text.Megaparsec.many $ do
  d <- anySingle
  space
  return (Prelude.read [d])

main :: Text -> IO ()
main indata = do
  input <- executeParser pInput indata
  let layers = chunksOf (25 * 6) input
  print
    $ [ (ones * twos)
      | layer <- layers
      , let zcount = filter (== 0) layer & length
      , zcount == 7
      , let ones = filter (== 1) layer & length
      , let twos = filter (== 2) layer & length
      ]
  let image  = foldl' addLayer (layers !! 0) layers
  let image' = [ if d == 1 then '█' else ' ' | d <- image ]
  let rows   = chunksOf 25 image'
  mapM_ putStrLn rows

addLayer :: (Eq a, Num a) => [a] -> [a] -> [a]
addLayer old new = [ if o == 2 then n else o | (o, n) <- zip old new ]

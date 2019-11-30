{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import           Control.Arrow
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import           Day10                                    ( knotHash )
import           Prelude                                  ( error )
import           Protolude
import           Text.Printf

readHexDigit :: Char -> Int
readHexDigit ch | ch >= '0' && ch <= '9' = fromEnum ch - fromEnum '0'
                | ch < 'a'               = error "out of range"
                | ch > 'f'               = error "out of range"
                | otherwise              = fromEnum ch - fromEnum 'a' + 10

toBinDigit :: Int -> Text
toBinDigit = printf "%04b" >>> Text.pack

grid :: Text -> [Text]
grid key = rowkeys <&> knotHash <&> reformat
 where
  rowkeys = [0 .. 127] <&> show <&> ((key <> "-") <>)
  reformat row = row & Text.unpack <&> readHexDigit <&> toBinDigit & mconcat

data DiskVal
  = O
  | X
  deriving (Eq, Show)

main :: IO ()
main = do
  let rows       = grid "xlqgujun"
  let rows       = grid "flqrgnkx"
  let wholething = rows & mconcat & Text.filter (== '1') & Text.length
  print wholething
  let diskMap = Map.fromList
        [ ((r, c), value)
        | (r, row ) <- zip [0 .. 127] (Text.unpack <$> rows)
        , (c, cell) <- zip [0 .. 127] row
        , let value = if cell == '1' then X else O
        ]
  let searchState = diskMap
  let searchLocs = (,) <$> [0..127]<*>[0..127]
  let regions ::[Int] = evalState (sequence $ fillMap <$> searchLocs)  searchState
  print regions

-- | floodfill from a location, returning the number of cells filled
fillMap :: (Int, Int) -> State (Map.Map (Int, Int) DiskVal) Int
fillMap here@(r, c) = do
  diskmap <- get
  if diskmap Map.!? here == Just X
    then return 0
    else do
      let neighbours = fillMap <$> [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
      put $ Map.insert (r, c) X diskmap
      surroundings <- sequence neighbours
      return (sum surroundings + 1)

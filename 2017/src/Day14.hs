{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Day14 where

import           Control.Arrow
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import           Day10                                    ( knotHash )
import           Prelude                                  ( error )
import           Protolude
import qualified System.IO                     as IO
import           Text.Printf

readHexDigit :: Char -> Int
readHexDigit ch | ch >= '0' && ch <= '9' = fromEnum ch - fromEnum '0'
                | ch >= 'a' && ch <= 'f' = fromEnum ch - fromEnum 'a' + 10
                | otherwise              = error "out of range"

toBinDigit :: Int -> Text
toBinDigit = printf "%04b" >>> Text.pack

grid :: Text -> [Text]
grid key = rowkeys <&> knotHash <&> reformat
 where
  rowkeys = [0 :: Int .. 127] <&> show <&> ((key <> "-") <>)
  reformat row = row & Text.unpack <&> readHexDigit <&> toBinDigit & mconcat

data DiskVal
  = O
  | X
  deriving (Eq, Show)

-- | silly function for displaying progress while doing slow hashing
loadRows :: Text -> IO [Text]
loadRows key = do
  putText "figuring out a bunch of hashes..."
  out <- forM (zip [0 :: Int ..] (grid key)) $ \(i, r) -> do
    putStr $ (show i <> " / 128\r" :: Text)
    IO.hFlush (IO.stdout)
    (r `seq` return) r
  putStr ("                \r" :: Text)
  return out

main :: IO ()
main = do
  rows <- loadRows "xlqgujun"
  let part1 = rows & mconcat & Text.filter (== '1') & Text.length
  print part1
  let diskMap = Map.fromList
        [ ((r, c), value)
        | (r, row ) <- zip [0 .. 127] (Text.unpack <$> rows)
        , (c, cell) <- zip [0 .. 127] row
        , let value = if cell == '1' then O else X
        ]
  let searchState = diskMap
  let searchLocs  = (,) <$> [0 .. 127] <*> [0 .. 127]
  let regions = evalState (sequence $ fillMap <$> searchLocs) searchState
  regions & filter (/= 0) & length & print

-- | floodfill from a location, returning the number of cells filled
fillMap :: (Int, Int) -> State (Map.Map (Int, Int) DiskVal) Int
fillMap here@(r, c) = do
  diskmap <- get
  if diskmap Map.!? here /= Just O
    then return 0
    else do
      put $ Map.insert (r, c) X diskmap
      let neighbours =
            fillMap <$> [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
      surroundings <- sequence neighbours
      return (sum surroundings + 1)

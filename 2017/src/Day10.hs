{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import           Data.List       (iterate', (!!))
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.Read  as TRead
import           Protolude
import           Text.Printf

data Ring =
  Ring
    { pos  :: Int
    , ring :: Map.Map Int Int
    , size :: Int
    , skip :: Int
    }
  deriving (Show)

-- for Day14 to import
knotHash :: Text -> Text
knotHash input = solve2 input [0..255]

step :: Ring -> Int -> Ring
step r len =
  r
    { pos = pos'
    , skip = skip r + 1
    , ring = Map.fromList (zip keys values) `Map.union` ring r
    }
  where
    keys = [p `mod` size r | p <- [pos r .. pos r + len - 1]]
    values = reverse [ring r Map.! k | k <- keys]
    pos' = (pos r + len + skip r) `mod` size r

solve :: Text -> [Int] -> Int
solve input r =
  product [(ring finished) Map.! (i `mod` size finished) | i <- [0, 1]]
  where
    lens =
      [n | len <- T.splitOn "," input, let Right (n, _) = TRead.decimal len]
    begin =
      Ring
        {skip = 0, size = length r, pos = 0, ring = Map.fromList (zip [0 ..] r)}
    finished = foldl' step begin lens

solve2 :: Text -> [Int] -> Text
solve2 input r = knotHash
  where
    lens = [ord ch | ch <- T.unpack input] <> [17, 31, 73, 47, 23]
    begin =
      Ring
        {skip = 0, size = length r, pos = 0, ring = Map.fromList (zip [0 ..] r)}
    sparse = ring $ iterate' runRound begin !! 64
    dense = [foldr xor 0 chunk | chunk <- chunksOf 16 (Map.elems sparse)]
    hexDigits = map nToHex dense
    knotHash = mconcat hexDigits :: Text
    runRound start = foldl' step start lens

nToHex :: Int -> Text
nToHex = T.pack . printf "%02x"

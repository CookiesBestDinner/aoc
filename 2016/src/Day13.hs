{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day13 where

import qualified Data.Map.Lazy as Map
import qualified Data.Text     as Text
import           Prelude       (read)
import           Protolude

main :: IO ()
main = do
  stuff <- getContents
  let n :: Int = read (Text.unpack stuff)
  let steps = search n (39, 31) Map.empty (Map.fromList [(0, [(1, 1, 0)])])
  print steps
  -- 92 is wrong, was missing current distance in heuristic (dist + manhattan)
  -- part 2, do 50 iterations of flood fill... but yeah did dijkstra's so err.
  -- start over a bit.

data Tile = Wall | Open deriving (Eq, Show)

tile n y x | odd bits  = Wall
           | otherwise = Open
  where bits = popCount $ x * x + 3 * x + 2 * x * y + y + y * y + n


popQueue :: Ord k => Map k [a] -> (a, Map k [a])
popQueue q = case v of
  [x     ] -> (x, rest)
  (x : xs) -> (x, Map.insert k xs rest)
  where ((k, v), rest) = Map.deleteFindMin q

putQueue :: (Ord k) => [(k, [a])] -> Map k [a] -> Map k [a]
putQueue pairs q = Map.unionWith (<>) q (Map.fromListWith (<>) pairs)

search n (ty, tx) visited queue | (y, x) == (ty, tx) = d
                                | beenHere = search n (ty, tx) visited q
                                | otherwise = search n (ty, tx) visited' queue'
 where
  visited'       = Map.insert (y, x) d visited
  queue'         = putQueue candidates q
  ((y, x, d), q) = popQueue queue
  beenHere       = Map.member (y, x) visited
  candidates     = do
    (yy, xx) <- (,) <$> [y - 1 .. y + 1] <*> [x - 1 .. x + 1]
    guard $ abs (y - yy) + abs (x - xx) == 1
    guard $ tile n yy xx == Open
    guard $ Map.notMember (yy, xx) visited
    let birb = abs (ty - yy) + abs (tx - xx)
    pure (d + birb, [(yy, xx, d + 1)])

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day13 where

import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
import qualified Data.Text     as Text
import           Prelude       (read)
import           Protolude

main :: IO ()
main = do
  stuff <- getContents
  let n :: Int = read (Text.unpack stuff)
  let steps = search n (39, 31) Map.empty (Map.fromList [(0, pure (1, 1, 0))])
  print steps
  print $ length $ flood n 50 (Set.singleton (1, 1)) (Set.singleton (1, 1))

data Tile = Wall | Open deriving (Eq, Show)

tile :: (Bits a, Num a) => a -> a -> a -> Tile
tile n y x | odd bits  = Wall
           | otherwise = Open
  where bits = popCount $ x * x + 3 * x + 2 * x * y + y + y * y + n


popQueue :: Ord k => Map k (NonEmpty a) -> (a, Map k (NonEmpty a))
popQueue q = case v of
  (x :| []      ) -> (x, rest)
  (a :| (b : xs)) -> (a, Map.insert k (b :| xs) rest)
  where ((k, v), rest) = Map.deleteFindMin q

putQueue
  :: (Ord k)
  => NonEmpty (k, NonEmpty a)
  -> Map k (NonEmpty a)
  -> Map k (NonEmpty a)
putQueue pairs q = Map.unionWith (<>) q (Map.fromListWith (<>) (toList pairs))

search
  :: Int
  -> (Int, Int)
  -> Map (Int, Int) Int
  -> Map Int (NonEmpty (Int, Int, Int))
  -> Int
search n (ty, tx) visited queue | (y, x) == (ty, tx) = d
                                | beenHere = search n (ty, tx) visited q
                                | otherwise = search n (ty, tx) visited' queue'
 where
  visited'       = Map.insert (y, x) d visited
  ((y, x, d), q) = popQueue queue
  beenHere       = Map.member (y, x) visited
  queue'         = case nonEmpty candidates of
    Just xs -> putQueue xs q
    Nothing -> q
  candidates = do
    (yy, xx) <- adjacent (y, x)
    guard $ tile n yy xx == Open
    guard $ Map.notMember (yy, xx) visited
    let birb = abs (ty - yy) + abs (tx - xx)
    pure (d + birb, pure (yy, xx, d + 1))

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (y, x) = do
  (yy, xx) <- (,) <$> [y - 1 .. y + 1] <*> [x - 1 .. x + 1]
  guard $ yy >= 0
  guard $ xx >= 0
  guard $ abs (y - yy) + abs (x - xx) == 1
  pure (yy, xx)


flood :: Int -> Int -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
flood n i visited heres | i == 0     = visited
                        | null heres = visited
                        | otherwise  = flood n (i - 1) visited' new
 where
  visited' = visited <> new
  new      = Set.fromList $ do
    (y , x ) <- Set.toList heres
    (yy, xx) <- adjacent (y, x)
    guard $ tile n yy xx == Open
    guard $ Set.notMember (yy, xx) visited
    pure (yy, xx)

-- | Barely sufficient.
-- part2 used 14GB (on a 16GB machine, not sure if it turned out 14 because of
-- that limit but could keep going anyway?) resident memory and took 3 minutes to run
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}

module Day17 where

import Protolude
import qualified Data.Sequence as Seq

main :: IO ()
main = do
  let input = zip [1..] (replicate 2017 354)
      buffer = Seq.fromList [0]
      begin = (0, buffer)

  foldl' insert begin input & snd & Seq.dropWhileL (/= 2017) & Seq.take 2 & print
  let input = zip [1..] (replicate 50_000_000 354)
  foldl' insert begin input & snd & Seq.dropWhileL (/= 0) & Seq.take 2 & print


insert :: (Int, Seq Int) -> (Int, Int) -> (Int, Seq Int)
insert (loc, buf) (x, steps) = (at, buf')
  where
    at = ((loc + steps) `mod` Seq.length buf) + 1
    buf' = Seq.insertAt at x buf

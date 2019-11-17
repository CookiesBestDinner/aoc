{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Control.Arrow
import Control.Monad
import Protolude
import Data.Text.Read (decimal)
import qualified Data.Set as Set
import qualified Data.Text as T

makeGroupOf :: Int -> Int -> Set.Set Int -> [[Int]]
makeGroupOf n limit xs
  | n == limit = [[]]
  | Set.null xs = []
  | otherwise = do
      let (x, xs') = Set.deleteFindMin xs
      guard $ n + x <= limit
      keep <- [[], [x]]
      restCandidate <- makeGroupOf (n + sum keep) limit xs'
      if null keep then [restCandidate] else [x : restCandidate]

main = do
  input <- (T.lines >>> map decimal >>> rights >>> map fst) <$> getContents
  print input
  -- each group should have a third
  -- except for part2, where it's a fourth.
  -- let gsize = sum input `div` 3
  let gsize = sum input `div` 4
      inputSet = Set.fromList input
      g1 = makeGroupOf 0 gsize inputSet & minimumBy (compare `on` length &&& product)
  print g1
  print $ product g1

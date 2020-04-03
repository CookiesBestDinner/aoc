{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day19 where

-- heads up: uses 8-9GB memory (memory leak?)

import           Control.Monad.Loops
import qualified Data.Sequence                 as S
import           Data.STRef
import           Protolude

main :: IO ()
main = do
  let input = 3012210
  print $ sim input (\i size -> (i + 1) `mod` size)
  print $ sim input (\i size -> (`mod` size) $ i + (size `div` 2))

sim :: Int -> (Int -> Int -> Int) -> Int
sim elfcount stealFrom = runST $ do
  current <- newSTRef $ S.fromList [ (elf, 1 :: Int) | elf <- [1 .. elfcount] ]
  whileM_ (((> 1) . S.length) <$> readSTRef current) $ do
    i <- newSTRef 0
    let inrange = (<) <$> readSTRef i <*> (readSTRef current <&> S.length)
    whileM_ inrange $ do
      here   <- readSTRef i
      circle <- readSTRef current
      let size = S.length circle
      when (size > 1) $ do
        let victim = stealFrom here size
            adjuster (id, n) = (id, n + (snd $ S.index circle victim))
        circle
          & S.adjust' adjuster here
          & S.deleteAt victim
          & writeSTRef current
        when (victim > here) $ do
          modifySTRef' i (+ 1)
  readSTRef current <&> (`S.index` 0) <&> fst

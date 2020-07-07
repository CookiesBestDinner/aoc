{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import           Protolude

discs1, discs2 :: [(Int, Int)]
discs1 = adjust [(13, 1), (19, 10), (3, 2), (7, 1), (5, 3), (17, 5)]
discs2 = adjust [(13, 1), (19, 10), (3, 2), (7, 1), (5, 3), (17, 5), (11, 0)]

adjust :: Integral b => [(b, b)] -> [(b, b)]
adjust ds = do
  ((n, s), t) <- zip ds [1 ..]
  pure (n, (s + t) `mod` n)

atTime :: Int -> Int -> Int -> Int
atTime t p x = (t + x) `mod` p

sol :: (Foldable t, Functor t) => t (Int, Int) -> Maybe Int
sol discs = head $ do
  t <- [0 ..]
  let outcomes = uncurry (atTime t) <$> discs
  guard $ all (== 0) outcomes
  pure t

main :: IO ()
main = print $ sol <$> [discs1, discs2]

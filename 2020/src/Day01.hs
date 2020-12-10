module Day01 where

import           Protolude

import           Parsing

part1 x xs = do
  a <- xs
  b <- xs
  guard $ a /= b
  guard $ a + b == x
  pure $ a * b

part2 x xs = do
  a <- xs
  b <- xs
  guard $ a /= b
  c <- xs
  guard $ c /= a
  guard $ c /= b
  guard $ a + b + c == x
  pure $ a * b * c

main :: Text -> IO ()
main input = do
  indata <- parse' numbers input
  print $ take 1 $ part1 2020 indata
  print $ take 1 $ part2 2020 indata

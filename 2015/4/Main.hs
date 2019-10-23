module Main where

import Data.Hash.MD5

main = do
  secret <- concat . lines <$> getContents
  let nums = [0 ..]
      answer = head $ filter (verify secret) nums
      answer2 = head $ filter (verify2 secret) nums
  print answer
  print answer2

verify :: String -> Integer -> Bool
verify s n = take 5 hash == "00000"
  where hashMe = s ++ show n
        hash = md5s (Str hashMe)


verify2 :: String -> Integer -> Bool
verify2 s n = take 6 hash == "000000"
  where hashMe = s ++ show n
        hash = md5s (Str hashMe)

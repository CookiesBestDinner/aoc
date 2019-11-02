module Main where

import           Data.List
import           Control.Arrow
import           Data.Function

main = do
  let input = 36000000 :: Integer
  let res = houses & dropWhile (presents >>> (< input)) & take 1
  print res
  let res2 = houses & dropWhile (presents2 >>> (< input)) & take 1
  print res2

factors = go 1
 where
  go d x = if d * d > x
    then []
    else
      let (f, m) = x `divMod` d
      in  if m == 0 then nub [f, x `div` f] ++ go (d + 1) x else go (d + 1) x


presents2 n = sum $ (* 11) <$> remainingElves
 where
  remainingElves = filter ((*50) >>> (>= n)) (factors n)

presents n = sum $ (* 10) <$> factors n

houses = [1 ..]

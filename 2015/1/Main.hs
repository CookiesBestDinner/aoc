module Main where

import           Prelude                           hiding ( floor )

main :: IO ()
main = do
  input' <- getContents
  let input = [ if x == '(' then UP else DOWN | x <- input', x `elem` "()" ]
  putStr "Answer to problem 1: "
  print $ solve1 input
  putStr "Answer to problem 2: "
  let posIns = zip [1 ..] input
  print $ basement posIns

solve1 :: [Direction] -> Integer
solve1 []          = 0
solve1 (UP   : xs) = 1 + solve1 xs
solve1 (DOWN : xs) = (-1) + solve1 xs

data Direction
  = UP
  | DOWN
  deriving (Show)

basement :: [(Integer, Direction)] -> Maybe Integer
basement = go 0
 where
  go :: Integer -> [(Integer, Direction)] -> Maybe Integer
  go 0     ((pos, DOWN) : _ ) = Just pos
  go floor ((_  , DOWN) : xs) = go (floor - 1) xs
  go floor ((_  , UP  ) : xs) = go (floor + 1) xs
  go _     []                 = Nothing

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( main
  )
where

import           Common

import           Protolude
import           Text.Megaparsec.Char.Lexer

pInput :: Parser (Integer, Integer)
pInput = (,) <$> decimal <* "-" <*> decimal

main :: Text -> IO ()
main input = do
  (lo, hi) <- executeParser pInput input
  let criteria   = (&&) <$> hasAdjacent <*> isSorted
  let candidates = digits <$> [lo .. hi]
  print $ length (candidates & filter criteria)
  let criteria2 = (&&) <$> isSorted <*> hasDouble
  print $ length (candidates & filter criteria2)

digits :: Integer -> [Integer]
digits = reverse . go
 where
  go :: Integer -> [Integer]
  go 0 = [0]
  go n | n < 10    = [n]
       | otherwise = n `mod` 10 : go (n `div` 10)

hasDouble :: Eq a => [a] -> Bool
hasDouble ds = group ds & filter ((== 2) . length) & not . null

hasAdjacent :: Eq a => [a] -> Bool
hasAdjacent (a : b : xs) | a == b    = True
                         | otherwise = hasAdjacent (b : xs)
hasAdjacent _ = False

isSorted :: Ord a => [a] -> Bool
isSorted (a : b : xs) | b < a     = False
                      | otherwise = isSorted (b : xs)
isSorted _ = True

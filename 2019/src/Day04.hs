{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04
  ( main
  )
where

import           Common

import           Protolude
import           Control.Arrow
import           Text.Megaparsec.Char.Lexer

pInput :: Parser (Int, Int)
pInput = (,) <$> decimal <* "-" <*> decimal

main :: Text -> IO ()
main input = do
  (lo, hi) <- executeParser pInput input
  let candidates = [lo .. hi] <&> digits & filter isSorted
  candidates & filter hasAdjacent & length & print
  candidates & filter hasDouble & length & print

digits :: Int -> [Int]
digits = reverse . go
 where
  go :: Int -> [Int]
  go 0 = [0]
  go n | n < 10    = [n]
       | otherwise = n `mod` 10 : go (n `div` 10)

hasDouble :: Eq a => [a] -> Bool
hasDouble = group >>> any (length >>> (== 2))

hasAdjacent :: Eq a => [a] -> Bool
hasAdjacent (a : b : xs) | a == b    = True
                         | otherwise = hasAdjacent (b : xs)
hasAdjacent _ = False

isSorted :: Ord a => [a] -> Bool
isSorted (a : b : xs) | b < a     = False
                      | otherwise = isSorted (b : xs)
isSorted _ = True

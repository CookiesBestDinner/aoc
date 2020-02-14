{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day20 where
import           Data.List                                ( delete )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer               ( decimal )

data Range = Range { begin :: Int, end :: Int } deriving (Show, Eq)

inRange :: Int -> Range -> Bool
inRange x (Range lo hi) = x >= lo && x <= hi

pRanges :: Parsec Void Text [Range]
pRanges = pRange `sepEndBy1` space <* eof

pRange :: Parsec Void Text Range
pRange = Range <$> decimal <* "-" <*> decimal

day20main :: IO ()
day20main = do
  input  <- readFile "input"
  parsed <- case parse pRanges "" input of
    Right wooo -> pure wooo
    Left  boo  -> putStr (errorBundlePretty boo) >> exitFailure
  print $ take 1 $ findIPs parsed 0
  print $ length $ findIPs parsed 0

findIPs :: [Range] -> Int -> [Int]
findIPs ranges ip
  | ip > 4294967295 = []
  | otherwise = case find (inRange ip) ranges of
    Just r@(Range _ hi) -> findIPs (delete r ranges) (hi + 1)
    Nothing             -> ip : findIPs ranges (ip + 1)

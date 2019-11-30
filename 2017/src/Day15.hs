{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Day15 where

import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Control.Arrow

type Parser = Parsec Void Text

pInput :: Parser (Integer, Integer)
pInput = do
  "Generator A starts with "
  a <- decimal
  newline >> "Generator B starts with "
  b <- decimal
  newline >> eof
  return (a, b)

parseInput :: Text -> IO (Integer, Integer)
parseInput input = case parse pInput "" input of
  Left boo -> do
    putStr $ errorBundlePretty boo
    exitFailure
  Right yay -> return yay

main :: IO ()
main = do
  (abegin, bbegin) <- getContents >>= parseInput
  let genA = generator genAMul abegin <&> getLow16
      genB = generator genBMul bbegin <&> getLow16
  zipWith (==) genA genB
    & take 40_000_000
    & filter (== True)
    & length
    & print
  zipWith (==) (part2GenA abegin <&> getLow16) (part2GenB bbegin <&> getLow16)
    & take 5_000_000
    & filter (== True)
    & length
    & print

generator :: Integer -> Integer -> [Integer]
generator mul !start = here : generator mul here
  where here = (mul * start) `mod` 2147483647

part2GenA :: Integer -> [Integer]
part2GenA = generator genAMul >>> filter ((`mod` 4) >>> (== 0))
part2GenB :: Integer -> [Integer]
part2GenB = generator genBMul >>> filter ((`mod` 8) >>> (== 0))

genAMul :: Integer
genAMul = 16807
genBMul :: Integer
genBMul = 48271

low16bitmask :: Integer
low16bitmask = 2 ^ (16 :: Int) - 1

getLow16 :: Integer -> Integer
getLow16 = (.&. low16bitmask)

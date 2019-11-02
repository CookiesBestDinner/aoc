{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Protolude
import           Text.Megaparsec
import           Data.List                                ( iterate' )
import           Text.Megaparsec.Char.Lexer               ( decimal )
import           Control.Arrow

type Parser = Parsec Void Text

pInput :: Parser (Int, Int)
pInput =
  (,)
    <$  "To continue, please consult the code grid in the manual."
    <*  "  Enter the code at row "
    <*> decimal
    <*  ", column "
    <*> decimal
    <*  "."

readInput :: IO (Int, Int)
readInput = do
  input <- getContents
  case parse pInput "" input of
    Left bundle -> do
      putStr $ errorBundlePretty bundle
      exitFailure
    Right res -> return res

nextVal :: Integer -> Integer
nextVal x = x * 252533 `mod` 33554393

keys :: [Integer]
keys = iterate' nextVal 20151125

indices :: [(Int, Int)]
indices = iterate' nextIndex (1, 1)

nextIndex :: (Int, Int) -> (Int, Int)
nextIndex (1  , col) = (col + 1, 1)
nextIndex (row, col) = (row - 1, col + 1)

infinitePaper :: [((Int, Int), Integer)]
infinitePaper = zip indices keys

main :: IO ()
main = do
  (row, col) <- readInput
  print row
  print col
  infinitePaper & dropWhile (fst >>> (/= (row, col))) & take 1 & print

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude
import           Data.Attoparsec.Text
import           Control.Arrow
import qualified Data.Set                      as Set

abba (a : b : c : d : rest) = if a /= b && a == d && b == c
  then [a, b, c, d] : continue
  else continue
  where continue = abba (b : c : d : rest)
abba _ = []

hasAbba = abba >>> (not . null)

aba (a : b : c : rest) = if a /= b && a == c
  then [a, b, c] : continue
  else continue
  where continue = aba (b : c : rest)
aba _ = []

supportsSSL :: [Either [Char] [Char]] -> Bool
supportsSSL ip = Set.size >>> (> 0) $ Set.intersection
  (Set.fromList $ invert <$> babs)
  (Set.fromList abas)
 where
  abas = concat $ aba <$> rights ip
  babs = concat $ aba <$> lefts ip
  invert [a, b, _] = [b, a, b]

supportsTLS :: [Either [Char] [Char]] -> Bool
supportsTLS ip = any hasAbba unbraced && not (any hasAbba braced)
 where
  braced   = lefts ip
  unbraced = rights ip

pIPV7 :: Parser [Either [Char] [Char]]
pIPV7 = do
  let letters       = many1 letter
      bracedLetters = "[" *> letters <* "]"
      part          = (Right <$> letters) <|> (Left <$> bracedLetters)
  many part <* endOfLine

main = do
  Right input <- parseOnly (many pIPV7) <$> getContents
  print $ (supportsTLS `filter` input) & length
  print $ (supportsSSL `filter` input) & length

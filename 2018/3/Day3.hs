{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Attoparsec.Text
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let claims = fromRight undefined $ parseOnly (pClaim `sepBy` char '\n') input
      marked      = markClaims claims
      part1       = length $ filter (> 1) $ M.elems marked
      part2       = filter (noOverlap marked) claims
      prettyPart2 = intercalate ", " (map (('#' :) . show . n) part2)
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ prettyPart2

fromRight :: Either a b -> b
fromRight (Right x) = x

-- check whether a claim does not overlap with any other claim
-- all coordinates of the claim will have a sum of 1
noOverlap :: Map (Integer, Integer) Integer -> Claim -> Bool
noOverlap marked claim = all
  (== 1)
  [ marked M.! (x, y)
  | x <- [col claim .. col claim + width claim - 1]
  , y <- [row claim .. row claim + height claim - 1]
  ]

-- add 1 to each coordinate that a claim covers
markClaims :: [Claim] -> Map (Integer, Integer) Integer
markClaims cs = M.fromListWith
  (+)
  [ ((col, row), 1)
  | c   <- cs
  , col <- [col c .. col c + width c - 1]
  , row <- [row c .. row c + height c - 1]
  ]

data Claim = Claim { n      :: Integer
                   , col    :: Integer
                   , row    :: Integer
                   , width  :: Integer
                   , height :: Integer
                   } deriving (Show)

pClaim :: Parser Claim
pClaim =
  Claim
    <$  char '#'
    <*> decimal
    <*  string " @ "
    <*> decimal
    <*  char ','
    <*> decimal
    <*  string ": "
    <*> decimal
    <*  char 'x'
    <*> decimal

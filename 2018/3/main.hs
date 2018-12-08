-- $ ghc -O2 main.hs && time ./main < zdata
-- 111935
-- real    0m0.788s
-- user    0m0.748s
-- sys     0m0.033s

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Char
import Data.Map.Strict (Map)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

main :: IO ()
main = interact run

run :: String -> String
run s = show claims
 where
  claims       = runParse parseClaims s
  marked       = markClaims claims
  overlapCount = length $ filter (> 1) $ M.elems marked

markClaims :: [Claim] -> Map Int Int
markClaims cs = M.fromListWith
  (+)
  [ (col * 10000 + row, 1)
  | c   <- cs
  , col <- [col c .. col c + width c - 1]
  , row <- [row c .. row c + height c - 1]
  ]

data Claim = Claim { n      :: Int
                   , col    :: Int
                   , row    :: Int
                   , width  :: Int
                   , height :: Int
                   } deriving (Show)

runParse :: ReadP a -> String -> a
runParse parser s = case readP_to_S parser s of
  [(result, [])] -> result
  _              -> error "failed to parse"

parseClaims :: ReadP [Claim]
parseClaims = do
  cs <- many claim
  eof
  return cs

claim :: ReadP Claim
claim = do
  char '#'
  n <- fmap read $ many1 $ satisfy isDigit
  string " @ "
  col <- fmap read $ many1 $ satisfy isDigit
  char ','
  row <- fmap read $ many1 $ satisfy isDigit
  string ": "
  width <- fmap read $ many1 $ satisfy isDigit
  char 'x'
  height <- fmap read $ many1 $ satisfy isDigit
  char '\n'
  return $ Claim {n , col , row , width , height }

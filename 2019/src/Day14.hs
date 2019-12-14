{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings   #-}

module Day14 where

import           Common

import qualified Data.Map.Strict               as Map
import           Protolude
import           Text.Megaparsec         hiding ( State(..) )
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Reaction = (Int, [(Int, Text)])

parseInput :: Parser (Map Text Reaction)
parseInput = Map.fromList <$> pLines
 where
  pLines = pLine `sepEndBy1` eol
  pLine  = do
    reqs <- pIngredients
    " => "
    (n, gives) <- pAmntName
    return (gives, (n, reqs))
  pIngredients = pAmntName `sepBy1` ", "
  pAmntName    = do
    n <- decimal
    space
    name <- takeWhile1P Nothing (`elem` ['A' .. 'Z'])
    return (n, name)

main :: Text -> IO ()
main indata = do
  chart <- executeParser parseInput indata
  let part1 = evalState (fetch chart (1, "FUEL")) Map.empty
  print part1
  let trillion = 1_000_000_000_000
  let p x = evalState (fetch chart (x, "FUEL")) Map.empty & (<= trillion)
  let lo = trillion `div` part1
  let hi = findHighBound p lo
  print $ findLargest p lo hi

findLargest :: (Int -> Bool) -> Int -> Int -> Int
findLargest p lo hi | lo + 1 == hi = lo
                    | p mid        = findLargest p mid hi
                    | otherwise    = findLargest p lo mid
  where mid = (hi + lo) `div` 2

findHighBound :: (Int -> Bool) -> Int -> Int
findHighBound p low | p low     = findHighBound p (low * 2)
                    | otherwise = low

fetch :: Map Text Reaction -> (Int, Text) -> State (Map.Map Text Int) Int
fetch _     (0, _    ) = pure 0
fetch _     (n, "ORE") = pure n
fetch chart (n, stuff) = do
  needThisMany <- do
    inventory <- get
    let got    = Map.findWithDefault 0 stuff inventory
    let excess = max 0 (got - n)
    put $ Map.insert stuff excess inventory
    return $ max 0 (n - got)
  let (yieldAmnt, reqs) = chart Map.! stuff
  let batchesToMake     = (needThisMany + yieldAmnt - 1) `div` yieldAmnt
  let shoppingList = do
        (reqN, reqName) <- reqs
        return (reqN * batchesToMake, reqName)
  shoppingBill <- sum <$> mapM (fetch chart) shoppingList
  let spares = batchesToMake * yieldAmnt - needThisMany
  sack <- get
  put $ Map.insertWith (+) stuff spares sack
  return shoppingBill

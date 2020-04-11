{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day24 where

import qualified Data.IntMap.Strict            as Map
import           Data.List                                ( (\\) )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex

type Parser = Parsec Void Text

pComp :: Parser (Int, Int)
pComp = (,) <$> Lex.decimal <* "/" <*> Lex.decimal

pInput :: Parser [(Int, Int)]
pInput = pComp `sepEndBy1` space

main :: IO ()
main = do
  s <- getArgs >>= \case
    []     -> getContents
    [path] -> readFile path
  input <- case parse pInput "" s of
    Right yay -> pure yay
    Left  boo -> (putStr $ errorBundlePretty boo) >> exitFailure
  let components = Map.fromListWith (<>) $ do
        (a, b) <- input
        [(a, [b]), (b, [a])]
  print $ bridge 0 0 components
  print $ maximum $ bridge2 0 0 0 components

bridge2 :: Int -> Int -> Int -> Map.IntMap [Int] -> [(Int, Int)]
bridge2 len acc a inventory = do
  let remove n = Map.adjust (\\ [n])
  b <- (inventory Map.!? a) & maybeToList & mconcat
  let inv    = remove a b $ remove b a $ inventory
  let here   = (len, acc + a + b)
  let future = bridge2 (succ len) (acc + a + b) b inv
  if null future then [here] else future

bridge :: Int -> Int -> Map.IntMap [Int] -> Int
bridge acc a inventory = maximumDef 0 $ do
  let remove n = Map.adjust (\\ [n])
  b <- (inventory Map.!? a) & maybeToList & mconcat
  let inv = remove a b $ remove b a $ inventory
  [(acc + a + b), bridge (acc + a + b) b inv]

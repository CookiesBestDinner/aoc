{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import           Common
import           Intcode

import           Conduit
import           Control.Arrow
import           Control.Lens
import           Data.List.Extra                ( chunksOf )
import qualified Data.Map.Strict               as Map
import           Text.Show.Pretty
import qualified Prelude
import           Protolude
import qualified Data.Text                     as T
import           Text.Megaparsec (many, satisfy, sepBy1, sepEndBy1)
import           Text.Megaparsec.Char           ( space
                                                , eol
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                )

pnsomething :: Parser (Int, Text)
pnsomething = do
  n <- decimal
  space
  name <- Text.Megaparsec.many (satisfy (`elem` ['A' .. 'Z']))
  return (n, T.pack name)

preqs :: Parser [(Int, Text)]
preqs = pnsomething `sepBy1` ", "

parseInput :: Parser [(Text, (Int, [(Int, Text)]))]
parseInput = (`sepEndBy1` eol) $ do
  reqs <- preqs
  " => "
  (n, gives) <- pnsomething
  return (gives, (n, reqs))

main :: Text -> IO ()
main indata = do
  reqs <- executeParser parseInput indata
  let chart = Map.fromList reqs
  let (c, s) = runState (fetch chart (1, "FUEL")) Map.empty
  print c
  -- got this number with manual binary search, fix later
  let (c, s) = runState (fetch chart (4052920, "FUEL")) Map.empty
  print c
  let tril = 1000000000000
  -- printing out whether over-shooting in my binary search
  print (c - tril)

fetch :: (Map Text (Int, [(Int, Text)])) -> (Int, Text) -> State (Map.Map Text Int) Int
fetch chart (n, "ORE") = pure n
fetch chart (n, stuff) = do
  inventory <- get
  -- got stuff?
  let got = Map.findWithDefault 0 stuff inventory
  let excess = max 0 (got - n)
  -- put the extra back (one can wish)
  let putback = Map.insert stuff excess
  put (putback inventory)
  -- fetch the rest.
  let rest = max 0 (n - got)
  -- ...what do I need for that?
  let (yieldAmnt, reqs) = chart Map.! stuff
  let batchesToMake = rest `batchCount` yieldAmnt
  let shoppingList = [ (reqN * batchesToMake, reqName)
                     | (reqN, reqName) <- reqs
                     ]
  -- fetch each such bunch
  shoppingBill <- sum <$> mapM (fetch chart) shoppingList

  -- oh and I made too many. how many?
  let madeSomeExtra = batchesToMake * yieldAmnt - rest
  -- .. put that back.
  sack <- get
  put $ Map.insertWith (+) stuff madeSomeExtra sack
  -- here's what this adventure cost me
  pure shoppingBill

batchCount :: Int -> Int -> Int
batchCount need size = fullBatches + oneMore
  where
    fullBatches = need `div` size
    oneMore = fromEnum $ (need `mod` size) > 0

module Main where

import           Control.Arrow                            ( (>>>) )
import           Control.Monad                            ( guard )
import           Data.Char                                ( isNumber )
import           Data.Function                            ( (&) )
import           Data.List.Extra                          ( chunksOf )
import qualified Data.Map.Strict               as Map


day1match attributes = Map.null $ Map.differenceWith
  (\sue realSue -> if sue == realSue then Nothing else Just sue)
  attributes
  theRealSue

day2match attributes = Map.null $ Map.differenceWithKey
  (\attr sue reading ->
    let okay | attr `elem` ["cats", "trees"]           = sue > reading
             | attr `elem` ["pomeranians", "goldfish"] = sue < reading
             | otherwise                               = sue == reading
    in  if okay then Nothing else Just sue
  )
  attributes
  theRealSue

main = do
  sues <- (lines >>> map parseSue) <$> getContents
  let candidates = do
        (num, attributes) <- sues
        guard $ day1match attributes
        [num]
  print candidates
  let candidates2 = do
        (num, attributes) <- sues
        guard $ day2match attributes
        [num]
  print candidates2

parsekvp :: [String] -> (String, Int)
parsekvp [name', amount'] = (name, amount)
 where
  name   = init name'
  amount = read $ takeWhile (isNumber) amount'

parseSue :: String -> (Int, Map.Map String Int)
parseSue ln = (num, Map.fromList keyvaluepairs)
 where
  tokens        = words ln
  num           = read $ init $ tokens !! 1
  keyvaluepairs = drop 2 tokens & chunksOf 2 & map parsekvp

theRealSue = Map.fromList
  [ ("children"   , 3)
  , ("cats"       , 7)
  , ("samoyeds"   , 2)
  , ("pomeranians", 3)
  , ("akitas"     , 0)
  , ("vizslas"    , 0)
  , ("goldfish"   , 5)
  , ("trees"      , 3)
  , ("cars"       , 2)
  , ("perfumes"   , 1)
  ]

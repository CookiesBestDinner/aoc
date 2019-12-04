{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import           Data.List                                ( last )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Prelude                                  ( (!!) )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char.Lexer

main :: Text -> IO ()
main input = do
  let lns = T.lines input
  a <- parseLine $ lns !! 0
  b <- parseLine $ lns !! 1
  let aTour  = walk (0, 0) a
  let bTour  = walk (0, 0) b
  let common = Set.fromList aTour `Set.intersection` Set.fromList bTour
  common & Set.elems <&> manhattan & minimum & print
  -- part2
  let mkStepDistMap tour =
        Map.fromListWith (flip const) (zip tour [1 :: Integer ..])
      sdma = mkStepDistMap aTour
      sdmb = mkStepDistMap bTour
  let totalSteps wh = sdma Map.! wh + sdmb Map.! wh
  common & Set.elems <&> totalSteps & minimum & print

manhattan :: (Integer, Integer) -> Integer
manhattan (x, y) = abs x + abs y

walk :: (Integer, Integer) -> [Dir] -> [(Integer, Integer)]
walk here (x : xs) = steps <> walk there xs
 where
  steps = step here x
  there = last steps
walk _ [] = []

step :: (Integer, Integer) -> Dir -> [(Integer, Integer)]
step (x, y) (Dup    n) = [ (x, y + i) | i <- [1 .. n] ]
step (x, y) (Ddown  n) = [ (x, y - i) | i <- [1 .. n] ]
step (x, y) (Dright n) = [ (x + i, y) | i <- [1 .. n] ]
step (x, y) (Dleft  n) = [ (x - i, y) | i <- [1 .. n] ]

data Dir
  = Dup Integer
  | Ddown Integer
  | Dleft Integer
  | Dright Integer
  deriving (Eq, Show)

parseLine :: Text -> IO [Dir]
parseLine ln = case parse pDirs "" ln of
  Left boo -> do
    putStr $ errorBundlePretty boo
    exitFailure
  Right yay -> return yay

type Parser = Parsec Void Text

pDirs :: Parser [Dir]
pDirs = oneDir `sepBy` ","
 where
  oneDir = choice
    [ Dup <$ "U" <*> decimal
    , Ddown <$ "D" <*> decimal
    , Dleft <$ "L" <*> decimal
    , Dright <$ "R" <*> decimal
    ]

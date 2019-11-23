{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import           Protolude hiding (option)
import qualified Data.Text as Text
import           Text.Megaparsec
import Data.List.Extra (maximumOn)
import           Text.Megaparsec.Char

pStep :: Parsec Void Text Coord
pStep =
  choice
    [ "ne" $> Coord {x = 1, y = 1}
    , "se" $> Coord {x = 1, y = -1}
    , "nw" $> Coord {x = -1, y = 1}
    , "sw" $> Coord {x = -1, y = -1}
    , "s" $> Coord {x = 0, y = -2}
    , "n" $> Coord {x = 0, y = 2}
    ]

pSteps :: Parsec Void Text [Coord]
pSteps = (pStep`sepBy1`",") <* space <* eof

data Coord =
  Coord
    { x :: Int
    , y :: Int
    }
    deriving (Show, Eq)

instance Semigroup Coord where
  a <> b = Coord {x = x a + x b, y = y a + y b}

instance Monoid Coord where
  mempty = Coord 0 0

-- part1 :: Text -> Either (ParseErrorBundle Text Void) [Coord]
part1 input = do
   steps <- case parse pSteps "" input of
              Right yay -> Right yay
              Left boo -> Left $ traceId $ Text.pack $ errorBundlePretty boo
   let delta = mconcat steps
   return $ countDistance delta

part2 input = do
   steps <- case parse pSteps "" input of
              Right yay -> Right yay
              Left boo -> Left $ traceId $ Text.pack $ errorBundlePretty boo
   let
     deltas :: [Coord]
     deltas = scanl' (<>) (Coord 0 0) steps
     distances = deltas <&> countDistance
   -- return $ (maximum `on` countDistance) deltas
   return $ maximum distances

countDistance c = abs (x c) + (y' `div` 2)
  where
    y' = (abs (y c) - abs (x c)) & \n -> if n < 0 then n `mod` 2 else n

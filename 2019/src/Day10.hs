{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import           Control.Arrow
import           Data.Fixed                               ( mod' )
import           Data.List.Extra                   hiding ( sortOn )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Protolude

-- |
-- >>> readFile "input/day10" >>= main
-- 274
-- 305
main :: Text -> IO ()
main indata = do
  let asteroids =
        [ (x, y)
        | (y, row ) <- zip [0 ..] (T.lines indata)
        , (x, cell) <- zip [0 ..] (T.unpack row)
        , cell == '#'
        ]
  let scan src = sortOn (manhattan src) <$> Map.fromListWith
        (<>)
        [ (dxdy src ast, [ast]) | ast <- asteroids, ast /= src ]
  let (part1, laser) =
        asteroids <&> ((scan >>> Map.keys >>> length) &&& identity) & maximum
  print part1
  let immafirinmalazors = sort
        [ (layer, Angle angle, ast)
        | (angle, targets) <- scan laser & Map.toList
        , ast              <- targets
        , let layer = elemIndex ast targets
        ]
  let (_, _, (x, y)) = immafirinmalazors & (!! 199)
  print (100 * x + y)

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

dxdy :: (Int, Int) -> (Int, Int) -> (Int, Int)
dxdy (x1, y1) (x2, y2) = (dx `div` multiple, dy `div` multiple)
 where
  dx       = x2 - x1
  dy       = y2 - y1
  multiple = gcd dx dy

newtype Angle = Angle (Int, Int) deriving (Eq, Show)

instance Ord Angle where
  compare a@(Angle (x1, y1)) b@(Angle (x2, y2))
    | a == b    = EQ
    | otherwise = compare (angle x1 y1) (angle x2 y2)
   where
    angle :: Int -> Int -> Double
    angle x y = atan2 (fromIntegral x) (fromIntegral (-y)) `mod'` (2 * pi)

{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import           Data.Fixed                               ( mod' )
import           Data.List.Extra
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Protolude

data Angle =
  Angle
    { _dx :: Int
    , _dy :: Int
    }
  deriving (Eq, Show)

instance Ord Angle where
  compare a@(Angle x1 y1) b@(Angle x2 y2)
    | a == b    = EQ
    | otherwise = compare (angle x1 y1) (angle x2 y2)
   where
    c = fromIntegral
    angle :: Int -> Int -> Double
    angle x y = atan2 (c x) (c (-y)) `mod'` (2 * pi)

-- |
-- >>> readFile "input/day10" >>= main
-- 274
-- 305
main :: Text -> IO ()
main indata = do
  let asteroids = Set.fromList
        [ (x, y)
        | (y, row ) <- zip [0 ..] (T.lines indata)
        , (x, cell) <- zip [0 ..] (T.unpack row)
        , cell == '#'
        ]
  let counts = Map.fromListWith
        (+)
        [ ((x, y), 1 :: Int)
        | self@( x , y ) <- Set.elems asteroids
        , other@(x', y') <- Set.elems asteroids
        , self /= other
        , let multiple = gcd (x' - x) (y' - y)
        , let (dx, dy) = ((x' - x) `div` multiple, (y' - y) `div` multiple)
        , let between = takeWhile
                (\b -> distance self b < distance self other)
                [ (xx, yy)
                | steps <- [1 ..]
                , let xx = x + steps * dx
                , let yy = y + steps * dy
                , (xx, yy) `Set.member` asteroids
                ]
        , null between
        ]
  let ((laserX, laserY), part1) = Map.toList counts & maximumOn snd
  print part1
  let dirToAsteroid = sort <$> Map.fromListWith
        (<>)
        [ (angle, [(x, y)])
        | (x, y) <- Set.elems asteroids
        , (x, y) /= (laserX, laserY)
        , let (dx, dy) = (x - laserX, y - laserY)
        , let multiple = gcd dx dy
        , let angle = (dx `div` multiple, dy `div` multiple)
        ]
  let immafirinmalazors = sort
        [ (layer, Angle ax ay, (x, y))
        | (x, y) <- Set.elems asteroids
        , (x, y) /= (laserX, laserY)
        , let (dx, dy) = (x - laserX, y - laserY)
        , let multiple       = gcd dx dy
        , let angle@(ax, ay) = (dx `div` multiple, dy `div` multiple)
        , let sameAngle = dirToAsteroid Map.! angle
        , let layer = elemIndex (x, y) sameAngle
        ]
  let (_, _, (x, y)) = immafirinmalazors & (!! 199)
  print (100 * x + y)

distance :: Num a => (a, a) -> (a, a) -> a
distance (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

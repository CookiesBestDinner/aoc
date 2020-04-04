{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Day22 where

import           Data.List                                ( iterate'
                                                          , (!!)
                                                          )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Protolude

type Grid = Map.Map (Int, Int) Cell
data Cell = Infected | Healthy | Weakened | Flagged deriving Show
data Orientation = OriUp | OriDown | OriLeft | OriRight deriving Show
data Carrier = Carrier
  { orientation    :: Orientation
  , loc            :: (Int, Int)
  , infectionCount :: Int
  } deriving Show

main :: IO ()
main = do
  input <- getArgs >>= \case
    [filepath] -> readFile filepath
    []         -> getContents
  let grid = Map.fromList $ do
        (y, row ) <- zip [0 ..] (lines input)
        (x, cell) <- zip [0 ..] (strConv Strict row :: [Char])
        let value = if cell == '#' then Infected else Healthy
        pure ((x, y), value)
  let height  = length (lines input)
      width   = Text.length $ lines input !! 0
      center  = (width `div` 2, height `div` 2)
      carrier = Carrier OriUp center 0
  iterate' burst (carrier, grid) !! 10000000 & fst & infectionCount & print

turnLeft OriUp    = OriLeft
turnLeft OriLeft  = OriDown
turnLeft OriDown  = OriRight
turnLeft OriRight = OriUp

turnRight OriUp    = OriRight
turnRight OriRight = OriDown
turnRight OriDown  = OriLeft
turnRight OriLeft  = OriUp

moveForward OriUp    (x, y) = (x, y - 1)
moveForward OriRight (x, y) = (x + 1, y)
moveForward OriDown  (x, y) = (x, y + 1)
moveForward OriLeft  (x, y) = (x - 1, y)

burst :: (Carrier, Grid) -> (Carrier, Grid)
burst (carrier, grid) = ((move . turn . count) carrier, newGrid)
 where
  oldcell = Map.findWithDefault Healthy (loc carrier) grid
  newGrid = Map.insert (loc carrier) (toggle oldcell) grid
  toggle Healthy  = Weakened
  toggle Weakened = Infected
  toggle Infected = Flagged
  toggle Flagged  = Healthy
  count c =
    let modifier = case oldcell of
          Weakened -> (+ 1)
          _        -> identity
    in  c { infectionCount = modifier $ infectionCount c }
  turn c =
    let turn' = case oldcell of
          Healthy  -> turnLeft
          Weakened -> identity
          Infected -> turnRight
          Flagged  -> turnLeft . turnLeft
    in  c { orientation = turn' $ orientation c }
  move c = c { loc = moveForward (orientation c) (loc c) }

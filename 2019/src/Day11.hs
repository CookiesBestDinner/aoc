{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day11 where

import           Common                                   ( executeParser )
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Day09                                    ( Comp(..)
                                                          , pInput
                                                          , run
                                                          )
import           Protolude

data Color
  = B
  | W
  deriving (Eq, Show)

type Panels = Map.Map (Int, Int) Int

data Robot =
  Robot
    { _dir    :: (Int, Int)
    , _loc    :: (Int, Int)
    , _panels :: Panels
    }

makeLenses ''Robot

white :: Int
white = 1

black :: Int
black = 0

-- |
-- >>> readFile "input/day11" >>= main
-- 2160
-- .█....███..████.████..██...██..████.████...
-- .█....█..█....█.█....█..█.█..█.█....█......
-- .█....█..█...█..███..█....█....███..███....
-- .█....███...█...█....█....█.██.█....█......
-- .█....█.█..█....█....█..█.█..█.█....█......
-- .████.█..█.████.████..██...███.█....████...
main :: Text -> IO ()
main indata = do
  program <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] program
  let brain      = Comp 0 0 initialMem
  let robot = Robot (0, 1) (0, 0) (Map.fromList [((0, 0), white)])
  let go startColor =
        let brainOut = run brain (startColor : (fst <$> robotOut))
            robotOut = runRobot robot brainOut
        in  lastDef Map.empty (snd <$> robotOut)
  go black & Map.size & print
  let painted = go white
  let maxY    = painted & Map.keys <&> snd & maximum
  let minY    = painted & Map.keys <&> snd & minimum
  let maxX    = painted & Map.keys <&> fst & maximum
  let minX    = painted & Map.keys <&> fst & minimum
  [ [ if color == white then '█' else '.'
    | x <- [minX .. maxX]
    , let color = Map.findWithDefault black (x, y) painted
    ]
    | y <- [maxY, maxY - 1 .. minY]
    ]
    & mapM_ putStrLn

runRobot :: Robot -> [Int] -> [(Int, Panels)]
runRobot robot (color : turn : rest) =
  (nextLocColor, robot ^. panels) : continue
 where
  paint        = panels . at (robot ^. loc) ?~ color
  steer        = dir .~ (dx, dy)
  drive        = loc .~ nextLocation
  (dx, dy)     = robot ^. dir & if turn == 0 then left else right
  nextLocation = let (x, y) = robot ^. loc in (x + dx, y + dy)
  nextLocColor = Map.findWithDefault black nextLocation (robot ^. panels)
  nextRobot    = robot & paint & steer & drive
  continue     = runRobot nextRobot rest
runRobot _ _ = []

left :: (Int, Int) -> (Int, Int)
left (0 , 1 ) = (-1, 0)
left (-1, 0 ) = (0, -1)
left (0 , -1) = (1, 0)
left (1 , 0 ) = (0, 1)
left _        = panic "woof?"

right :: (Int, Int) -> (Int, Int)
right (-1, 0 ) = (0, 1)
right (0 , -1) = (-1, 0)
right (1 , 0 ) = (0, -1)
right (0 , 1 ) = (1, 0)
right _        = panic "meow?"

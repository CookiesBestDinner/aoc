{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day11
  ( main
  )
where

import           Common                                   ( executeParser )
import           Control.Arrow                            ( (&&&) )
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Day09                                    ( Comp(..)
                                                          , pInput
                                                          , run
                                                          )
import           Protolude

type Panels = Map.Map (Int, Int) Int

data Robot =
  Robot
    { _dir    :: (Int, Int)
    , _loc    :: (Int, Int)
    , _panels :: Panels
    }

makeLenses ''Robot

white, black :: Int
(white, black) = (1, 0)

left, right :: (Int, Int) -> (Int, Int)
right (x, y) = (y, -x)
left (x, y) = (-y, x)

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
  let robot      = Robot (0, 1) (0, 0) Map.empty
  let go startColor =
        let brainOut = run brain (startColor : (fst <$> robotOut))
            robotOut = runRobot robot brainOut
        in  lastDef Map.empty (snd <$> robotOut)
  go black & Map.size & print
  let painted      = go white
  let (minX, maxX) = painted & Map.keys <&> fst & (minimum &&& maximum)
  let (minY, maxY) = painted & Map.keys <&> snd & (minimum &&& maximum)
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

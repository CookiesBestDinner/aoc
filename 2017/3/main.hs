module Derp where

import Prelude hiding (Either(..))

input :: [Integer]
input = [1, 12, 23, 1024, 265149]

data Dir = Up | Left | Down | Right deriving (Eq, Ord, Show, Enum)

main = do
  let n = 265149
      ring = getRing n
      startcoord = getRingStartCoord ring
      width = (ring-1) * 2 + 1
      startVal = width * width + 1
      endCoord = walkUp ring startcoord (howMany Up ring) startVal n
      dist = distanceToMiddle endCoord
  putStrLn $ "n: " ++ show n
  putStrLn $ "ring:" ++ show ring
  putStrLn $ "startcoord:" ++ show startcoord
  putStrLn $ "startval:" ++ show startVal
  print dist

diff :: Dir -> (Integer, Integer)
diff Up = (0, 1)
diff Left = (-1, 0)
diff Right = (1, 0)
diff Down = (0, -1)

getRing :: Integer -> Integer
getRing n = choochoo 1
  where choochoo ring = if ring * ring >= n
                           then ring `div` 2
                           else choochoo (ring+2)

distanceToMiddle :: (Integer, Integer) -> Integer
distanceToMiddle (x, y) = abs x + abs y

howMany :: Dir -> Integer -> Integer
howMany Up ring = 2 * ring - 1
howMany Left ring = ring * 2
howMany Down ring = howMany Left ring
howMany Right ring = howMany Left ring

getRingStartCoord :: Integer -> (Integer, Integer)
getRingStartCoord 0 = (0, 0)
getRingStartCoord 1 = (1, 0)
getRingStartCoord n = let (x, y) = getRingStartCoord (n-1)
                       in (x+1, y-1)

walkUp :: Integer -> (Integer, Integer) -> Integer -> Integer -> Integer -> (Integer, Integer)
walkUp ring from 0 n findme
  | n == findme = from
  | otherwise   = walkLeft ring from (howMany Left ring) n findme

walkUp ring from remain n findme
  | n == findme = from
  | otherwise   = walkUp ring (walk Up from) (remain-1) (n+1) findme


walkLeft :: Integer -> (Integer, Integer) -> Integer -> Integer -> Integer -> (Integer, Integer)
walkLeft ring from 0 n findme
  | n == findme = from
  | otherwise  = walkDown ring from (howMany Down ring) n findme

walkLeft ring from remain n findme
  | n == findme = from
  | otherwise   = walkLeft ring (walk Left from) (remain-1) (n+1) findme


walkDown :: Integer -> (Integer, Integer) -> Integer -> Integer -> Integer -> (Integer, Integer)
walkDown ring from 0 n findme
  | n == findme = from
  | otherwise   = walkRight ring from (howMany Right ring) n findme

walkDown ring from remain n findme
  | n == findme = from
  | otherwise   = walkDown ring (walk Down from) (remain-1) (n+1) findme


walkRight :: Integer -> (Integer, Integer) -> Integer -> Integer -> Integer -> (Integer, Integer)
walkRight ring from 0 n findme
  | n == findme = from
  | otherwise   = error "didn't encounter findme"

walkRight ring from remain n findme
  | n == findme = from
  | otherwise   = walkRight ring (walk Right from) (remain-1) (n+1) findme



walk :: Dir -> (Integer, Integer) -> (Integer, Integer)
walk dir (x, y) = let (dx, dy) = diff dir in
                      (x+dx, y+dy)

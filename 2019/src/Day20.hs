{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day20 where

import           Control.Arrow
import           Data.Char
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Protolude


main :: Text -> IO ()
main input = do
  let (meep, begin, end, outers) = readGrid input
  print $ bfs meep (Set.singleton begin) mempty 0 end
  -- part2
  let (y, x)   = begin
  let (ey, ex) = end
  print $ bfs2 meep outers (Set.singleton (0, y, x)) mempty 0 (0, ey, ex)


bfs2
  :: Map (Int, Int) [(Int, Int)]
  -> Set (Int, Int)
  -> Set (Int, Int, Int)
  -> Set (Int, Int, Int)
  -> Int
  -> (Int, Int, Int)
  -> Int
bfs2 meep outers heres seen steps end
  | end `elem` heres = steps
  | Set.null can_go_to = panic "nothing more to search"
  | otherwise = bfs2 meep
                     outers
                     can_go_to
                     (Set.union seen can_go_to)
                     (steps + 1)
                     end
 where
  can_go_to = Set.fromList $ do
    (z, y, x) <- Set.toList heres
    (ty, tx)  <- meep Map.! (y, x)
    -- it's a portal jump if not adjacent
    let dz | abs (y - ty) + abs (x - tx) == 1 = 0
           | (y, x) `Set.member` outers       = -1
           | otherwise                        = 1
        tz = z + dz
    -- can't go outside the outermost level
    guard $ tz >= 0
    guard $ (tz, ty, tx) `Set.notMember` seen
    pure (tz, ty, tx)


bfs :: (Ord k) => Map k [k] -> Set k -> Set k -> Int -> k -> Int
bfs meep heres seen steps end
  | end `elem` heres = steps
  | otherwise = bfs meep can_go_to (Set.union seen can_go_to) (steps + 1) end
 where
  can_go_to = Set.fromList $ do
    here  <- Set.toList heres
    there <- meep Map.! here
    guard $ there `Set.notMember` seen
    pure there


readGrid
  :: Text
  -> ( Map.Map (Int, Int) [(Int, Int)]
     , (Int, Int)
     , (Int, Int)
     , Set (Int, Int)
     )
readGrid t = (dotsWithPortals, start, end, outers) where
  grid = Map.fromList $ do
    (y, row ) <- zip [0 ..] (lines t)
    (x, cell) <- zip [0 ..] (toS row)
    pure ((y, x), cell)
  dots = Map.fromList $ do
    ((y, x), cell) <- Map.toList grid
    guard (cell == '.')
    pure
      ( (y, x)
      , filter (`Map.member` dots)
               [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
      )
  -- portals can be identified as letters next to dots
  portals :: Map [Char] [(Int, Int)]
  portals = Map.fromListWith (<>) $ do
    ((y, x), cell) <- Map.toList grid
    guard (isAlpha cell)
    let ns = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
    (doty, dotx) <- filter (`Map.member` dots) ns
    -- the other letter is 1 step further in same direction from the dot
    let (dy, dx) = (y - doty, x - dotx)
        (yy, xx) = (y + dy, x + dx)
        -- figure out which letter comes first (reading order -> sort coords)
        (a , b ) = uncurry min &&& uncurry max $ ((y, x), (yy, xx))
        name     = (grid Map.!) <$> [a, b]
    -- the location of the portal is at the dot itself, not the letter
    pure (name, [(doty, dotx)])
  insertPortal dotmap tunnel = case tunnel of
    [a, b] -> Map.adjust (b :) a $ Map.adjust (a :) b dotmap
    _      -> dotmap
  dotsWithPortals = foldl' insertPortal dots portals
  start           = headDef (panic "eek") $ portals Map.! "AA"
  end             = headDef (panic "eek") $ portals Map.! "ZZ"
  height          = length (lines t)
  width           = 1 + (grid & Map.keys <&> snd & maximum)
  -- outer is a dot 3 from the edge in any direction
  is_outer (y, x) = y == 2 || y == height - 3 || x == 2 || x == width - 3
  outers = dots & Map.keys & filter is_outer & Set.fromList

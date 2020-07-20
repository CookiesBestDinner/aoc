{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day24 where

import           Data.Char (isDigit)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Protolude

main :: IO ()
main = do
  input <- getContents
  let
    -- locate walls
    walls = Set.fromList $ do
      (y, ln) <- zip [0 ..] (lines input)
      (x, c ) <- zip [0 ..] (toS ln)
      guard $ c == '#'
      pure (x, y)
    -- locate numbers
    numbers = Map.fromList $ do
      (y, ln) <- zip [0 ..] (lines input)
      (x, n ) <- zip [0 ..] (toS ln)
      guard $ isDigit n
      pure (n, (x, y))
    -- roads describe smallest distance between two points of interests
    roads = Map.fromList $ do
      ((an, (ax, ay)), (bn, (bx, by))) <-
        ordNub $ (,) <$> Map.toList numbers <*> Map.toList numbers
      let dist = distance (Set.singleton (0, 0, ax, ay)) walls (bx, by)
      [((an, bn), dist), ((bn, an), dist)]
    -- all possible routes in terms of the numbers (3 to 7 to 1 to 2)
    possibleRoutes =
      ('0' :) <$> permutations (Map.keys numbers & filter (/= '0'))
    -- fold each route into its distance
    atob (total, curr) next =
      let dist = fromMaybe
            (panic $ toS $ "no path found between " <> [curr, ' ', next])
            (roads Map.! (curr, next))
      in  (total + dist, next)
    routeDist []       = 0
    routeDist (x : xs) = foldl' atob (0, x) xs & fst
  -- pick the smallest, print
  possibleRoutes <&> routeDist & minimum & print
  possibleRoutes <&> (<> "0") <&> routeDist & minimum & print

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | A*
distance
  :: Set (Int, Int, Int, Int) -> Set (Int, Int) -> (Int, Int) -> Maybe Int
distance pq visited target | Set.null pq                         = Nothing
                           | (herex, herey) `Set.member` visited = discard
                           | (herex, herey) == target            = Just dist
                           | otherwise                           = continue
 where
  ((_, dist, herex, herey), pq') = Set.deleteFindMin pq
  neighbours                     = do
    (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
    let (xx, yy) = (herex + dx, herey + dy)
    guard $ (xx, yy) `Set.notMember` visited
    let priority = dist + 1 + manhattan (xx, yy) target
    pure (priority, dist + 1, xx, yy)
  continue = distance (pq' <> Set.fromList neighbours)
                      (Set.insert (herex, herey) visited)
                      target
  discard = distance pq' visited target

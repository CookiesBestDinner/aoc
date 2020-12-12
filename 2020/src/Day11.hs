module Day11 where

-- This is rather slow (3 secs), if it was to be improved, an obvious change
-- would be to replace Data.Set with Data.Vector

import           Data.Ix              (inRange)
import qualified Data.Set             as Set
import           Protolude            hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

data Ferry
  = Ferry
      { taken      :: Set (Int, Int)
      , exist      :: Set (Int, Int)
      , isInBounds :: (Int, Int) -> Bool
      }

parseFerry :: Parser Ferry
parseFerry = do
  let cell = oneOf ("L.#" :: [Char])
      ln   = some cell
  grid <- ln `sepEndBy1` eol
  let width      = length (headDef [] grid)
      height     = length grid
      locations  = flip (,) <$> [0 .. height - 1] <*> [0 .. width - 1]
      places     = zip locations (mconcat grid)
      exist      = places & filter ((/= '.') . snd) <&> fst & Set.fromList
      taken      = places & filter ((== '#') . snd) <&> fst & Set.fromList
      isInBounds = inRange ((0, 0), (width - 1, height - 1))
  pure $ Ferry { .. }

step :: (Ferry -> (Int, Int) -> [(Int, Int)]) -> Int -> Ferry -> Ferry
step around leave f = f { taken = newTaken }
 where
  newTaken = Set.fromList $ do
    here <- exist f & Set.toList
    let occupancy = sum [ 1 | loc <- around f here, loc `Set.member` taken f ]
        isempty     = here `Set.notMember` taken f
    [ here | isempty && occupancy == 0 || not isempty && occupancy < leave ]

around1, around2 :: Ferry -> (Int, Int) -> [(Int, Int)]
around1 _ (x, y) =
  (\dx dy -> (x + dx, y + dy)) <$> [-1 .. 1] <*> [-1 .. 1] & filter (/= (x, y))

around2 f (x, y) = do
  (dx, dy) <- (,) <$> [-1 .. 1] <*> [-1 .. 1] & filter (/= (0, 0))
  (\n -> (x + dx * n, y + dy * n))
    <$> [1 ..]
    &   takeWhile (isInBounds f)
    &   dropWhile (`Set.notMember` exist f)
    &   take 1
    &   filter (`Set.member` taken f)

run :: (Ferry -> (Int, Int) -> [(Int, Int)]) -> Int -> Ferry -> Int
run around n ferry | taken ferry == taken ferry' = ferry & taken & Set.size
                   | otherwise                   = run around n ferry'
  where ferry' = step around n ferry

main :: Text -> IO ()
main input = do
  indata <- parse' parseFerry input
  print $ run around1 4 indata
  print $ run around2 5 indata

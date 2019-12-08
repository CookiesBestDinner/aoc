module Day01 where

import           Data.Function ((&))
import           Data.Functor  ((<&>))
import qualified Data.Text     as Text

-- |
-- >>> readFile "input/day01" >>= (main . Text.pack)
-- 3339288
-- 5006064
main :: Text.Text -> IO ()
main input = do
  let nums = input & Text.unpack & words <&> read
  let mass = nums <&> fuelForMass & sum
  print mass
  nums <&> fuelForFuel & sum & print

fuelForFuel :: Integer -> Integer
fuelForFuel m = extras & takeWhile (> 0) & sum
  where
    extras = fuelForMass m : (fuelForMass <$> extras)

fuelForMass :: Integer -> Integer
fuelForMass m = m `div` 3 & subtract 2

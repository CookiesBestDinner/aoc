module Day01 where

import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
  input <- getContents
  let nums = input & words <&> read
  let mass = nums <&> fuelForMass & sum
  print mass
  nums <&> fuelForFuel & sum & print

fuelForFuel :: Integer -> Integer
fuelForFuel m = extras & takeWhile (> 0) & sum
  where
    extras = fuelForMass m : (fuelForMass <$> extras)

fuelForMass :: Integer -> Integer
fuelForMass m = m `div` 3 & subtract 2

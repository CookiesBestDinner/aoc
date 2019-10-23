module Main where

import           Control.Arrow
import           Data.List.Split
import Data.List

main :: IO ()
main =
  getContents
    >>= (   lines
        >>> map (splitOn "x" >>> map (read :: String -> Integer) >>> (wrap &&& bow))
        >>> map (\(a, b) -> [a, b])
        >>> transpose
        >>> map sum
        >>> print
        )

wrap :: [Integer] -> Integer
wrap [h, w, l] =
  2 * h * w + 2 * w * l + 2 * h * l + minimum [h * w, w * l, l * h]
wrap _ = error "Are you sure that's a box?"

bow :: [Integer] -> Integer
bow box@[h, w, l] =
  minimum [2 * h + 2 * w, 2 * h + 2 * l, 2 * w + 2 * l] + product box
bow _ = error "Are you sure that's a box?"

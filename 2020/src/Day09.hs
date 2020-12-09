module Day09 where

import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Parsing

findInvalid :: [Integer] -> Maybe Integer
findInvalid xs = listToMaybe $ go (take 25 <$> tails xs) (drop 25 xs)
 where
  isSum x prev = or $ (\a b -> a /= b && a + b == x) <$> prev <*> prev
  go (p : rev) (x : xs) | isSum x p = go rev xs
                        | otherwise = x : go rev xs
  go _ _ = []

findSet :: [Integer] -> Integer -> Maybe [Integer]
findSet xs invalid = find ((== invalid) . sum) candidates
 where
  candidates = (takeSum 0 <$> tails xs) & filter ((> 1) . length)
  takeSum total (y : ys) | total < invalid = y : takeSum (total + y) ys
                         | otherwise       = []
  takeSum _ _ = []

main :: Text -> IO ()
main input = do
  indata <- parse' (number `sepEndBy1` space1) input
  let invalid = findInvalid indata
  print invalid
  print $ invalid >>= findSet indata <&> liftA2 (+) minimum maximum

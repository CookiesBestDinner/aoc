import Data.Set

main :: IO ()
main = interact solve

solve :: String -> String
solve s = show answer
 where
  withoutPlus      = Prelude.filter (/= '+') s
  numbers          = Prelude.map read $ lines withoutPlus :: [Integer]
  repeatingNumbers = cycle numbers
  frequencyList    = scanl (+) 0 repeatingNumbers
  answer           = findDuplicate frequencyList


findDuplicate :: [Integer] -> Integer
findDuplicate = go empty
 where
  go _    []     = error "no duplicate"
  go seen (x:xs) = if x `member` seen then x else go (insert x seen) xs

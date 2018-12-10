import Data.Set (member, insert, empty)

main :: IO ()
main = interact solve

solve :: String -> String
solve input = unlines [part1 input, part2 input]

part1 :: String -> String
part1 s = show $ sum numbers
    where withoutPlus = filter (/= '+') s
          numbers = map read $ lines withoutPlus :: [Integer]

part2 :: String -> String
part2 s = show answer
 where
  withoutPlus      = filter (/= '+') s
  numbers          = map read $ lines withoutPlus :: [Integer]
  repeatingNumbers = cycle numbers
  frequencyList    = scanl (+) 0 repeatingNumbers
  answer           = findDuplicate frequencyList


findDuplicate :: [Integer] -> Integer
findDuplicate = go empty
 where
  go _    []     = error "no duplicate"
  go seen (x:xs) = if x `member` seen then x else go (insert x seen) xs

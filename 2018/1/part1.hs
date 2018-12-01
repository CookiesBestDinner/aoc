main :: IO ()
main = interact solve

solve :: String -> String
solve s = show $ sum numbers
    where withoutPlus = filter (/= '+') s
          numbers = map read $ lines withoutPlus :: [Integer]

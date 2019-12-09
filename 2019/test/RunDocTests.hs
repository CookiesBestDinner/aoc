import Test.DocTest

main :: IO ()
main = do
  doctest
    [ "--fast"
    , "src/Day01.hs"
    , "src/Day02.hs"
    , "src/Day03.hs"
    , "src/Day04.hs"
    , "src/Day05.hs"
    , "src/Day06.hs"
    , "src/Day07.hs"
    , "src/Day08.hs"
    , "src/Day09.hs"
    ]

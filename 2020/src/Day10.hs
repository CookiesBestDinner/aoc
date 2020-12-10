module Day10 where

import           Data.Ix
import           Data.List       ((!!))
import           Data.List.Extra (dropEnd)
import           Protolude

import           Parsing

main :: Text -> IO ()
main input = do
  indata <- parse' numbers input
  let sorted  = sort indata
  let indata' = 0 : sorted <> [maximum indata + 3]
  let diffs = abs <$> zipWith (-) indata' (drop 1 indata')
  let ones    = length $ filter (== 1) diffs
  let threes  = length $ filter (== 3) diffs
  putText "adapters (+ends)"
  print indata'
  putText "\ndiffs"
  print diffs
  putText $ "count(1) * count(3) = " <> show (ones * threes)
  -- part 2
  -- observation: cannot skip difference of 3
  --            , there is no diff of 2
  -- so, split on 3s, count combinations for individual runs, multiply
  -- together
  -- the two values creating diff of 3 must always be included
  putText "--------------- part 2"
  putText "cut on the differences of 3"
  print $ splitAtThrees indata'
  let runs = dropEnd 1 . drop 1 <$> splitAtThrees indata'
  let lens          = length <$> runs
  let arrangeCounts = ways <$> lens
  let answer        = product arrangeCounts
  putText "\ndrop the values that create that diff of 3"
  print runs
  putText "\nonly the length is interesting at this point, they all have diff=1"
  print lens
  putText "\nhere's how many ways each run can be arranged"
  print arrangeCounts
  putText $ "...of which the product is " <> show answer

-- welcome to this mess.
-- it might for that matter be wrong seeing as it's only used up to x=3
ways = (([0 ..] <&> \n -> ways' 0 (n + 1) [1 .. n]) !!)
ways' _ _ [] = 1
ways' s e [x] | inRange (s, s + 3) e = 2
              | inRange (s, s + 3) x && inRange (e - 3, e) x = 1
              | otherwise            = 0
ways' s e (a : b : xs)
  | not (inRange (s, s + 3) a) = 0
  | otherwise = sum
    -- keep both
    [ ways' b e xs
    -- drop a, exclude continuing by dropping b
    , ways' s e (b : xs) - ways' s e xs
    -- drop b
    , ways' a e xs
    -- drop both
    , ways' s e xs
    ]

splitAtThrees []  = []
splitAtThrees [x] = [[x]]
splitAtThrees xs  = here : splitAtThrees rest
 where
  here = takeTo3 xs
  rest = drop (length here) xs

takeTo3 (a : b : xs) | b - a == 3 = [a]
                     | otherwise  = a : takeTo3 (b : xs)
takeTo3 xs = xs

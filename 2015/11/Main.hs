module Main where

import           Data.List                                ( nub
                                                          , group
                                                          )
import           Data.Function
import           Control.Arrow
import           Data.Char


main = do
  seed <- filter isLetter <$> getContents
  let part1 = passwords' seed & drop 1 & filter valid & head
  putStrLn part1
  let part2 = passwords' part1 & drop 1 & filter valid & head
  putStrLn part2

passwords' :: String -> [String]
passwords' []          = []
passwords' [ch       ] = return <$> [ch .. 'z']
passwords' (w : heels) = do
  here <- [w .. 'z']
  let wheels = if here == w then heels else 'a' <$ heels
  (here :) <$> passwords' wheels

twoPairs pw =
  pw & group & filter (take 3 >>> length >>> (== 2)) & nub & length & (>= 2)

threeSucc (a : b : c : rest) =
  b == succ a && c == succ b || threeSucc (b : c : rest)
threeSucc _ = False

valid :: String -> Bool
valid = (&&) <$> threeSucc <*> twoPairs

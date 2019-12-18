{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import           Protolude               hiding ( many )
import           Data.List.Extra                ( (!!)
                                                , chunksOf
                                                , iterate'
                                                )
import qualified Data.Map.Lazy               as Map
import           Text.Show.Pretty
import qualified Data.Text as T
import           Control.Arrow
import           Text.Megaparsec
import           Common

fft :: [Int] -> [Int]
fft input = doThing <$> pats
 where
  keepOne :: Int -> Int
  keepOne = (`rem` 10) >>> abs
  len     = length input
  pats    = pattern <$> [0 .. len - 1]
  doThing pat = zipWith (*) input pat & sum & keepOne

pattern :: Num a => Int -> [a]
pattern i = dup
 where
  base = [0, 1, 0, -1]
  dup  = cycle (base >>= replicate (i + 1)) & drop 1

pInput :: Parser [Int]
pInput = many digit
 where
  digit = do
    d <- oneOf ("0123456789" :: [Char])
    return (fromEnum d - fromEnum '0')

-- |
-- >>> :set -XOverloadedStrings
-- >>> main "80871224585914546619083218645595"
-- 24176176
-- >>> main "19617804207202209144916044189917"
-- 73745418
-- >>> main "69317163492948606335995924319873"
-- 52432133

doTimes f 0 x = x
doTimes f n x = doTimes f (n-1) (f x)

main :: Text -> IO ()
main input = do
  nums <- executeParser pInput input
  let digitsToS :: [Int] -> Text
      digitsToS = map show >>> mconcat
  iterate fft nums & (!! 100) & take 8 & digitsToS & putText
  -- let realSignal = nums & replicate 10000 & mconcat
  -- let len     = length realSignal
  -- let pats    = pattern <$> [0 .. len - 1]
  -- let msgOffset = nums & take 7 & digitsToS & T.unpack & readNum
  -- let theThing = doTimes fft 1 realSignal
  -- print "hi?"
  -- realSignal & drop msgOffset & take 8 <&> show & mconcat & putText
  -- print "1"
  -- theThing & drop msgOffset & take 8 <&> show & mconcat & putText
  -- print "2"

readNum :: [Char] -> Int
readNum s = i
  where Just i = readMaybe s

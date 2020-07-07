{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day21 where

import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = do
  indata <- readFile "inputs/21"
  instr  <- case parse (parseIns `sepEndBy` space <* eof) "" indata of
    Left  boo -> putStr (errorBundlePretty boo) >> exitFailure
    Right yay -> pure yay
  let part1 = foldl' (&) (V.fromList "abcdefgh") instr
  print part1
  instr2 <- case parse (parseIns2 `sepEndBy` space <* eof) "" indata of
    Left  boo -> putStr (errorBundlePretty boo) >> exitFailure
    Right yay -> pure yay
  print $ foldl' (&) (V.fromList "fbgdceah") (reverse instr2)

d :: ParsecT Void Text Identity Int
d = decimal

parseIns :: Parsec Void Text (Pw -> Pw)
parseIns = choice
  [ swapAB <$ "swap position " <*> d <* " with position " <*> d
  , move <$ "move position " <*> d <* " to position " <*> d
  , reversePos <$ "reverse positions " <*> d <* " through " <*> d
  , rotateBasedOn <$ "rotate based on position of letter " <*> anySingle
  , rotateLeft <$ "rotate left " <*> d <* (" steps" <|> " step")
  , rotateRight <$ "rotate right " <*> d <* (" steps" <|> " step")
  , swapCH <$ "swap letter " <*> anySingle <* " with letter " <*> anySingle
  ]

parseIns2 :: Parsec Void Text (Pw -> Pw)
parseIns2 = choice
  [ swapAB <$ "swap position " <*> d <* " with position " <*> d
  , flip move <$ "move position " <*> d <* " to position " <*> d
  , reversePos <$ "reverse positions " <*> d <* " through " <*> d
  , swapCH <$ "swap letter " <*> anySingle <* " with letter " <*> anySingle
  , rotateBasedOn2 <$ "rotate based on position of letter " <*> anySingle
  , rotateRight <$ "rotate left " <*> d <* (" steps" <|> " step")
  , rotateLeft <$ "rotate right " <*> d <* (" steps" <|> " step")
  ]

type Pw = V.Vector Char

rotateBasedOn2 :: Char -> Pw -> Pw
rotateBasedOn2 ch pw = rotateLeft delta pw where
  locs       = [0 ..] & take (V.length pw)
  deltas     = ([1, 2, 3, 4] <> [6 ..]) & take (V.length pw)
  endsUpAt   = zipWith (+) locs deltas <&> (`mod` V.length pw)
  Just isAt  = V.findIndex (== ch) pw
  Just delta = head $ do
    (loc, steps) <- zip endsUpAt deltas
    guard $ isAt == loc
    pure (steps `mod` V.length pw)

swapAB :: Int -> Int -> Pw -> Pw
swapAB i j pw = runST $ do
  cp <- V.thaw pw
  VM.write cp i $ pw V.! j
  VM.write cp j $ pw V.! i
  V.freeze cp

swapCH :: Char -> Char -> Pw -> Pw
swapCH a b = V.map
  (\ch -> if
    | ch == a   -> b
    | ch == b   -> a
    | otherwise -> ch
  )

rotateLeft :: Int -> Pw -> Pw
rotateLeft n pw = V.drop n pw <> V.take n pw

rotateRight :: Int -> Pw -> Pw
rotateRight n pw = rotateLeft (negate n `mod` V.length pw) pw

rotateBasedOn :: Char -> Pw -> Pw
rotateBasedOn ch pw =
  let Just loc = V.findIndex (== ch) pw
      n        = (1 + loc + if loc >= 4 then 1 else 0) `mod` V.length pw
  in  rotateRight n pw

reversePos :: Int -> Int -> Pw -> Pw
reversePos a b pw =
  V.take a pw <> V.reverse (V.slice a (b - a + 1) pw) <> V.drop (b + 1) pw

move :: Int -> Int -> Pw -> Pw
move a b pw =
  let cp = V.take a pw <> V.drop (a + 1) pw
  in  V.take b cp <> V.singleton (pw V.! a) <> V.drop b cp

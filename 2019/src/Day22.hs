{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day22 where

import           Common
import           Data.List                  (elemIndex)
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

-- this is only part 1
-- I'm guessing part 2 would be done in reverse order, tracking only how
-- position 2020 moves and that there would be some predictable pattern to how
-- it moves which could be scaled up to the number of times the deck is
-- supposed to be shuffled

main :: Text -> IO ()
main indata = do
  instr <- executeParser pInput indata
  print $ elemIndex 2019 $ run instr

signedDecimal :: Parser Int
signedDecimal = Lex.signed (pure ()) Lex.decimal

data Instr
  = Cut Int
  | DealWith Int
  | Reverse
  deriving Show

pInput :: Parser [Instr]
pInput = pInstr `sepEndBy1` space where
  pCut   = "cut " >> Cut <$> signedDecimal
  pRev   = "deal into new stack" $> Reverse
  pDeal  = "deal with increment " >> DealWith <$> signedDecimal
  pInstr = choice [pCut, pRev, pDeal]

shuffle :: Ord a => Instr -> [a] -> [a]
shuffle Reverse xs = reverse xs
shuffle (Cut n) xs = let n' = n `mod` length xs in drop n' xs <> take n' xs
shuffle (DealWith n) xs =
  let indices = [0, n ..] & take (length xs) <&> (`mod` length xs)
      pairs   = zip indices xs
  in  sort pairs <&> snd

run :: [Instr] -> [Int]
run = foldl' (&) [0 .. 10006] . map shuffle

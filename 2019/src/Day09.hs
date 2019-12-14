{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day09 where

import           Common

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Protolude                         hiding ( zero )
import           Text.Megaparsec
import           Text.Megaparsec.Char                     ( space )
import           Text.Megaparsec.Char.Lexer               ( decimal
                                                          , signed
                                                          )

data Comp =
  Comp
    { _pc  :: !Int
    , _param :: !Int
    , _mem :: !(Map.Map Int Int)
    }
  deriving (Show)

makeLenses ''Comp

pInput :: Parser [Int]
pInput = signed (pure ()) decimal `sepBy1` "," <* space

-- |
-- >>> readFile "input/day09" >>= main
-- [2594708277]
-- [87721]
main :: Text -> IO ()
main indata = do
  program <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] program
  run (Comp 0 0 initialMem) [1] & print
  run (Comp 0 0 initialMem) [2] & print

decodeOp :: Int -> (Int, Int, Int, Int)
decodeOp i =
  ( i `mod` 100
  , (i `div` 100) `mod` 10
  , (i `div` 1000) `mod` 10
  , (i `div` 10000) `mod` 10
  )

run :: Comp -> [Int] -> [Int]
run comp input | i == 99   = []
               | i == 4    = ra : continue
               | otherwise = continue
 where
  continue = run updated nxtInput
  nxtInput | i == 3    = drop 1 input
           | otherwise = input
  updated  = comp & case i of
    1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
    2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
    3 -> pc %~ (+ 2) >>> mem . at a ?~ input ^?! ix 0
    4 -> pc %~ (+ 2)
    5 -> pc .~ if ra /= 0 then rb else p + 3
    6 -> pc .~ if ra == 0 then rb else p + 3
    7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
    8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
    9 -> pc %~ (+ 2) >>> param %~ (+ ra)
    _ -> panic "heeeeelp"
  lookup loc = fromMaybe 0 (comp ^? (mem . ix loc))
  (i, ma, mb, mc) = decodeOp (lookup p)
  p = comp ^. pc
  a = if ma == 2 then (comp ^. param) + lookup (p + 1) else lookup (p + 1)
  b = if mb == 2 then (comp ^. param) + lookup (p + 2) else lookup (p + 2)
  c = if mc == 2 then (comp ^. param) + lookup (p + 3) else lookup (p + 3)
  ra = if ma == 1 then a else lookup a
  rb = if mb == 1 then b else lookup b

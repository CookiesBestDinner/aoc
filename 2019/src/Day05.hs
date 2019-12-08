{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day05
  ( main
  )
where

import           Common

import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char                     ( space )
import           Text.Megaparsec.Char.Lexer               ( decimal
                                                          , signed
                                                          )

data Comp =
  Comp
    { _pc     :: Int
    , _mem    :: Map.Map Int Int
    , _input  :: [Int]
    , _output :: [Int]
    }
  deriving (Show)

makeLenses ''Comp

pInput :: Parser [Int]
pInput = signed (pure ()) decimal `sepBy1` "," <* space

-- |
-- >>> readFile "input/day05" >>= main
-- [7566643,0,0,0,0,0,0,0,0,0]
-- [9265694]
main :: Text -> IO ()
main indata = do
  xs <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] xs
  run (Comp 0 initialMem [1] []) & _output & print
  run (Comp 0 initialMem [5] []) & _output & print

run :: Comp -> Comp
run c = case step c of
  Just c' -> run c'
  Nothing -> c

decodeOp :: Int -> (Int, Int, Int, Int)
decodeOp i =
  ( i `mod` 100
  , (i `div` 100) `mod` 10
  , (i `div` 1000) `mod` 10
  , (i `div` 10000) `mod` 10
  )

step :: Comp -> Maybe Comp
step comp = (if i == 99 then Nothing else Just comp) <&> case i of
  1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
  2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
  7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
  8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
  3 -> pc %~ (+ 2) >>> mem . at a ?~ comp ^?! input . ix 0 >>> input %~ drop 1
  4 -> pc %~ (+ 2) >>> output %~ (ra :)
  5 -> pc .~ if ra /= 0 then rb else p + 3
  6 -> pc .~ if ra == 0 then rb else p + 3
  _ -> panic $ "bad instruction: " <> show i
 where
  lookup loc = comp ^?! (mem . ix loc)
  (i, ma, mb, mc) = decodeOp (lookup p)
  p               = comp ^. pc
  a               = lookup (p + 1)
  b               = lookup (p + 2)
  c               = lookup (p + 3)
  ra              = if ma == 0 then lookup a else a
  rb              = if mb == 0 then lookup b else b
  _rc             = if mc == 0 then lookup c else c

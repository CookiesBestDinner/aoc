{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05
  ( main
  )
where

import           Common

import qualified Data.Map.Strict               as Map
import           Prelude                                  ( head
                                                          , tail
                                                          )
import           Protolude                         hiding ( head )
import           Text.Megaparsec
import           Text.Megaparsec.Char.Lexer

pInput :: Parser [Int]
pInput = (signed (pure ()) decimal) `sepBy1` ","

main :: Text -> IO ()
main indata = do
  xs <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] xs
  run (Comp 0 (initialMem) [1] []) & _output & print
  run (Comp 0 (initialMem) [5] []) & _output & print

data Comp =
  Comp
    { pc      :: Int
    , mem     :: Map.Map Int Int
    , _input  :: [Int]
    , _output :: [Int]
    }
  deriving (Show)

run :: Comp -> Comp
run c = case step c of
  Right c' -> run c'
  Left  c' -> c'

decodeOp :: Int -> (Int, Int, Int, Int)
decodeOp i = (10 * tens + ones, huns, thou, tenk)
 where
  ones = i `mod` 10
  tens = (i `div` 10) `mod` 10
  huns = (i `div` 100) `mod` 10
  thou = (i `div` 1000) `mod` 10
  tenk = (i `div` 10000) `mod` 10

step :: Comp -> Either Comp Comp
step x@(Comp p m input output) = case i of
  1  -> Right $ Comp (p + 4) (Map.insert c (ra + rb) m) input output
  2  -> Right $ Comp (p + 4) (Map.insert c (ra * rb) m) input output
  3  -> Right $ Comp (p + 2) (Map.insert a (head input) m) (tail input) output
  4  -> Right $ Comp (p + 2) m input (ra : output)
  5  -> Right $ Comp (if ra /= 0 then rb else p + 3) m input output
  6  -> Right $ Comp (if ra == 0 then rb else p + 3) m input output
  7  -> Right $ Comp (p + 4) (Map.insert c (fromEnum $ ra < rb) m) input output
  8  -> Right $ Comp (p + 4) (Map.insert c (fromEnum $ ra == rb) m) input output
  99 -> Left x
  _  -> panic $ "bad instruction: " <> show i
 where
  (i, ma, mb, mc) = decodeOp (m Map.! p)
  a               = m Map.! (p + 1)
  b               = m Map.! (p + 2)
  c               = m Map.! (p + 3)
  ra  = if ma == 0 then m Map.! a else a
  rb  = if mb == 0 then m Map.! b else b
  _rc = if mc == 0 then m Map.! c else c

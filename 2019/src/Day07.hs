{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day07
  ( main
  )
where

import           Common

import           Control.Arrow
import           Control.Lens
import           Data.List                                ( (!!) )
import qualified Data.Map                      as MapL
import qualified Data.Map.Strict               as Map
import           Protolude                         hiding ( zero )
import           Text.Megaparsec
import           Text.Megaparsec.Char                     ( space )
import           Text.Megaparsec.Char.Lexer               ( decimal
                                                          , signed
                                                          )

data Comp =
  Comp
    { _pc  :: Int
    , _mem :: Map.Map Int Int
    }
  deriving (Show)

makeLenses ''Comp

pInput :: Parser [Int]
pInput = signed (pure ()) decimal `sepBy1` "," <* space

-- |
-- >>> readFile "input/day07" >>= main
-- Just 116680
-- Just 89603079
main :: Text -> IO ()
main indata = do
  program <- executeParser pInput indata
  let initialMem = Map.fromList $ zip [0 ..] program
  let go phasers =
        phasers
          &   permutations
          <&> makeRig (Comp 0 initialMem)
          <&> (Map.! 4)
          <&> lastMay
          &   maximum
          &   print
  go [0 .. 4]
  go [5 .. 9]

makeRig :: Comp -> [Int] -> Map Int [Int]
makeRig zero inputs = pipes
 where
  pipes = MapL.fromList $ do
    id <- zipWith const [0 ..] inputs
    let phaser = inputs !! id
    let input = phaser : case id of
          0 -> 0 : pipes Map.! (length inputs - 1)
          _ -> pipes Map.! (id - 1)
    pure (id, run zero input)

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
  updated  = comp & case i of
    1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
    2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
    3 -> pc %~ (+ 2) >>> mem . at a ?~ input ^?! ix 0
    4 -> pc %~ (+ 2)
    5 -> pc .~ if ra /= 0 then rb else p + 3
    6 -> pc .~ if ra == 0 then rb else p + 3
    7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
    8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
    _ -> panic "heeeeelp"
  nxtInput | i == 3    = drop 1 input
           | otherwise = input
  lookup loc = comp ^?! (mem . ix loc)
  (i, ma, mb, mc) = decodeOp (lookup p)
  p               = comp ^. pc
  a               = lookup (p + 1)
  b               = lookup (p + 2)
  c               = lookup (p + 3)
  ra              = if ma == 0 then lookup a else a
  rb              = if mb == 0 then lookup b else b
  _rc             = if mc == 0 then lookup c else c

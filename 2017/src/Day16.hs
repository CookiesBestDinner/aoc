{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day16 where

import           Control.Arrow
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import           Prelude                                  ( error )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Move
  = Exchange Int Int
  | Partner Char Char
  | Spin Int
  deriving (Show)

type Dance = Seq.Seq Char

runMove :: Move -> Dance -> Dance
runMove (Spin n) dance =
  (Seq.drop (Seq.length dance - n) dance)
    <> (Seq.take (Seq.length dance - n) dance)
runMove (Exchange a b) dance =
  let a' = dance Seq.!? b & fromMaybe undefined
      b' = dance Seq.!? a & fromMaybe undefined
      l' = dance & Seq.adjust (const a') a & Seq.adjust (const b') b
  in  l'
runMove (Partner a b) dance = runMove (Exchange pa pb) dance
 where
  l  = Seq.length dance
  -- idk how to get rid of this. it SHOULD crash if it comes up as Nothing.
  pa = Seq.elemIndexL a dance & fromMaybe undefined & subtract l & (`mod` l)
  pb = Seq.elemIndexL b dance & fromMaybe undefined & subtract l & (`mod` l)

pInstruction :: Parser Move
pInstruction = choice [pPartner, pExchange, pSpin]
 where
  pProgram  = oneOf ['a' .. 'p']
  pPartner  = Partner <$ "p" <*> pProgram <* "/" <*> pProgram
  pExchange = Exchange <$ "x" <*> decimal <* "/" <*> decimal
  pSpin     = Spin <$ "s" <*> decimal

cycleN :: Int -> [a] -> [a]
cycleN n = replicate n >>> concat

cycleSize :: [(Int, [Char])] -> Int
cycleSize = go Map.empty
 where
  go !seen ((i, s) : r) =
    if s `Map.member` seen then i else go (Map.insert s i seen) r
  go _ _ = error "didn't think that far"

main :: IO ()
main = do
  input        <- getContents
  instructions <-
    case parse (pInstruction `sepBy` "," <* newline <* eof) "" input of
      Left boo -> do
        putStr $ errorBundlePretty boo
        exitFailure
      Right yay -> return yay
  let begin  = Seq.fromList ['a' .. 'p']
      events = runMove <$> instructions
  print $ foldl' (&) begin events
  -- the full dance as a function
  let doADance s = foldl' (&) s events
  -- like a crazy person, do it over and over and over and over
  let states = zip [0 ..] ((iterate doADance begin) <&> foldr (:) [])
  -- but wait, does it have a cycle to its states?
  let cs        = cycleSize states
  -- remove full cycles from the total amount
  let minidance = runMove <$> (cycleN (1_000_000_000 `mod` cs) instructions)
  -- ... and let's see where it ends up.
  print $ foldl' (&) begin minidance
  -- (already did something like this with the 1D cellular automata from 2018)
  -- there was something repeating such silly amounts that one had to find the
  -- period and ... do this

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

pLayer :: Parser (Int, Int)
pLayer = do
  depth <- decimal
  ": "
  range <- decimal
  return (depth, range)

pInput :: Parser [(Int, Int)]
pInput = pLayer `sepEndBy1` newline

readInput :: IO [(Int, Int)]
readInput = do
  inputText <- getContents
  case parse pInput "" inputText of
    Right yay -> return yay
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitFailure

data ScannerHit =
  ScannerHit
    { shTime  :: Int
    , shDepth :: Int
    , shPos   :: Int
    }
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

getCycle :: Int -> [Int]
getCycle r =
  let range = [0 .. r - 1]
      descent = range
      ascent = reverse (drop 1 range) & drop 1
   in descent <> ascent

main :: IO ()
main = do
  input <- readInput
  let periods = Map.fromList [(d, length (getCycle r)) | (d, r) <- input]
  -- build table of all possible situations to get scanned
  let hits =
        Set.fromList
          [ ScannerHit
            {shTime = t `mod` periods Map.! d, shDepth = d, shPos = p}
          | (d, r) <- input
          , (t, p) <- zip [0 ..] (getCycle r)
          ]
  -- iterate through situations that do happen, filter by being a hit
  let gotCaught delay =
        [ depth * range
        | (depth, range) <- input
        , let t = depth + delay
        , let situation =
                ScannerHit
                  { shTime = t `mod` (periods Map.! depth)
                  , shDepth = depth
                  , shPos = 0
                  }
        , situation `Set.member` hits
        ]
  putText "Part1:"
  print $ sum $ gotCaught 0
  let noCatchDelays = [0 ..] & filter (null . gotCaught)
  putText "Part2:"
  print $ take 1 noCatchDelays

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Day06
  ( main
  )
where

import           Common

import           Data.List                                ( (!!) )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char

pOrbit :: Parser (Text, Text)
pOrbit = do
  a <- takeWhile1P Nothing (/= ')')
  ")"
  b <- takeWhile1P Nothing (/= '\n')
  return (a, b)

pOrbits :: Parser [(Text, Text)]
pOrbits = pOrbit `sepEndBy1` eol


-- |
-- >>> readFile "input/day06" >>= main
-- 402879
-- Just 484
main :: Text -> IO ()
main indata = do
  input <- executeParser pOrbits indata
  let innerToOuter = Map.fromListWith (<>) [ (f, [t]) | (f, t) <- input ]
  print $ countthings "COM" innerToOuter 0
  let myObj    = [ obj | (obj, sat) <- input, sat == "YOU" ] !! 0
  let santaObj = [ obj | (obj, sat) <- input, sat == "SAN" ] !! 0
  let connections = Map.fromListWith
        (<>)
        (Map.toList innerToOuter <> [ (b, [a]) | (a, b) <- input ])
  print $ findSanta myObj santaObj connections (Set.fromList [myObj])

countthings :: Text -> Map.Map Text [Text] -> Int -> Int
countthings c m n
  | null satelites = n
  | otherwise      = n + sum [ countthings sat m (n + 1) | sat <- satelites ]
  where satelites = Map.findWithDefault [] c m

findSanta :: Text -> Text -> Map.Map Text [Text] -> Set.Set Text -> Maybe Int
findSanta f t conns seen | f == t          = Just 0
                         | null candidates = Nothing
                         | otherwise       = Just $ 1 + minimum candidates
 where
  candidates = catMaybes
    [ findSanta h t conns (Set.insert h seen)
    | h <- Map.findWithDefault [] f conns
    , h `Set.notMember` seen
    ]

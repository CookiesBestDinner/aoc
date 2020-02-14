{-# LANGUAGE RecordWildCards #-}

module Day4 where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List

data RoomEntry =
  RoomEntry
    { letters  :: String
    , sectorId :: Int
    , isReal   :: Bool
    }
  deriving (Show)

rotate 'z' = 'a'
rotate ch  = succ ch

rotateN n ch = iterate' rotate ch !! n

decipher room = (unwords realWords, sectorId room)
 where
  text      = letters room & map (\ch -> if ch == '-' then ' ' else ch)
  rotDist   = sectorId room
  realWords = map (rotateN rotDist) <$> words text

calcCheckSum :: String -> String
calcCheckSum =
  filter isLetter
    >>> sort
    >>> group
    >>> sortOn (length &&& (head >>> fromEnum >>> negate))
    >>> reverse
    >>> map head
    >>> take 5

parseEntry s = RoomEntry { .. }
 where
  letters  = takeWhile (not . isNumber) s & init
  rest     = dropWhile (not . isNumber) s
  sectorId = read $ takeWhile isNumber rest
  checkSum = dropWhile isNumber rest & tail & init
  isReal   = calcCheckSum letters == checkSum

main = do
  input <- getContents
  let rows      = lines input
      rooms     = parseEntry <$> rows
      realRooms = filter isReal rooms
      ids       = sectorId <$> realRooms
  print $ sum ids
  print $ map decipher realRooms & filter (\(t, _) -> "north" `isInfixOf` t)

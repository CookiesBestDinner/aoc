{-# LANGUAGE NamedFieldPuns #-}
module Day12 where

import           Data.List                  (iterate', (!!))
import qualified Data.Text                  as T
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

data Dir = West | South | North | East deriving (Show)
data Comp
  = Comp
      { posx  :: Int
      , posy  :: Int
      , face  :: Dir
      , shipx :: Int
      , shipy :: Int
      }
  deriving (Show)

data ExitReason = Terminates | Loops deriving (Show)

pIns :: Parser [(Char, Int)]
pIns =
  let ln =
        (,)
          <$> choice (map T.head <$> ["N", "S", "E", "W", "L", "R", "F"])
          <*> decimal
  in  (ln `sepEndBy1` eol) <* eof

facex (face -> East) = 1
facex (face -> West) = -1
facex _              = 0
facey (face -> North) = 1
facey (face -> South) = -1
facey _               = 0

right c@Comp { face } = c { face = face' }
 where
  face' = case face of
    North -> East
    East  -> South
    South -> West
    West  -> North

step comp (c, n) = case c of
  'N' -> comp { posy = posy comp + n }
  'S' -> comp { posy = posy comp - n }
  'E' -> comp { posx = posx comp + n }
  'W' -> comp { posx = posx comp - n }
  'R' -> iterate' right comp !! (n `div` 90)
  'L' -> iterate' (`step` ('R', n)) comp !! 3
  'F' -> comp { posx = posx comp + n * facex comp
              , posy = posy comp + n * facey comp
              }

step2 comp (c, n) = case c of
  'N' -> comp { posy = posy comp + n }
  'S' -> comp { posy = posy comp - n }
  'E' -> comp { posx = posx comp + n }
  'W' -> comp { posx = posx comp - n }
  'R' -> if n == 0
    then comp
    else step2 (comp { posx = posy comp, posy = posx comp * (-1) }) (c, n - 90)
  'L' -> iterate' (`step2` ('R', n)) comp !! 3
  'F' -> comp { shipx = shipx comp + n * posx comp
              , shipy = shipy comp + n * posy comp
              }


main :: Text -> IO ()
main input = do
  instr <- parse' pIns input
  let final1 = foldl' step (Comp 0 0 East 0 0) instr
  print $ abs (posy final1) + abs (posx final1)
  let final2 = foldl' step2 (Comp 10 1 East 0 0) instr
  print $ abs (shipy final2) + abs (shipx final2)

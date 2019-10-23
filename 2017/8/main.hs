{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Map (Map)
import Data.Text (Text)
import Prelude hiding (compare)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  input <- TIO.getContents
  let operations = case parseOnly (parseOp `sepBy` endOfLine) input of
        Right ops -> ops
        Left  err -> error err
      result = scanl (flip id) Map.empty operations
      largestValue mem | Map.null mem = 0
                       | otherwise    = maximum $ Map.elems $ mem
      part1 = largestValue $ last result
      part2 = maximum $ map largestValue result
  print part1
  print part2

operation
  :: String
  -> Text
  -> Int
  -> String
  -> Text
  -> Int
  -> Map String Int
  -> Map String Int
operation register what howMuch condreg condop condval registers =
  if compare (Map.findWithDefault 0 condreg registers) condval
    then Map.insert register (op current) registers
    else registers
 where
  current = Map.findWithDefault 0 register registers
  op      = case what of
    "dec" -> (subtract howMuch)
    "inc" -> (+ howMuch)
    _     -> error "no such operation"
  compare = case condop of
    ">"  -> (>)
    "<"  -> (<)
    ">=" -> (>=)
    "<=" -> (<=)
    "==" -> (==)
    "!=" -> (/=)
    badop -> error $ mappend "unknown comparison: " $ Text.unpack badop

parseOp :: Parser (Map String Int -> Map String Int)
parseOp =
  operation
    <$> many1 letter  -- register
    <*  " "
    <*> choice ["inc", "dec"]
    <*  " "
    <*> signed decimal
    <*  " if "
    <*> many1 letter  -- cond register
    <*  " "
    <*> choice ["<=", ">=", ">", "<", "!=", "=="]
    <*  " "
    <*> signed decimal

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative                      ( (<|>) )
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Functor                             ( ($>) )
import qualified Data.Map                      as Map
import qualified Data.Text.IO                  as TIO
import           Data.Word                                ( Word16 )

data Value
  = Literal Word16
  | Cell String
  deriving (Show)

data Source
  = AND Value Value
  | OR Value Value
  | RSHIFT Value Value
  | LSHIFT Value Value
  | NOT Value
  | VALUE Value
  deriving (Show)

data Component =
  Component String Source
  deriving (Show)

evalValue :: Map.Map String Word16 -> Value -> Word16
evalValue _     (Literal x) = x
evalValue board (Cell    c) = board Map.! c

evalSource board (VALUE a) = evalValue board a
evalSource board (NOT   a) = complement $ evalValue board a
evalSource board (AND a b) = evalValue board a .&. evalValue board b
evalSource board (OR  a b) = evalValue board a .|. evalValue board b
evalSource board (RSHIFT a b) =
  evalValue board a `shiftR` fromEnum (evalValue board b)
evalSource board (LSHIFT a b) =
  evalValue board a `shiftL` fromEnum (evalValue board b)

main = do
  input <- TIO.getContents
  let Right eComps = parseOnly pComponents input
  print eComps
  let circuitBoard = Map.fromList
        [ (key, value)
        | Component key source <- eComps
        , let value = evalSource circuitBoard source
        ]
  -- Part 1: What value does wire a have?
  let a = circuitBoard Map.! "a"
  print a
  -- Part 2: Override wire b with a's value, what value does a get now?
  -- simply replace the instruction for wire b to set it to the old a
  -- then run it again.
  let removeB    = filter (\(Component name _) -> name /= "b") eComps
      setBtoOldA = Component "b" (VALUE (Literal a)) : removeB
  let alteredBoard = Map.fromList
        [ (key, value)
        | Component key source <- setBtoOldA
        , let value = evalSource alteredBoard source
        ]
  print $ alteredBoard Map.! "a"

pCell = do
  name <- many1 letter
  return $ Cell name

pLiteral = do
  num <- decimal
  return $ Literal num

pValue :: Parser Value
pValue = pLiteral <|> pCell

pUnaryGate = do
  gate <- ("NOT " $> NOT) <|> ("" $> VALUE)
  a    <- pValue
  return $ gate a

pBinGate :: Parser Source
pBinGate = do
  a    <- pValue
  _    <- " "
  gate <- choice
    [("AND" $> AND), ("OR" $> OR), ("RSHIFT" $> RSHIFT), ("LSHIFT" $> LSHIFT)]
  _ <- " "
  b <- pValue
  return $ gate a b

pComponent = do
  source   <- pUnaryGate <|> pBinGate <|> (VALUE <$> pValue)
  _        <- " -> "
  wireName <- many1 letter
  return $ Component wireName source

pComponents :: Parser [Component]
pComponents = pComponent `sepBy` endOfLine

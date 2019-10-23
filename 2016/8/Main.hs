{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Protolude                         hiding ( rotate
                                                          , many
                                                          )
import qualified Data.Map.Strict               as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Display = Map (Int, Int) Light

data Light = On | Off deriving (Show, Eq)

data RowCol = Row | Col deriving Show

data Instruction
  = Rect Int Int    -- ^ width, height
  | RotRow Int Int  -- ^ row, dist
  | RotCol Int Int  -- ^ col, dist
  | Rotate RowCol Int Int  -- ^ col, dist
  deriving Show

type Parser = Parsec Void Text

-- rect 1x1
pRect :: Parser Instruction
pRect = do
  _ <- "rect "
  a <- decimal
  _ <- "x"
  b <- decimal
  pure $ Rect a b

-- rotate row y=0 by 20
-- rotate column x=0 by 1
pRotate :: Parser Instruction
pRotate = do
  _        <- "rotate "
  rowcol   <- ("row" $> Row) <|> (Col <$ "column")
  _        <- " y=" <|> " x="
  location <- decimal
  _        <- " by "
  amount   <- decimal
  pure $ Rotate rowcol location amount

pInstructions :: Parser [Instruction]
pInstructions = many ((pRect <|> pRotate) <* newline) <* eof

rotate n xs = xs & cycle & drop steps & take len
 where
  len   = length xs
  steps = negate n `mod` len

initialDisplay = Map.fromList $ do
  x <- [0 .. xBound]
  y <- [0 .. yBound]
  pure ((x, y), Off)

applyInstruction display (Rect width height) = Map.union updates display
 where
  updates = Map.fromList $ do
    x <- [0 .. width - 1]
    y <- [0 .. height - 1]
    pure ((x, y), On)

applyInstruction display (Rotate rowcol loc dist) = Map.union updates display
 where
  assocs = do
    x <- case rowcol of
      Row -> [0 .. xBound]
      Col -> [loc]
    y <- case rowcol of
      Row -> [loc]
      Col -> [0 .. yBound]
    let key = (x, y)
    pure (key, display Map.! key)
  keys          = fst <$> assocs
  values        = snd <$> assocs
  rotatedValues = rotate dist values
  rotatedAssocs = zip keys rotatedValues
  updates       = Map.fromList rotatedAssocs

showDisplay display = concat $ do
  y <- [0 .. yBound]
  let row = do
        x <- [0 .. xBound]
        let val = display Map.! (x, y)
            ch  = case val of
              On  -> '#'
              Off -> '.'
        pure ch
  pure $ row <> "\n"

xBound = 49
yBound = 5
-- xBound = 6
-- yBound = 2

main = do
  input        <- getContents
  instructions <- case parse pInstructions "" input of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      exitFailure
    Right instr -> pure instr
  let outcome = foldl' applyInstruction initialDisplay instructions
  let states  = scanl' applyInstruction initialDisplay instructions
  mapM_ putStr (showDisplay <$> states)
  let lit = Map.elems outcome & filter (== On) & length
  print lit

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day25 where

import           Control.Arrow
import           Data.IntSet                as ISet
import           Data.List                  (iterate', (!!))
import qualified Data.Map.Strict            as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data TuringState
  = TuringState
      { name      :: !Char
      , write0    :: !Bool
      , move0     :: !Int
      , continue0 :: !Char
      , write1    :: !Bool
      , move1     :: !Int
      , continue1 :: !Char
      }
  deriving (Show)
data Program
  = Program
      { pos          :: !Int
      , remain       :: !Int
      , currentState :: !Char
      , tape         :: !IntSet
      , states       :: !(Map Char TuringState)
      }
  deriving (Show)

nameParser :: Parser Char
nameParser = anySingle

stateParser :: Parser TuringState
stateParser = do
  name <- "In state " *> nameParser <* ":" <* space
  "If the current value is 0:" >> space
  "- Write the value "
  write0 <- ("1" $> True <|> "0" $> False) <* "." <* space
  "- Move one slot to the "
  move0     <- ("right" $> 1 <|> "left" $> -1) <* "." <* space
  continue0 <- "- Continue with state " *> nameParser <* "." <* space
  "If the current value is 1:" >> space
  "- Write the value "
  write1 <- ("1" $> True <|> "0" $> False) <* "." <* space
  "- Move one slot to the "
  move1     <- ("right" $> 1 <|> "left" $> -1) <* "." <* space
  continue1 <- "- Continue with state " *> nameParser <* "." <* space
  pure $ TuringState { .. }

programParser :: Parser Program
programParser = do
  currentState <- "Begin in state " *> nameParser <* "." <* space
  "Perform a diagnostic checksum after "
  remain <- decimal
  space >> "steps." >> space
  states' <- stateParser `sepEndBy1` space
  eof
  let pos    = 0
      states = Map.fromList $ (name &&& identity) <$> states'
      tape   = mempty
  pure $ Program { .. }

step :: Program -> Program
step p
  | remain p == 0 = p
  | otherwise = p { pos          = newpos
                  , remain       = remain p - 1
                  , tape         = newtape
                  , currentState = newstate
                  }
 where
  is0 = ISet.notMember (pos p) (tape p)
  is1 = not is0
  st  = states p Map.! currentState p
  newtape | is0 && write0 st || is1 && write1 st = ISet.insert (pos p) (tape p)
          | otherwise                            = ISet.delete (pos p) (tape p)
  newpos | is0       = pos p + move0 st
         | otherwise = pos p + move1 st
  newstate | is0       = continue0 st
           | otherwise = continue1 st

main :: IO ()
main = do
  args <- getArgs
  s    <- case args of
    [path] -> readFile path
    _      -> getContents
  input <- case parse programParser "" s of
    Right yay -> pure yay
    Left  boo -> putStr (errorBundlePretty boo) >> exitFailure
  let endstate = iterate' step input !! remain input
  print $ ISet.size $ tape endstate

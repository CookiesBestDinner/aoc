{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict            as Map
import           Protolude
import           Text.Megaparsec
import Text.Show.Pretty
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data Register
  = A
  | B
  deriving (Eq, Show)

data Computer =
  Computer
    { a  :: Int
    , b  :: Int
    , pc :: Int
    } deriving (Show)

exec (Half A) comp = comp {a = a comp `div` 2}
exec (Half B) comp = comp {b = b comp `div` 2}
exec (Triple A) comp = comp {a = a comp * 3}
exec (Triple B) comp = comp {b = b comp * 3}
exec (Inc A) comp = comp {a = a comp + 1}
exec (Inc B) comp = comp {b = b comp + 1}
exec (Jump off) comp = comp {pc = pc comp + off - 1}
exec (JumpEven A off) comp =
  if even (a comp)
    then comp {pc = pc comp + off - 1}
    else comp
exec (JumpEven B off) comp =
  if even (b comp)
    then comp {pc = pc comp + off - 1}
    else comp
exec (JumpOne A off) comp =
  if (== 1) (a comp)
    then comp {pc = pc comp + off - 1}
    else comp
exec (JumpOne B off) comp =
  if (== 1) (b comp)
    then comp {pc = pc comp + off - 1}
    else comp

-- runCode instructions = go $ Computer 1 0 0  -- part 2
runCode instructions = go $ Computer 0 0 0
  where
    code = Map.fromList $ zip [0 ..] instructions
    go comp =
      let i = Map.lookup (pc comp) code
          nextState = exec <$> i <*> pure comp
       in case nextState of
            Just s  -> (comp, i) : (go $ s {pc = pc s + 1})
            Nothing -> [(comp, i)]

data Instruction
  = Half Register
  | Triple Register
  | Inc Register
  | Jump Int
  | JumpEven Register Int
  | JumpOne Register Int
  deriving (Eq, Show)

pInstructions :: Parser [Instruction]
pInstructions = (singleIns `sepEndBy` newline) <* eof
  where
    singleIns = choice [half, triple, inc, jump, jie, jio]
    pRegName = ("a" >> return A) <|> ("b" >> return B)
    signedDecimal = signed ("" >> return ()) decimal
    half = do
      "hlf "
      r <- pRegName
      return $ Half r
    triple = do
      "tpl "
      r <- pRegName
      return $ Triple r
    inc = do
      "inc "
      r <- pRegName
      return $ Inc r
    jump = do
      "jmp "
      offset <- signedDecimal
      return $ Jump offset
    jie = do
      "jie "
      r <- pRegName
      offset <- ", " >> signedDecimal
      return $ JumpEven r offset
    jio = do
      "jio "
      r <- pRegName
      offset <- ", " >> signedDecimal
      return $ JumpOne r offset

main = do
  input <- getContents
  instructions <-
    case parse pInstructions "" input of
      Right instr -> return instr
      Left err    -> putStrLn (errorBundlePretty err) >> exitFailure
  print instructions
  pPrint $ runCode instructions

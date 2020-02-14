{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import qualified Data.Map.Strict            as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Computer =
  Computer
    { a  :: Value
    , b  :: Value
    , c  :: Value
    , d  :: Value
    , pc :: Value
    }
  deriving (Show)

data Instruction
  = Cpy Var Register
  | Inc Register
  | Dec Register
  | Jnz Var Var
  deriving (Show)

data Var
  = Reg Register
  | Val Value
  deriving (Show)

data Register
  = A
  | B
  | C
  | D
  deriving (Show)

type Value = Integer

type Parser = Parsec Void Text

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ do "cpy "
         var <- pVar
         reg <- " " >> pReg
         return $ Cpy var reg
    , do "jnz "
         v1 <- pVar
         v2 <- " " >> pVar
         return $ Jnz v1 v2
    , do "inc "
         reg <- pReg
         return $ Inc reg
    , do "dec "
         reg <- pReg
         return $ Dec reg
    ]

pVar :: Parser Var
pVar = (Val <$> pVal) <|> (Reg <$> pReg)

pVal :: Parser Integer
pVal = signed (() <$ "") decimal

pReg :: Parser Register
pReg = ("a" $> A <|> "b" $> B <|> "c" $> C <|> "d" $> D)

decode :: Computer -> Var -> Value
decode _ (Val v) = v
decode comp (Reg r) =
  case r of
    A -> a comp
    B -> b comp
    C -> c comp
    D -> d comp

exec :: Computer -> Instruction -> Computer
exec comp (Cpy v A) = comp {a = (decode comp v)}
exec comp (Cpy v B) = comp {b = (decode comp v)}
exec comp (Cpy v C) = comp {c = (decode comp v)}
exec comp (Cpy v D) = comp {d = (decode comp v)}
exec comp (Inc A) = comp {a = a comp + 1}
exec comp (Inc B) = comp {b = b comp + 1}
exec comp (Inc C) = comp {c = c comp + 1}
exec comp (Inc D) = comp {d = d comp + 1}
exec comp (Dec A) = comp {a = a comp - 1}
exec comp (Dec B) = comp {b = b comp - 1}
exec comp (Dec C) = comp {c = c comp - 1}
exec comp (Dec D) = comp {d = d comp - 1}
exec comp (Jnz cond dist)
  | (decode comp cond) == 0 = comp
  | otherwise = comp {pc = pc comp + (decode comp dist) - 1}

incrementPC comp = comp {pc = pc comp + 1}

step instructions comp = do
  instr <- fromMaybe (Left comp) (Right <$> Map.lookup (pc comp) instructions)
  Right $ incrementPC $ exec comp instr

-- runComputer :: Computer -> Map Value Instruction -> Computer
runComputer comp insMap =
  case step insMap comp of
    Left done        -> comp
    Right unfinished -> runComputer unfinished insMap

main = do
  input <- getContents
  instructions <-
    case parse (pInstruction `sepEndBy` newline <* eof) "" input of
      Right res -> return res
      Left err  -> putStrLn (errorBundlePretty err) >> exitFailure
  print instructions
  let begind1 = Computer 0 0 0 0 0
  let begind2 = Computer 0 0 1 0 0
  let insMap = Map.fromList $ zip [0 ..] instructions
  print $ runComputer begind1 insMap
  print $ runComputer begind2 insMap
  return ()

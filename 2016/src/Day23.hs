{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import           Control.Monad.Extra        (loop)
import qualified Data.Map.Strict            as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Computer
  = Computer { a     :: !Value
             , b     :: !Value
             , c     :: !Value
             , d     :: !Value
             , pc    :: !Value
             , instr :: !(Map Integer Instruction)
             }
  deriving (Show)

data Instruction
  = Cpy Var Var
  | Inc Var
  | Dec Var
  | Jnz Var Var
  | Tgl Var
  deriving (Show)

data Var
  = Reg Register
  | Val Value
  deriving (Show)

data Register = A | B | C | D deriving (Show)

type Value = Integer

type Parser = Parsec Void Text

pInstruction :: Parser Instruction
pInstruction = choice
  [ "cpy " >> Cpy <$> pVar <* " " <*> pVar
  , "jnz " >> Jnz <$> pVar <* " " <*> pVar
  , "inc " >> Inc <$> pVar
  , "dec " >> Dec <$> pVar
  , "tgl " >> Tgl <$> pVar
  ]

pVar :: Parser Var
pVar = (Val <$> pVal) <|> (Reg <$> pReg)

pVal :: Parser Integer
pVal = signed (pure ()) decimal

pReg :: Parser Register
pReg = "a" $> A <|> "b" $> B <|> "c" $> C <|> "d" $> D

decode :: Computer -> Var -> Value
decode _    (Val v) = v
decode comp (Reg r) = case r of
  A -> a comp
  B -> b comp
  C -> c comp
  D -> d comp

incrementPC :: Computer -> Computer
incrementPC comp = comp { pc = pc comp + 1 }

step :: Computer -> Either Computer Computer
step comp = case instr comp Map.!? pc comp of
  Nothing -> Right comp
  Just i  -> Left $ incrementPC $ case i of
    (Cpy v (Reg A)) -> comp { a = decode comp v }
    (Cpy v (Reg B)) -> comp { b = decode comp v }
    (Cpy v (Reg C)) -> comp { c = decode comp v }
    (Cpy v (Reg D)) -> comp { d = decode comp v }
    (Inc (Reg A)  ) -> comp { a = a comp + 1 }
    (Inc (Reg B)  ) -> comp { b = b comp + 1 }
    (Inc (Reg C)  ) -> comp { c = c comp + 1 }
    (Inc (Reg D)  ) -> comp { d = d comp + 1 }
    (Dec (Reg A)  ) -> comp { a = a comp - 1 }
    (Dec (Reg B)  ) -> comp { b = b comp - 1 }
    (Dec (Reg C)  ) -> comp { c = c comp - 1 }
    (Dec (Reg D)  ) -> comp { d = d comp - 1 }
    (Tgl v        ) -> toggle (decode comp v) comp
    (Jnz cond dist) -> if decode comp cond == 0
      then comp
      else comp { pc = pc comp + decode comp dist - 1 }
    _ -> comp  -- invalid instructions are skipped

toggle :: Value -> Computer -> Computer
toggle offset comp = case instr comp Map.!? loc of
  Nothing  -> comp
  Just old -> comp { instr = Map.insert loc newInstr (instr comp) }
   where
    newInstr = case old of
      Inc v   -> Dec v
      Dec v   -> Inc v
      Tgl v   -> Inc v
      Jnz v w -> Cpy v w
      Cpy v w -> Jnz v w
  where loc = pc comp + offset

main :: IO ()
main = do
  input <- getContents
  i     <- case parse (pInstruction `sepEndBy` newline <* eof) "" input of
    Right res -> return res
    Left  err -> putStrLn (errorBundlePretty err) >> exitFailure
  putText "part1:"
  print $ loop step $ Computer 7 0 0 0 0 $ Map.fromList $ zip [0 ..] i
  putText "expecting 3-5 minutes for part 2"
  print $ loop step $ Computer 12 0 0 0 0 $ Map.fromList $ zip [0 ..] i

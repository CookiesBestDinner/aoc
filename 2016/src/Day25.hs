{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day25 where

import qualified Data.Map.Strict            as Map
import           Protolude                  hiding (infinity)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data Computer
  = Computer { a      :: !Value
             , b      :: !Value
             , c      :: !Value
             , d      :: !Value
             , pc     :: !Value
             , signal :: !(Maybe Value)
             , instr  :: !(Map Integer Instruction)
             }
  deriving (Show)

data Instruction
  = Cpy Var Var
  | Inc Var
  | Dec Var
  | Jnz Var Var
  | Tgl Var
  | Out Var
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
  , "out " >> Out <$> pVar
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
    (Out v        ) -> comp { signal = Just (decode comp v) }
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
      Out v   -> Inc v
      Jnz v w -> Cpy v w
      Cpy v w -> Jnz v w
  where loc = pc comp + offset

run :: Computer -> [Integer]
run comp = case signal comp of
  Just s  -> s : run comp { signal = Nothing }
  Nothing -> case step comp of
    Right _     -> []
    Left  comp' -> run comp'


main :: IO ()
main = do
  input <- getContents
  i     <- case parse (pInstruction `sepEndBy` newline <* eof) "" input of
    Right res -> return res
    Left  err -> putStrLn (errorBundlePretty err) >> exitFailure
  putText "part1:"
  let instructions = Map.fromList $ zip [0 ..] i
  -- ... how big is infinity? 1000 is pretty big.
  let infinity     = 1000
  let correct = cycle [0 :: Integer, 1] & take infinity
  forM_ [0 ..] $ \n -> do
    let sig = run $ Computer n 0 0 0 0 Nothing instructions
    when (n `mod` 1000 == 0) (print n)
    when (take infinity sig == correct) $ do
      print n
      putText "---"
      -- comment out exitSuccess to show that the next one is really far away
      -- meaning, it's unreasonable that it's one of them (next is 8384 for me)
      exitSuccess

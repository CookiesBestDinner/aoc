{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Day23 where

import           Control.Arrow
import           Control.Lens                      hiding ( Strict )
import qualified Data.Map                      as Map
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex

type Code = [Instr]
data Reg = Literal Int | Variable Char deriving (Eq, Ord, Show)
data Instr
  = Set Reg Reg
  | Sub Reg Reg
  | Mul Reg Reg
  | Jnz Reg Reg
  deriving Show

data VM = VM
  { _mulCount  :: Int
  , _registers :: Map.Map Reg Int
  , _ptr       :: Int
  , _code      :: Code
  } deriving Show

makeLenses ''VM

literal :: Parsec Void Text Reg
literal = Literal <$> Lex.signed (pure ()) Lex.decimal

pInstr :: Parsec Void Text Instr
pInstr = do
  operation <- "set" $> Set <|> "sub" $> Sub <|> "mul" $> Mul <|> "jnz" $> Jnz
  " "
  a <- (oneOf ['a' .. 'h'] <&> Variable) <|> literal
  " "
  b <- (oneOf ['a' .. 'h'] <&> Variable) <|> literal
  pure $ operation a b

readInstr :: Instr -> VM -> VM
readInstr instr vm = incrPtr $ operation vm
 where
  operation = case instr of
    Set x y -> write x (read y)
    Sub x y -> update x (subtract $ read y)
    Mul x y -> update x (* read y) >>> mulCount %~ (+ 1)
    Jnz x y ->
      if read x /= 0 then ptr %~ ((+ read y) . subtract 1) else identity
  read (Literal x) = x
  read x           = fromMaybe 0 $ vm ^? registers . ix x
  write k@(Variable _) x = registers . at k ?~ x
  write _              _ = panic "incomplete functions are bad m'kay."
  update k f = write k (f $ read k)
  incrPtr = ptr %~ (+ 1)

run :: VM -> VM
run vm
  | vm ^. ptr >= length (vm ^. code)
  = vm
  | otherwise
  = let step :: VM -> VM = readInstr (vm ^?! code . ix (vm ^. ptr))
    in  run $ step vm

main :: IO ()
main = do
  input <- getArgs >>= \case
    [filepath] -> readFile filepath
    _          -> getContents
  instructions <- case parse (pInstr `sepEndBy` space) "" input of
    Left  boo -> die $ strConv Strict $ errorBundlePretty boo
    Right joy -> pure joy
  let vm = VM { _mulCount  = 0
              , _registers = Map.empty
              , _ptr       = 0
              , _code      = instructions
              }
      end = run vm
  print $ end ^. mulCount

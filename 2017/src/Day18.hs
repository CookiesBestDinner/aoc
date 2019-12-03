{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day18 where

import           Control.Lens                      hiding ( op )
import qualified Data.Map.Strict               as Map
import           Prelude                                  ( error )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Register = Map.Map Char Integer

data Val
  = Val Integer
  | Reg Char
  deriving (Show)

data Op
  = Snd Val
  | Set Val Val
  | Add Val Val
  | Mul Val Val
  | Mod Val Val
  | Rcv Val
  | Jgz Val Val
  deriving (Show)

data Computer =
  Computer
    { _pc       :: Integer
    , _register :: Register
    , _program  :: Map.Map Integer Op
    , _output   :: [Integer]
    }
  deriving (Show)

makeLenses ''Computer

getOps :: IO [Op]
getOps = do
  input <- getContents
  case parse (pOp `sepEndBy1` newline <* eof) "" input of
    Left  boo -> putStrLn (errorBundlePretty boo) >> exitFailure
    Right joy -> pure joy

type Parser = Parsec Void Text

pOp :: Parser Op
pOp = choice
  [ Snd <$ "snd " <*> pVal
  , Set <$ "set " <*> pReg <* " " <*> pVal
  , Add <$ "add " <*> pReg <* " " <*> pVal
  , Mul <$ "mul " <*> pReg <* " " <*> pVal
  , Mod <$ "mod " <*> pReg <* " " <*> pVal
  , Rcv <$ "rcv " <*> pVal
  , Jgz <$ "jgz " <*> pVal <* " " <*> pVal
  ]
 where
  pReg = Reg <$> oneOf ['a' .. 'z']
  pNum = Val <$> signed ("" $> ()) decimal
  pVal :: Parser Val
  pVal = pReg <|> pNum

readVal :: Register -> Val -> Integer
readVal _   (Val n) = n
readVal reg (Reg r) = reg Map.!? r & fromMaybe 0

doComputerStep :: Computer -> Either Computer Computer
doComputerStep comp = case op' of
  Nothing -> Left comp
  Just _  -> comp & action op <&> pc %~ (+ 1)
 where
  r       = readVal (comp ^. register)
  op'     = view program comp Map.!? view pc comp
  Just op = op'
  action :: Op -> Computer -> Either Computer Computer
  action (Snd x        ) = Right . (output %~ (r x :))
  action (Set (Reg a) b) = Right . (register . at a ?~ r b)
  action (Add (Reg a) b) = Right . (register . at a %~ map (+ r b))
  action (Mul (Reg a) b) = Right . (register . at a %~ map (* r b))
  action (Mod (Reg a) b) = Right . (register . at a %~ map (`mod` r b))
  action (Rcv x        ) = if r x == 0
    then Right . identity
    else Left . (output %~ (comp ^?! (output . ix 0) :))
  action (Jgz x y) = Right . if r x <= 0 then identity else pc %~ (+ (r y - 1))
  action other     = error $ "bad instruction: " <> show other

choochoo :: Computer -> Computer
choochoo comp = case doComputerStep comp of
  Left  boo -> boo
  Right yay -> choochoo yay

main :: IO ()
main = do
  input <- getOps
  let instructions = Map.fromList $ zip [0 ..] input
  let begin        = Computer 0 Map.empty instructions []
  print $ choochoo begin

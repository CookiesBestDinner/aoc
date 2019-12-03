{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day18 where

import           Control.Lens                      hiding ( op )
import qualified Data.Map.Strict               as Map
import           Control.Arrow
import           Prelude                                  ( error
                                                          , last
                                                          , init
                                                          )
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Register = Map.Map Char Integer

data Val
  = Val Integer
  | Reg Char
  deriving (Eq, Show)

data Op
  = Snd Val
  | Set Val Val
  | Add Val Val
  | Mul Val Val
  | Mod Val Val
  | Rcv Val
  | Jgz Val Val
  deriving (Eq, Show)

data Status
  = Running | Receiving Char deriving (Eq, Show)

data Computer =
  Computer
    { _pc       :: Integer
    , _register :: Register
    , _program  :: Map.Map Integer Op
    , _output   :: [Integer]
    , _status :: Status
    , _sendCount :: Int
    }
  deriving (Eq, Show)

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

doComputerStep2 :: Computer -> Either Computer Computer
doComputerStep2 comp
  | comp ^. status /= Running = Left comp
  | otherwise = case op' of
    Nothing -> Left comp
    Just _  -> comp & action op <&> pc %~ (+ 1)
 where
  r       = readVal (comp ^. register)
  op'     = view program comp Map.!? view pc comp
  Just op = op'
  action :: Op -> Computer -> Either Computer Computer
  action (Snd x        ) = Right . ((output %~ (r x :)) >>> sendCount %~ (+ 1))
  action (Set (Reg a) b) = Right . (register . at a ?~ r b)
  action (Add (Reg a) b) = Right . (register . at a %~ map (+ r b))
  action (Mul (Reg a) b) = Right . (register . at a %~ map (* r b))
  action (Mod (Reg a) b) = Right . (register . at a %~ map (`mod` r b))
  action (Rcv (Reg x)) | r (Reg x) == 0 = Right . identity
                       | otherwise      = Left . (status .~ Receiving x)
  action (Jgz x y) = Right . if r x <= 0 then identity else pc %~ (+ (r y - 1))
  action other     = error $ "bad instruction: " <> show other

choochoo :: Computer -> Computer
choochoo comp = case doComputerStep comp of
  Left  boo -> boo
  Right yay -> choochoo yay

choochoos :: (Computer, Computer) -> (Computer, Computer)
choochoos c@(a, b) =
  let a'         = choochoo a
      b'         = choochoo b
      (a'', b'') = communicateab a' b
  in  if (a', b') == (a'', b'') then (a', b') else choochoos (a'', b'')


communicateab a b
  | null $ b ^. output = communicateba a b
  | otherwise = communicateba (a & putMsg & putStatusRunning) (b & popMsg)
 where
  msg                   = last (b ^. output)
  (Receiving recipient) = a ^. status
  popMsg                = output %~ init
  putMsg                = (register . at recipient) ?~ msg
  putStatusRunning      = status .~ Running

communicateba a b | null $ a ^. output = (a, b)
                  | otherwise = (a & popMsg, b & putMsg & putStatusRunning)
 where
  msg                   = last (a ^. output)
  (Receiving recipient) = b ^. status
  popMsg                = output %~ init
  putMsg                = (register . at recipient) ?~ msg
  putStatusRunning      = status .~ Running

main :: IO ()
main = do
  input <- getOps
  let instructions = Map.fromList $ zip [0 ..] input
  let begin        = Computer 0 Map.empty instructions [] Running 0
  print $ choochoo begin
  let patchP id = (register . at 'p') ?~ id
  putText "-------------------"
  choochoos (begin & patchP 0, begin & patchP 1) & print

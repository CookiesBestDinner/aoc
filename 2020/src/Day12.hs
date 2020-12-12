{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Day12 where

import           Control.Lens
import           Data.List                  (iterate', (!!))
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

data Comp -- It's not a boat, it's a VM.
  = Comp
      { _posx  :: Int
      , _posy  :: Int
      , _shipx :: Int
      , _shipy :: Int
      }
  deriving (Show)

makeLenses ''Comp

data ExitReason = Terminates | Loops deriving (Show)

pIns :: Parser [(Char, Int)]
pIns =
  let ln = (,) <$> oneOf ("NSEWLRF" :: [Char]) <*> decimal
  in  (ln `sepEndBy1` eol) <* eof

step
  :: ASetter Comp Comp Int Int
  -> ASetter Comp Comp Int Int
  -> Comp
  -> (Char, Int)
  -> Comp
step mvx mvy comp (c, n) = case c of
  'N' -> comp & mvy %~ (+ n)
  'S' -> comp & mvy %~ subtract n
  'E' -> comp & mvx %~ (+ n)
  'W' -> comp & mvx %~ subtract n
  'R' -> if n == 0
    then comp
    else step mvx
              mvy
              (comp & (posx .~ comp ^. posy) . (posy .~ comp ^. posx * (-1)))
              (c, n - 90)
  'L' -> iterate' (\comp' -> step mvx mvy comp' ('R', n)) comp !! 3
  'F' ->
    comp & (shipx %~ (+ n * (comp ^. posx))) . (shipy %~ (+ n * (comp ^. posy)))
  _ -> panic $ "invalid command" <> show c

manhattan :: Comp -> Int
manhattan comp = abs (comp ^. shipy) + abs (comp ^. shipx)

main :: Text -> IO ()
main input = do
  instr <- parse' pIns input
  print $ foldl' (step shipx shipy) (Comp 1 0 0 0) instr & manhattan
  print $ foldl' (step posx posy) (Comp 10 1 0 0) instr & manhattan

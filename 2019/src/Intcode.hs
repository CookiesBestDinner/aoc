{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Intcode
  ( Comp(..)
  , mem
  , param
  , parseComp
  , pc
  , run
  , run'
  )
where

import           Common

import           Conduit
import           Control.Arrow
import           Control.Lens
import qualified Data.Map.Strict               as Map
import           Protolude               hiding ( zero )
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( space )
import           Text.Megaparsec.Char.Lexer     ( decimal
                                                , signed
                                                )

data Comp =
  Comp
    { _pc    :: !Int
    , _param :: !Int
    , _mem   :: !(Map.Map Int Int)
    }
  deriving (Show, Eq, Ord)

makeLenses ''Comp

parseComp :: Parser Comp
parseComp = do
  ops <- signed (pure ()) decimal `sepBy1` "," <* space
  return $ Comp 0 0 $ Map.fromList (zip [0 ..] ops)

decodeOp :: Int -> (Int, Int, Int, Int)
decodeOp i =
  ( i `mod` 100
  , (i `div` 100) `mod` 10
  , (i `div` 1000) `mod` 10
  , (i `div` 10000) `mod` 10
  )

run :: Monad m => Comp -> ConduitT Int Int m ()
run comp = do
  let lookup loc = fromMaybe 0 (comp ^? mem . ix loc)
      var o m = lookup (comp ^. pc + o) + if m == 2 then comp ^. param else 0
      (i, ma, mb, mc) = decodeOp (lookup (comp ^. pc))
      (a, b, c)       = (var 1 ma, var 2 mb, var 3 mc)
      ra              = if ma == 1 then a else lookup a
      rb              = if mb == 1 then b else lookup b
  readInput <- if i == 3 then await else pure Nothing
  when (i == 4) $ yield ra
  when (i /= 99) $ run $ comp & case i of
    1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
    2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
    3 -> pc %~ (+ 2) >>> mem . at a .~ readInput
    4 -> pc %~ (+ 2)
    5 -> pc %~ if ra /= 0 then const rb else (+ 3)
    6 -> pc %~ if ra == 0 then const rb else (+ 3)
    7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
    8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
    9 -> pc %~ (+ 2) >>> param %~ (+ ra)
    _ -> panic "heeeeelp"

run' :: Monad m => Comp -> ConduitT Int (Int, Comp) m ()
run' comp = do
  let lookup loc = fromMaybe 0 (comp ^? mem . ix loc)
      var o m = lookup (comp ^. pc + o) + if m == 2 then comp ^. param else 0
      (i, ma, mb, mc) = decodeOp (lookup (comp ^. pc))
      (a, b, c)       = (var 1 ma, var 2 mb, var 3 mc)
      ra              = if ma == 1 then a else lookup a
      rb              = if mb == 1 then b else lookup b
  readInput <- if i == 3 then await else pure Nothing
  let next = comp & case i of
          1 -> pc %~ (+ 4) >>> mem . at c ?~ ra + rb
          2 -> pc %~ (+ 4) >>> mem . at c ?~ ra * rb
          3 -> pc %~ (+ 2) >>> mem . at a .~ readInput
          4 -> pc %~ (+ 2)
          5 -> pc %~ if ra /= 0 then const rb else (+ 3)
          6 -> pc %~ if ra == 0 then const rb else (+ 3)
          7 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra < rb)
          8 -> pc %~ (+ 4) >>> mem . at c ?~ fromEnum (ra == rb)
          9 -> pc %~ (+ 2) >>> param %~ (+ ra)
          _ -> panic "heeeeelp"
  when (i == 4) $ yield (ra, next)
  when (i /= 99) $ run' next

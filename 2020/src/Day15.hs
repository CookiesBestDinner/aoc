{-# LANGUAGE FlexibleContexts #-}
module Day15 where

import           Data.Array.ST
import           Data.STRef
import           Protolude
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

import           Parsing

speak :: STUArray s Int Int -> Int -> Int -> ST s Int
speak mem tnow n = do
  recall <- readArray mem n
  writeArray mem n (tnow - 1)
  pure $ case recall of
    -1 -> 0
    t  -> tnow - t - 1

run :: Int -> [Int] -> Int
run steps m = runST $ do
  let init = replicate steps (-1)
  mem  <- newListArray (0, steps) init :: ST s (STUArray s Int Int)
  last <- newSTRef (lastDef (panic "empty initial values (the AoC input)") m)
  forM_ (zip [1 ..] m) $ \(t, x) -> do
    writeArray mem x t
  forM_ [length m + 1 .. steps] $ \t -> do
    writeSTRef last =<< (speak mem t =<< readSTRef last)
  readSTRef last

main :: Text -> IO ()
main input = do
  indata <- parse' (decimal `sepBy` "," <* space <* eof) input
  print indata
  print $ run 2020 indata
  print $ run 30_000_000 indata

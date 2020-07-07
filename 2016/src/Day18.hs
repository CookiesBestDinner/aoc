{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day18 where

import qualified Data.Text           as Text
import qualified Data.Vector.Unboxed as V
import           Protolude

main :: IO ()
main = do
  ln1 <- Text.strip <$> readFile "inputs/18"
  let rows = 400000  -- part 1 is 40
      room = V.fromList $ do
        t <- toS ln1
        pure $ t == '^'
      width = Text.length ln1
  expand width 1 rows room 0 & ((width * rows) -) & print

trapPatterns :: [[Bool]]
trapPatterns =
  [ [True, True, False]
  , [False, True, True]
  , [True, False, False]
  , [False, False, True]
  ]

expand :: (Eq t, Num t) => Int -> t -> t -> V.Vector Bool -> Int -> Int
expand w r h room acc
  | r == h    = acc + V.sum (fromEnum `V.map` room)
  | otherwise = expand w (r + 1) h here (acc + V.sum (fromEnum `V.map` room)) where
  here = V.fromList $ do
    x <- [0 .. w - 1]
    let parentKeys = [x - 1, x, x + 1]
        parents    = (room V.!?) <$> parentKeys <&> \case
          Nothing -> False
          Just b  -> b
    pure $ parents `elem` trapPatterns

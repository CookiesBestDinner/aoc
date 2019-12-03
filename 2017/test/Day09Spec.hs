{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day09Spec where

import           Control.Monad
import           Day09.Main    (Result (..), solve)
import           Protolude
import           Test.Hspec
import qualified Data.Text     as T

examples1 :: [(Text, Int)]
examples1 =
  [ ("{}", 1)
  , ("{{{}}}", 6)
  , ("{{},{}}", 5)
  , ("{{{},{},{{}}}}", 16)
  , ("{<a>,<a>,<a>,<a>}", 1)
  , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
  , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
  , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
  ]

examples2 :: [(Text, Int)]
examples2 =
  [ ("<>", 0)
  , ("<random characters>", 17)
  , ("<<<<>", 3)
  , ("<{!>}>", 2)
  , ("<!!>", 0)
  , ("<!!!>>", 0)
  , ("<{o\"i!a,<{i<a>", 10)
  ]

spec :: Spec
spec = do
  describe "part1 examples" $ do
    forM_ examples1 $ \(input, expect) -> do
      it (T.unpack input) $ do
        (solve input <&> groupScore) `shouldBe` (Right expect)
  describe "part2 examples" $ do
    forM_ examples2 $ \(input, expect) -> do
      it (T.unpack input) $ do
        (solve input <&> garbageCount) `shouldBe` (Right expect)
  input <- runIO $ readFile "input/day09"
  describe "full input" $ do
    it "solves part1 and part2" $ do
      solve input `shouldBe` (Right $ Result 20530 9978)

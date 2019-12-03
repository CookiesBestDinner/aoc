{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Day10Spec where

import           Protolude
import           Day10                                    ( solve
                                                          , solve2
                                                          )
import           Test.Hspec
import qualified Data.Text                     as T

input :: Text
input = "46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"

p2Examples :: [(Text, Text)]
p2Examples =
  [ (""        , "a2582a3a0e66e6e86e3812dcb672a272")
  , ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd")
  , ("1,2,3"   , "3efbe78a8d82f29979031a4aa0b16a9d")
  , ("1,2,4"   , "63960835bcdc130f0b66d7ff4f6a5a8e")
  ]

spec = do
  describe "part1" $ do
    let example = "3,4,1,5"
    it ("example: " <> T.unpack example) $ do
      solve example [0 .. 4] `shouldBe` 12
    it ("puzzle input: " <> T.unpack input) $ do
      solve input [0 .. 255] `shouldBe` 52070
  describe "part2" $ do
    forM_ p2Examples $ \(input, output) -> do
      it (T.unpack $ mconcat [show input, " -> ", output]) $ do
        solve2 input [0 .. 255] `shouldBe` output
    it ("puzzle input: " <> T.unpack input) $ do
      solve2 input [0 .. 255] `shouldBe` "7f94112db4e32e19cf6502073c66f9bb"

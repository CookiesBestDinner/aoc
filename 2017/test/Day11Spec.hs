{-# LANGUAGE NoImplicitPrelude #-}

module Day11Spec where

import qualified Data.Text                     as T
import           Protolude
import           Test.Hspec

import           Day11

p1Examples =
  [("ne,ne,ne", 3), ("ne,ne,sw,sw", 0), ("ne,ne,s,s", 2), ("se,sw,se,sw,sw", 3)]

spec = do
  describe "part1 examples" $ do
    forM_ p1Examples $ \(input, output) -> do
      it input $ do
        part1 (T.pack input) `shouldBe` (Right output)
  biginput <- runIO $ readFile "data/input/day11"
  it "input" $ do
    part1 biginput `shouldBe` Right 743
  it "part2" $ do
    part2 biginput `shouldBe` Right 1493

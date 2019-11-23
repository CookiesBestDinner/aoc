{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day12Spec where

import           Day12
import           Protolude
import           Test.Hspec
import           Text.InterpolatedString.QM

ex :: Text
ex = [qnb|
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
|]

spec = do
  it "part1 example" $ do
    let res = part1 ex
    res `shouldBe` Right 6
  input <- runIO $ readFile "data/input/day12"
  it "input part 1" $ do
    part1 input `shouldBe` Right 288
  it "input part 2" $ do
    part2 input `shouldBe` Right 211

{-# LANGUAGE NoImplicitPrelude #-}
module Day13Spec where

import Protolude
import Test.Hspec
import Day13 (getCycle)
import Data.Map.Strict as Map

spec = do
  getCycleSpec

getCycleSpec = do
  let tests =
        [ (0, [])
        , (1, [0])
        , (2, [0, 1])
        , (3, [0, 1, 2, 1])
        , (4, [0, 1, 2, 3, 2, 1])
        ]
  describe "getCycle" $ do
    forM_ tests $ \(input, output) ->
      it (show input <> " -> " <> show output) $ do
        getCycle input `shouldBe` output
